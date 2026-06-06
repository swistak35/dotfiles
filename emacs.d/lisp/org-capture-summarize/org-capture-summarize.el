;;; org-capture-summarize.el --- Summarize a referenced FILE inside an org-capture entry  -*- lexical-binding: t; -*-

;; Summarize the file pointed to by the SOURCE_FILE property of the
;; current org-capture entry using an LLM (via gptel + a local Ollama model).
;; The generated summary is inserted into the entry body and the model
;; also suggests tags, biased towards the tags already used in the
;; capture target file.

;;; Code:

(require 'org)
(require 'org-capture)
(require 'gptel)
(require 'json)
(require 'seq)
(require 'dom)

(defgroup org-capture-summarize nil
  "Summarize files referenced from org-capture entries using an LLM."
  :group 'org-capture)

(defcustom org-capture-summarize-backend-name "Ollama"
  "Name of the gptel backend to use for summarization.
This must match the name passed to `gptel-make-ollama' (or another
`gptel-make-*' constructor).  When the backend cannot be found the
current `gptel-backend' is used instead."
  :type 'string
  :group 'org-capture-summarize)

(defcustom org-capture-summarize-model nil
  "gptel model to use for summarization.
When nil the model configured on the chosen backend (or the current
`gptel-model') is used."
  :type '(choice (const :tag "Backend default" nil) symbol)
  :group 'org-capture-summarize)

(defcustom org-capture-summarize-max-chars 16000
  "Maximum number of characters of the file sent to the LLM."
  :type 'integer
  :group 'org-capture-summarize)

(defcustom org-capture-summarize-system-message
  "You are a helpful assistant that summarizes web pages for a personal \
note-taking system based on Emacs org-mode. The text you are given is the \
extracted textual content of an HTML web page. Summarize what the page is \
actually about -- its substance, arguments, or information -- as if briefing \
someone who has not read it. Do NOT describe the page itself (do not say \
things like \"this is a web page\", \"this article discusses\", \"the document \
contains\"); just convey the content directly. Ignore navigation menus, \
cookie notices, ads, and other boilerplate. Write a concise, factual summary \
and propose a short list of relevant tags."
  "System message sent to the LLM when summarizing a file."
  :type 'string
  :group 'org-capture-summarize)

(defconst org-capture-summarize--schema
  '(:type "object"
    :properties
    (:summary
     (:type "string"
      :description "A concise summary of the web page's content, 2 to 5 sentences, written in plain prose. Convey the actual subject matter directly; do not describe the page or mention that it is a web page.")
     :tags
     (:type "array"
      :description "A short list (at most 6) of relevant org-mode tags for the document. Strongly prefer reusing tags from the list of existing tags when they fit. Tags must be lowercase and use underscores instead of spaces."
      :items (:type "string"))))
  "JSON schema describing the structured response expected from the LLM.")

;;;; Reading information out of the capture buffer

(defun org-capture-summarize--link-to-path (value)
  "Return a filesystem path from VALUE.
VALUE may be a bare path or an org-mode link such as
\"[[~/dir/file.html]]\" or \"[[file:~/dir/file.html][desc]]\".  Any
\"file:\" link type prefix and \"::search\" suffix are stripped."
  (let ((v (string-trim value)))
    (when (string-match "\\`\\[\\[\\(.*?\\)\\]\\(?:\\[.*?\\]\\)?\\]\\'" v)
      (setq v (match-string 1 v)))
    (setq v (replace-regexp-in-string "\\`file:" "" v))
    (setq v (replace-regexp-in-string "::.*\\'" "" v))
    v))

(defun org-capture-summarize--entry-file ()
  "Return the file path from the SOURCE_FILE property of the current entry.
The property value may be a bare path or an org-mode link; in both
cases the underlying filesystem path is returned.  Returns nil when the
property is absent."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward org-heading-regexp nil t)
      (let ((value (org-entry-get (point) "SOURCE_FILE")))
        (when value
          (org-capture-summarize--link-to-path value))))))

(defun org-capture-summarize--target-file ()
  "Return the path of the file targeted by the current capture template."
  (let ((target (org-capture-get :target)))
    (pcase target
      (`(file ,path) path)
      (`(file+headline ,path . ,_) path)
      (`(file+olp ,path . ,_) path)
      (`(file+olp+datetree ,path . ,_) path)
      (`(file+regexp ,path . ,_) path)
      (`(file+function ,path . ,_) path)
      (_ nil))))

(defun org-capture-summarize--existing-tags ()
  "Return the list of tags already used in the capture target file."
  (let ((file (org-capture-summarize--target-file)))
    (when (and file (file-exists-p file))
      (delete-dups
       (mapcar #'car (with-current-buffer (find-file-noselect file)
                       (org-get-buffer-tags)))))))

(defun org-capture-summarize--html-p (path raw)
  "Return non-nil when PATH/RAW looks like an HTML document."
  (or (string-match-p "\\.x?html?\\'" path)
      (string-match-p "<!doctype html\\|<html[ >]"
                      (downcase (substring raw 0 (min 1000 (length raw)))))))

(defun org-capture-summarize--html-to-text (html)
  "Extract readable plain text from the HTML string HTML.
Strips script, style, head and other non-content nodes (so inlined
base64 assets in SingleFile pages are ignored) and collapses
whitespace.  Falls back to returning HTML unchanged when libxml is
unavailable."
  (if (not (fboundp 'libxml-parse-html-region))
      html
    (with-temp-buffer
      (insert html)
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (dolist (tag '(script style noscript head svg link meta))
          (dolist (node (dom-by-tag dom tag))
            (ignore-errors (dom-remove-node dom node))))
        (let ((text (dom-texts dom)))
          (string-trim
           (replace-regexp-in-string
            "\n[ \t]*\\(\n[ \t]*\\)+" "\n\n"
            (replace-regexp-in-string "[ \t]+" " " text))))))))

(defun org-capture-summarize--read-file (path)
  "Return readable text of file at PATH, truncated for the LLM.
HTML files are reduced to their textual content first; the result is
capped at `org-capture-summarize-max-chars' characters."
  (let* ((raw (with-temp-buffer
                (insert-file-contents path)
                (buffer-string)))
         (text (if (org-capture-summarize--html-p path raw)
                   (org-capture-summarize--html-to-text raw)
                 raw)))
    (substring text 0 (min (length text) org-capture-summarize-max-chars))))

;;;; Applying the LLM result back into the capture buffer

(defun org-capture-summarize--insert-summary (summary)
  "Insert SUMMARY into the body of the first heading in the current buffer."
  (when (and (stringp summary) (not (string-empty-p summary)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward org-heading-regexp nil t)
        (org-end-of-meta-data t)
        (insert "\n" (string-trim summary) "\n")))))

(defun org-capture-summarize--sanitize-tag (tag)
  "Normalize TAG to lowercase alphanumeric characters and underscores.
Any other character is replaced with an underscore and leading,
trailing and repeated underscores are collapsed.  Returns nil when
nothing usable remains."
  (let ((clean (replace-regexp-in-string
                "_+" "_"
                (replace-regexp-in-string
                 "[^a-z0-9_]" "_" (downcase tag)))))
    (setq clean (string-trim clean "_+" "_+"))
    (unless (string-empty-p clean) clean)))

(defun org-capture-summarize--apply-tags (tags)
  "Add TAGS (a list of strings) to the first heading in the current buffer.
Each tag is normalized to lowercase alphanumerics and underscores."
  (let ((tags (delq nil (mapcar #'org-capture-summarize--sanitize-tag tags))))
    (when (consp tags)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward org-heading-regexp nil t)
          (org-back-to-heading t)
          (org-set-tags
           (seq-uniq (append (org-get-tags nil t) tags) #'string-equal)))))))

(defun org-capture-summarize--handle-response (response info)
  "Callback for the gptel request: apply RESPONSE to the capture buffer.
INFO is the gptel info plist."
  (let ((buffer (plist-get info :buffer)))
    (cond
     ((not (stringp response))
      (message "org-capture-summarize: request failed: %s"
               (plist-get info :status)))
     ((not (buffer-live-p buffer))
      (message "org-capture-summarize: capture buffer is gone, discarding summary"))
     (t
      (condition-case err
          (let* ((data (json-parse-string response
                                          :object-type 'plist
                                          :array-type 'list))
                 (summary (plist-get data :summary))
                 (tags (plist-get data :tags)))
            (with-current-buffer buffer
              (org-capture-summarize--insert-summary summary)
              (org-capture-summarize--apply-tags tags))
            (message "org-capture-summarize: summary inserted (%d tag(s))"
                     (length tags)))
        (error
         (message "org-capture-summarize: could not parse response: %s"
                  (error-message-string err))))))))

;;;; Entry point

;;;###autoload
(defun org-capture-summarize-file ()
  "Summarize the file referenced by the current org-capture entry.

Reads the SOURCE_FILE property of the current entry, sends its contents to
the LLM (via gptel and the `org-capture-summarize-backend-name'
backend, normally a local Ollama model) and asks it for a summary and
a set of tags.  The list of tags already used in the capture target
file is passed along so the model reuses existing tags where possible.

The summary is inserted into the entry body and the suggested tags are
added to the entry heading."
  (interactive)
  (let ((file (org-capture-summarize--entry-file)))
    (unless file
      (user-error "No SOURCE_FILE property found in the current capture entry"))
    (setq file (expand-file-name file))
    (unless (file-readable-p file)
      (user-error "Cannot read file: %s" file))
    (let* ((content (org-capture-summarize--read-file file))
           (existing-tags (org-capture-summarize--existing-tags))
           (backend (or (ignore-errors
                          (gptel-get-backend org-capture-summarize-backend-name))
                        gptel-backend))
           (gptel-backend backend)
           (gptel-model (or org-capture-summarize-model gptel-model))
           (prompt (concat
                    "Below is the extracted text content of an HTML web page. "
                    "Summarize what the page is actually about (its content), "
                    "not what kind of page it is, and suggest tags.\n\n"
                    "Existing tags already in use (prefer these when relevant): "
                    (if existing-tags
                        (mapconcat #'identity existing-tags ", ")
                      "(none)")
                    "\n\n--- BEGIN WEB PAGE CONTENT ---\n"
                    content
                    "\n--- END WEB PAGE CONTENT ---")))
      (message "org-capture-summarize: summarizing %s..."
               (file-name-nondirectory file))
      (gptel-request prompt
        :system org-capture-summarize-system-message
        :schema org-capture-summarize--schema
        :callback #'org-capture-summarize--handle-response))))

;;;; org-capture integration

;;;###autoload
(defun org-capture-summarize-maybe ()
  "Offer to summarize the current capture entry if it has a SOURCE_FILE property.
Intended for use in `org-capture-mode-hook'."
  (when (org-capture-summarize--entry-file)
    (when (y-or-n-p "Summarize the referenced file with the LLM? ")
      (org-capture-summarize-file))))

(provide 'org-capture-summarize)

;;; org-capture-summarize.el ends here

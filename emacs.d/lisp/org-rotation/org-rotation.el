;;; org-rotation.el --- Rotate a repeating task through its child variants  -*- lexical-binding: t; -*-

;; One repeating task (SCHEDULED with a `.+N' repeater) whose direct
;; children describe the variants to cycle through: session A, session
;; B, session C, and so on.  The entry keeps a 1-based ROTATION_INDEX
;; pointing at the variant that is due now, and its headline is
;; rendered as ROTATION_PREFIX + separator + the current variant's
;; headline, so the agenda shows which variant is up:
;;
;;     ** TODO Brzuch — Sesja A (fundament)                :@exercises:
;;        SCHEDULED: <2026-09-22 wto .+2d>
;;        :PROPERTIES:
;;        :ROTATION_PREFIX: Brzuch
;;        :ROTATION_INDEX: 1
;;        :END:
;;     *** Sesja A (fundament)
;;     *** Sesja B (skośne i boki)
;;     *** Sesja C (progresja)
;;
;; Because the variants are headlines rather than a separator-delimited
;; property, their names can contain arbitrary punctuation, and each one
;; owns the body text and the log of when it was last done.
;;
;; Whenever the repeater fires the index advances by one (wrapping
;; around), the finished variant gets a line in its own log drawer, and
;; the headline is re-rendered.  Setting ROTATION_INDEX by hand re-renders
;; it too, both via `org-set-property' (`C-c C-x p') and via
;; `org-property-next-allowed-value' (`S-right', when ROTATION_INDEX_ALL
;; lists the allowed values).  After editing the property drawer as plain
;; text, call `org-rotation-sync-heading' to catch up.

;;; Code:

(require 'org)

(defgroup org-rotation nil
  "Cycle a repeating org task through the variants listed as its children."
  :group 'org)

(defcustom org-rotation-index-property "ROTATION_INDEX"
  "Property holding the 1-based index of the variant that is due now.
Its presence is what marks an entry as a rotation."
  :type 'string
  :group 'org-rotation)

(defcustom org-rotation-prefix-property "ROTATION_PREFIX"
  "Property holding the common prefix of the rendered headline.
When absent or empty the headline is just the current variant."
  :type 'string
  :group 'org-rotation)

(defcustom org-rotation-heading-separator " — "
  "String inserted between the prefix and the current variant."
  :type 'string
  :group 'org-rotation)

(defcustom org-rotation-log-done t
  "Whether to log each completion in the log drawer of the finished variant.
This keeps a per-variant history, which the rotation's own LOGBOOK
cannot provide since it only knows that some variant was done."
  :type 'boolean
  :group 'org-rotation)

(defcustom org-rotation-log-text "Zrobione"
  "Text of the log line added to the finished variant, before the timestamp."
  :type 'string
  :group 'org-rotation)

(defvar org-rotation--inhibit nil
  "When non-nil, changes to the index property do not trigger a re-render.")

(defun org-rotation-entry-p ()
  "Return non-nil when the entry at point drives a rotation."
  (and (org-entry-get nil org-rotation-index-property) t))

(defun org-rotation--variants ()
  "Return the headlines of the direct children of the entry at point."
  (save-excursion
    (org-back-to-heading t)
    (let (variants)
      (when (org-goto-first-child)
        (push (org-get-heading t t t t) variants)
        (while (org-get-next-sibling)
          (push (org-get-heading t t t t) variants)))
      (nreverse variants))))

(defun org-rotation--index (count)
  "Return the current 1-based index, wrapped into the 1..COUNT range."
  (if (<= count 0)
      0
    (let ((raw (string-to-number
                (or (org-entry-get nil org-rotation-index-property) "1"))))
      (1+ (mod (1- raw) count)))))

(defun org-rotation--headline (variants index)
  "Render the headline for the INDEX-th of VARIANTS."
  (let ((prefix (org-trim (or (org-entry-get nil org-rotation-prefix-property) ""))))
    (concat (if (string-empty-p prefix)
                ""
              (concat prefix org-rotation-heading-separator))
            (nth (1- index) variants))))

;;;###autoload
(defun org-rotation-sync-heading ()
  "Re-render the headline of the rotation entry at point from its index.
Useful after editing the property drawer as plain text, which does not
go through `org-set-property' and so triggers no re-render."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (unless (org-rotation-entry-p)
      (user-error "Not a rotation: entry has no %s property"
                  org-rotation-index-property))
    (let ((variants (org-rotation--variants)))
      (unless variants
        (user-error "Not a rotation: entry has no child headlines"))
      (let* ((count (length variants))
             (index (org-rotation--index count)))
        ;; Write the wrapped value back, so a hand-typed index that runs
        ;; past the last variant does not keep disagreeing with the headline.
        (unless (equal (org-entry-get nil org-rotation-index-property)
                       (number-to-string index))
          (let ((org-rotation--inhibit t))
            (org-entry-put nil org-rotation-index-property
                           (number-to-string index))))
        (org-edit-headline (org-rotation--headline variants index))
        (when (called-interactively-p 'interactive)
          (message "Rotacja %d/%d: %s" index count (nth (1- index) variants)))))))

(defun org-rotation--indent-region (beg end)
  "Re-indent every line between BEG and END the way org would."
  (let ((limit (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (< (point) limit)
        (org-indent-line)
        (forward-line 1)))
    (set-marker limit nil)))

(defun org-rotation--log-drawer-line (drawer)
  "Return the position at the end of DRAWER's opening line in the entry at point.
The drawer is created just after the entry's metadata when missing."
  (let* ((bound (save-excursion (outline-next-heading) (point)))
         (case-fold-search t)
         (re (format "^[ \t]*:%s:[ \t]*$" (regexp-quote drawer))))
    (or (save-excursion (re-search-forward re bound t))
        (save-excursion
          (org-end-of-meta-data)
          (let ((beg (point)))
            (insert ":" drawer ":\n:END:\n")
            (org-rotation--indent-region beg (point))
            (goto-char beg)
            (line-end-position))))))

(defun org-rotation--insert-log-line (line)
  "Insert LINE as the newest entry in the log drawer of the entry at point.
The drawer is created when missing, and LINE picks up its indentation.
When logging into a drawer is turned off, LINE goes into the body."
  (org-back-to-heading t)
  (let ((drawer (org-log-into-drawer)))
    (if (not drawer)
        (progn
          (org-end-of-meta-data)
          (let ((beg (point)))
            (insert line "\n")
            (org-rotation--indent-region beg (point))))
      (goto-char (org-rotation--log-drawer-line drawer))
      (let ((indent (current-indentation)))
        (forward-line 1)
        (insert (make-string indent ?\s) line "\n")))))

(defun org-rotation--log-variant (index)
  "Log a completion in the INDEX-th child of the entry at point."
  (save-excursion
    (org-back-to-heading t)
    (when (org-goto-first-child)
      (let ((reached 1))
        (while (and (< reached index) (org-get-next-sibling))
          (setq reached (1+ reached)))
        (when (= reached index)
          (org-rotation--insert-log-line
           (format "- %s %s"
                   org-rotation-log-text
                   (format-time-string (org-time-stamp-format t t)))))))))

(defun org-rotation-advance ()
  "Move the rotation at point on to its next variant.
Meant for `org-todo-repeat-hook', which runs once the repeater has
already pushed SCHEDULED forward."
  (save-excursion
    (org-back-to-heading t)
    (when (org-rotation-entry-p)
      (let ((count (length (org-rotation--variants))))
        (when (> count 0)
          (let ((index (org-rotation--index count)))
            (when org-rotation-log-done
              (org-rotation--log-variant index))
            (let ((org-rotation--inhibit t))
              (org-entry-put nil org-rotation-index-property
                             (number-to-string (1+ (mod index count)))))
            (org-rotation-sync-heading)))))))

(defun org-rotation-on-property-change (property _value)
  "Re-render the headline when PROPERTY is the rotation index."
  (when (and (not org-rotation--inhibit)
             (equal property org-rotation-index-property)
             (derived-mode-p 'org-mode))
    (let ((org-rotation--inhibit t))
      (condition-case err
          (org-rotation-sync-heading)
        (error (message "org-rotation: %s" (error-message-string err)))))))

;;;###autoload
(defun org-rotation-setup ()
  "Make rotations advance on repeat and follow manual index edits."
  (add-hook 'org-todo-repeat-hook #'org-rotation-advance)
  (add-hook 'org-property-changed-functions #'org-rotation-on-property-change))

(provide 'org-rotation)

;;; org-rotation.el ends here

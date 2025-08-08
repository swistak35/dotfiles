;;; emacs-movies.el --- Movie database management for Org-mode

;;; Commentary:
;; This file contains functions for managing movie database entries in Org-mode.
;; It integrates with Upflix API to automatically refresh movie information.

;;; Code:

(require 'org)
(require 'json)
(require 'url)

(defvar rl-movies-upflix-replace-hostname "http://localhost:9393"
  "Hostname to replace in Upflix URLs for API calls.")

(defvar rl-movies-supported-subscriptions
  '("netflix" "disney" "viaplay" "skyshowtime" "canalplus" "cineman" "appletv" "hbomax" "cdapremium" "amazon" "tvpvod")
  "List of supported streaming service subscriptions.")

(defun extract-tmdb-id (url)
  "Extract TMDB ID and type from a TMDB URL.
For movies like 'https://www.themoviedb.org/movie/69315-battlestar-galactica-razor'
returns '(69315 movie)'.
For TV shows like 'https://www.themoviedb.org/tv/1972-battlestar-galactica'
returns '(1972 tvshow)'."
  (cond
   ((and url (string-match "themoviedb\\.org/movie/\\([0-9]+\\)" url))
    (list (match-string 1 url) 'movie))
   ((and url (string-match "themoviedb\\.org/tv/\\([0-9]+\\)" url))
    (list (match-string 1 url) 'tvshow))
   (t nil)))

(defun remove-property-from-all-entries (property)
  "Remove PROPERTY from all entries in the buffer."
  (interactive "sProperty to remove: ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (let ((props (org-entry-properties)))
        (when (assoc property props)
          (org-delete-property property))))))

(defun list-explicitly-defined-properties-in-buffer ()
  "List explicitly defined properties used in the current buffer."
  (interactive)
  (let ((properties '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let ((props (org-entry-properties)))
          (dolist (prop props)
            (when (cdr prop) ; Check if property has a value
              (push (car prop) properties))))))
    (message "Explicitly defined properties used in buffer: %s" (mapconcat 'identity (delete-dups properties) ", "))))

(defun list-explicitly-used-tags-in-buffer ()
  "List explicitly used tags in the current buffer."
  (interactive)
  (let ((tags '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let ((headline-tags (org-get-tags)))
          (dolist (tag headline-tags)
            (push tag tags)))))
    (message "Explicitly used tags in buffer: %s" (mapconcat 'identity (delete-dups tags) ", "))))

(defun fetch-and-parse-json (url)
  "Fetch a webpage from the given URL and parse it as JSON."
  (let ((buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (url-http-parse-response)
      (if (eq url-http-response-status 200)
          (progn
            (goto-char (point-min))
            (re-search-forward "\n\n")
            (delete-region (point-min) (point))
            (prog1
                (json-parse-buffer)
              (kill-buffer)))))))

(defun rl-movies-refresh-all-by-timestamp ()
  "Refresh all movie entries, processing those without timestamps first, then by timestamp order."
  (interactive)
  (let ((headlines-with-timestamps '())
        (headlines-without-timestamps '()))
    ;; Step 1: Iterate over all headlines
    (org-map-entries
     (lambda ()
       (let* ((timestamp (org-entry-get nil "LAST_REFRESHED"))
             (props (org-entry-properties))
             (upflix-url (assoc "UPFLIX_LINK" props)))
         (if (and upflix-url (not (string-empty-p (cdr upflix-url))))
           (if timestamp
               ;; Add entry with timestamp to the list
               (push (cons (org-read-date t t timestamp) (point)) headlines-with-timestamps)
               ;; Add entry without timestamp to the list
               (push (point) headlines-without-timestamps))))))
    ;; Step 2: Sort entries with timestamps
    (setq headlines-with-timestamps (sort headlines-with-timestamps (lambda (a b) (time-less-p (car a) (car b)))))
    ;; Step 3: Process entries without timestamp first
    (dolist (headline headlines-without-timestamps)
      (goto-char headline)
      (message "Processing entry without timestamp: %s" (org-get-heading t t))
      (rl-movies-refresh-entry))
    ;; Step 4: Process entries with timestamps
    (dolist (headline headlines-with-timestamps)
      (goto-char (cdr headline))
      (message "Processing entry with timestamp: %s" (org-get-heading t t))
      (rl-movies-refresh-entry))))

(defun rl-movies-refresh-all ()
  "Refresh all movie entries in the current buffer that have UPFLIX_LINK property."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (let ((props (org-entry-properties)))
        (when (assoc "UPFLIX_LINK" props)
          (rl-movies-refresh-entry))))))

(defun rl-movies-set-title ()
  "Update the headline title at point based on TITLE_PL, TITLE_EN, and YEAR properties."
  (interactive)
  (let* ((title-pl (org-entry-get (point) "TITLE_PL"))
         (title-en (org-entry-get (point) "TITLE_EN"))
         (year (org-entry-get (point) "YEAR"))
         (todo (org-get-todo-state))
         (tags (org-get-tags-string))
         (new-title (string-join (delq nil (list (if (= (length title-en) 0) nil (concat title-en " /")) title-pl (concat "(" year ")"))) " ")))
    (when new-title
      (save-excursion
        (org-back-to-heading t)
        (looking-at org-outline-regexp)
        (let ((stars (match-string 0)))
          ;; Replace the current headline, preserving leading stars, TODO state, and tags
          (re-search-forward org-heading-regexp)
          (replace-match (concat stars todo " " new-title " " tags) t t))))))

(defun rl-movies-refresh-entry ()
  "Refresh movie information for the current entry using Upflix API."
  (interactive)
  (let* ((link1 (org-entry-get nil "UPFLIX_LINK"))
         (api1 (replace-regexp-in-string (regexp-quote "https://upflix.pl") rl-movies-upflix-replace-hostname link1))
         (information (fetch-and-parse-json api1)))
    (if information
        (let* ((title-pl (gethash "polish_title" information))
               (title-en (gethash "english_title" information))
               (year (gethash "year" information))
               (genres (gethash "genres" information))
               (subscriptions (gethash "subscriptions" information))
               (filmweb-url (gethash "filmweb_url" information))
               (imdb-url (gethash "imdb_url" information))
               (current-local-tags (org-get-tags nil t)))
          (save-excursion
            (org-back-to-heading)
            (org-set-property "TITLE_PL" title-pl)
            (org-set-property "TITLE_EN" title-en)
            (if year
                (org-set-property "YEAR" year))
            (if genres
                (org-set-property "GENRES" (mapconcat 'identity genres " ")))
            (org-set-property "SUBSCRIPTIONS" (mapconcat 'identity subscriptions " "))
            (dolist (subscription-name rl-movies-supported-subscriptions)
              (let ((subscription-tag (concat "on_" subscription-name)))
                (if (cl-position subscription-name subscriptions :test #'equal)
                    (setq current-local-tags (append current-local-tags (list subscription-tag)))
                  (setq current-local-tags (delete subscription-tag current-local-tags)))))
            (if (and filmweb-url (not (eq filmweb-url :null)))
                (org-set-property "FILMWEB_URL" filmweb-url))
            (if (and imdb-url (not (eq imdb-url :null)))
                (org-set-property "IMDB_URL" imdb-url))
            (org-set-property "LAST_REFRESHED" (with-temp-buffer (org-time-stamp '(16) 'inactive) (buffer-string)))
            (org-set-tags (delete-dups current-local-tags))
            "ok"))
      (message "Failed to retrieve information"))))

;;; Tests

(require 'ert)

(ert-deftest test-extract-tmdb-id-movie ()
  "Test extracting TMDB ID from movie URLs."
  (should (equal (extract-tmdb-id "https://www.themoviedb.org/movie/69315-battlestar-galactica-razor")
                 '("69315" movie)))
  (should (equal (extract-tmdb-id "https://www.themoviedb.org/movie/123-some-movie")
                 '("123" movie)))
  (should (equal (extract-tmdb-id "https://www.themoviedb.org/movie/456789")
                 '("456789" movie))))

(ert-deftest test-extract-tmdb-id-tvshow ()
  "Test extracting TMDB ID from TV show URLs."
  (should (equal (extract-tmdb-id "https://www.themoviedb.org/tv/1972-battlestar-galactica")
                 '("1972" tvshow)))
  (should (equal (extract-tmdb-id "https://www.themoviedb.org/tv/789-some-show")
                 '("789" tvshow)))
  (should (equal (extract-tmdb-id "https://www.themoviedb.org/tv/12345")
                 '("12345" tvshow))))

(ert-deftest test-extract-tmdb-id-invalid ()
  "Test handling of invalid or unrecognized URLs."
  (should (equal (extract-tmdb-id "https://www.themoviedb.org/person/123-actor-name") nil))
  (should (equal (extract-tmdb-id "https://example.com/movie/123") nil))
  (should (equal (extract-tmdb-id "not-a-url") nil))
  (should (equal (extract-tmdb-id nil) nil))
  (should (equal (extract-tmdb-id "") nil)))

(provide 'emacs-movies)

;;; emacs-movies.el ends here
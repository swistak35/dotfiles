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

(defvar emacs-movies-directory "/media/plex/Wideo"
  "Directory containing video files.")

(defvar emacs-movies-video-extensions '("mkv" "mp4" "avi")
  "List of video file extensions to search for.")

(defun emacs-movies-all-video-files ()
  "Return a list of all video files in `emacs-movies-directory'.
Raises an error if the variable is not set or directory does not exist."
  (unless emacs-movies-directory
    (error "emacs-movies-directory is not set"))
  (unless (file-exists-p emacs-movies-directory)
    (error "Directory does not exist: %s" emacs-movies-directory))
  (unless (file-directory-p emacs-movies-directory)
    (error "Path is not a directory: %s" emacs-movies-directory))

  (let ((video-files '())
        (extensions-regex (concat "\\." (regexp-opt emacs-movies-video-extensions) "$")))
    (dolist (file (directory-files-recursively emacs-movies-directory extensions-regex))
      (when (file-regular-p file)
        (push file video-files)))
    (nreverse video-files)))

(defun extract-tmdb-id-from-filepath (filepath)
  "Extract TMDB ID from filepath if it contains a tmdb tag.
The tmdb tag format is {tmdb-TMDB_ID} where TMDB_ID are only numbers.
Returns the TMDB_ID as a string if found, nil otherwise."
  (let ((filename (file-name-nondirectory filepath)))
    (when (string-match "{tmdb-\\([0-9]+\\)}" filename)
      (match-string 1 filename))))

(defun filter-files-without-tmdb-tag (files)
  "Filter FILES list to return only files that do NOT have a TMDB tag.
Uses `extract-tmdb-id-from-filepath' to check for TMDB tags."
  (cl-remove-if (lambda (file)
                  (extract-tmdb-id-from-filepath file))
                files))

(defun filter-files-with-tmdb-tag (files)
  "Filter FILES list and return mapping from TMDB_ID to list of filepaths.
Returns an alist where each element is (TMDB_ID . (list of filepaths)).
Uses `extract-tmdb-id-from-filepath' to extract TMDB IDs."
  (let ((tmdb-map '()))
    (dolist (file files)
      (let ((tmdb-id (extract-tmdb-id-from-filepath file)))
        (when tmdb-id
          (let ((existing (assoc tmdb-id tmdb-map)))
            (if existing
                (setcdr existing (cons file (cdr existing)))
              (push (cons tmdb-id (list file)) tmdb-map))))))
    tmdb-map))

(defun deobfuscate-filepath (filepath)
  "Deobfuscate filename by removing codec/quality information for comparison.
Takes only the filename into account and removes technical metadata."
  (let* ((filename (file-name-nondirectory filepath))
         (name-without-ext (file-name-sans-extension filename))
         (deobfuscated name-without-ext))

    ;; Remove common quality indicators
    (setq deobfuscated (replace-regexp-in-string "\\b\\(720p\\|1080p\\|1440p\\|2160p\\|4K\\|HD\\|UHD\\|FHD\\)\\b" "" deobfuscated t t))

    ;; Remove common codec information
    (setq deobfuscated (replace-regexp-in-string "\\b\\(x264\\|x265\\|h264\\|h265\\|HEVC\\|AVC\\|XviD\\|DivX\\)\\b" "" deobfuscated t t))

    ;; Remove common source/release info
    (setq deobfuscated (replace-regexp-in-string "\\b\\(BluRay\\|BRRip\\|DVDRip\\|WEBRip\\|HDTV\\|WEB-DL\\|BDRip\\)\\b" "" deobfuscated t t))

    ;; Remove common audio codecs
    (setq deobfuscated (replace-regexp-in-string "\\b\\(AAC\\|AC3\\|DTS\\|MP3\\|FLAC\\)\\b" "" deobfuscated t t))

    ;; Remove release group info (usually at the end in brackets or after dash)
    (setq deobfuscated (replace-regexp-in-string "[-\\[].*$" "" deobfuscated))

    ;; Remove TMDB tags
    (setq deobfuscated (replace-regexp-in-string "{tmdb-[0-9]+}" "" deobfuscated))

    ;; Remove multiple spaces and trim
    (setq deobfuscated (replace-regexp-in-string "\\s-+" " " deobfuscated))
    (setq deobfuscated (string-trim deobfuscated))

    deobfuscated))

(defun edit-distance (str1 str2)
  "Compute the Levenshtein edit distance between STR1 and STR2.
Returns the minimum number of single-character edits (insertions, deletions,
or substitutions) required to change one string into the other."
  (let* ((len1 (length str1))
         (len2 (length str2))
         (matrix (make-vector (1+ len1) nil)))

    ;; Initialize matrix
    (dotimes (i (1+ len1))
      (aset matrix i (make-vector (1+ len2) 0)))

    ;; Fill first row and column
    (dotimes (i (1+ len1))
      (aset (aref matrix i) 0 i))
    (dotimes (j (1+ len2))
      (aset (aref matrix 0) j j))

    ;; Fill the matrix
    (dotimes (i len1)
      (dotimes (j len2)
        (let* ((row (1+ i))
               (col (1+ j))
               (cost (if (eq (aref str1 i) (aref str2 j)) 0 1))
               (deletion (1+ (aref (aref matrix i) col)))
               (insertion (1+ (aref (aref matrix row) j)))
               (substitution (+ (aref (aref matrix i) j) cost)))
          (aset (aref matrix row) col
                (min deletion insertion substitution)))))

    ;; Return the edit distance
    (aref (aref matrix len1) len2)))

(defun find-files-with-tmdb-id (tmdb-id files)
  "Find files with specific TMDB_ID tag from a list of FILES.
Returns a list of filepaths that contain {tmdb-TMDB_ID} tag matching the given ID.
Uses `filter-files-with-tmdb-tag' to group files by TMDB ID."
  (let ((tmdb-map (filter-files-with-tmdb-tag files)))
    (cdr (assoc tmdb-id tmdb-map))))

(defun emacs-movies-find-downloaded-file ()
  "Find downloaded file for current org entry using TMDB_URL.
Searches for files with matching TMDB ID and asks user for confirmation.
If confirmed, stores the filepath in DOWNLOADED_FILEPATH property."
  (interactive)
  (let ((tmdb-url (org-entry-get nil "TMDB_URL")))
    (unless tmdb-url
      (error "No TMDB_URL property found for this entry"))

    (let* ((tmdb-info (extract-tmdb-id tmdb-url))
           (tmdb-id (car tmdb-info)))
      (unless tmdb-id
        (error "Could not extract TMDB ID from URL: %s" tmdb-url))

      (let* ((all-files (emacs-movies-all-video-files))
             (files-with-tmdb (filter-files-with-tmdb-tag all-files))
             (matching-files (find-files-with-tmdb-id tmdb-id all-files)))

        (if matching-files
            (let ((file-list (mapconcat (lambda (file) 
                                          (format "  %s" file))
                                        matching-files "\n")))
              (when (yes-or-no-p (format "Found files:\n%s\n\nAre these the correct files for this entry? " file-list))
                (let* ((filepath (if (= (length matching-files) 1)
                                     (car matching-files)
                                   (completing-read "Select file: " matching-files)))
                       (filename (file-name-nondirectory filepath))
                       (org-link (format "[[file:%s][%s]]" filepath filename)))
                  (org-set-property "DOWNLOADED_FILEPATH" org-link)
                  (message "Set DOWNLOADED_FILEPATH to: %s" org-link))))
          (message "No downloaded files found with TMDB ID %s" tmdb-id))))))

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

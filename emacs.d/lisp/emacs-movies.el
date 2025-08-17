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

(defvar emacs-movies-tmdb-api-key nil
  "API key for The Movie Database (TMDB). Set this to use TMDB API functions.")

(defvar emacs-movies-tmdb-base-url "https://api.themoviedb.org/3"
  "Base URL for The Movie Database API.")

(defvar emacs-movies-tmdb-language "pl-PL"
  "Language code for TMDB API requests (e.g., 'pl-PL' for Polish, 'en-US' for English).")

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

(defun emacs-movies-search-tmdb (query)
  "Search for movies on TMDB using QUERY string.
Returns all search results with id, title, original_title, and overview.
Entries with corresponding files in the movies directory are marked with '(file on disk)' prefix.
Requires `emacs-movies-tmdb-api-key' to be set."
  (unless emacs-movies-tmdb-api-key
    (error "TMDB API key not set. Please set emacs-movies-tmdb-api-key"))

  (let* ((encoded-query (url-hexify-string query))
         (url (format "%s/search/movie?api_key=%s&query=%s&language=%s"
                      emacs-movies-tmdb-base-url
                      emacs-movies-tmdb-api-key
                      encoded-query
                      emacs-movies-tmdb-language))
         (response-buffer (url-retrieve-synchronously url)))

    (if response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t) ; Skip headers
          (let* ((json-data (json-parse-buffer :object-type 'alist))
                 (results (alist-get 'results json-data))
                 (video-files (condition-case nil
                                  (emacs-movies-all-video-files)
                                (error '())))
                 (tmdb-file-mapping (filter-files-with-tmdb-tag video-files)))
            (kill-buffer response-buffer)
            (mapcar (lambda (movie)
                      (let* ((movie-id (number-to-string (alist-get 'id movie)))
                             (has-file (assoc movie-id tmdb-file-mapping))
                             (title (alist-get 'title movie))
                             (marked-title (if has-file
                                               (concat "(file on disk) " title)
                                             title)))
                        (list :id (alist-get 'id movie)
                              :title marked-title
                              :original-title (alist-get 'original_title movie)
                              :overview (alist-get 'overview movie)
                              :release_date (alist-get 'release_date movie)
                              :original_language (alist-get 'original_language movie))))
                    results)))
      (error "Failed to retrieve data from TMDB API"))))

(defun emacs-movies-set-tmdb-url-from-heading ()
  "Set TMDB_URL property for current org entry based on heading.
Uses heading text before slash (/) as search query for TMDB.
Sets TMDB_URL property to the first search result's TMDB URL."
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         (query (if (string-match "\\(.*?\\)\\s-*/" heading)
                    (match-string 1 heading)
                  heading))
         (clean-query (string-trim query)))

    (if (string-empty-p clean-query)
        (error "No valid query found in heading")

      (message "Searching TMDB for: %s" clean-query)
      (let ((results (emacs-movies-search-tmdb clean-query)))
        (if results
            (let* ((choices (mapcar (lambda (result)
                                      (let* ((id (plist-get result :id))
                                             (title (plist-get result :title))
                                             (original-title (plist-get result :original-title))
                                             (overview (plist-get result :overview))
                                             (release-date (plist-get result :release_date))
                                             (year (if (and release-date
                                                           (stringp release-date)
                                                           (>= (length release-date) 4))
                                                       (substring release-date 0 4)
                                                     "Unknown"))
                                             (full-overview (if (and overview (> (length overview) 0))
                                                                overview
                                                              "No overview")))
                                        (format "%s / %s (%s) [ID: %d] - %s"
                                                title
                                                (or original-title title)
                                                year
                                                id
                                                full-overview)))
                                    results))
                   (choices-with-none (append choices (list "None of these"))))

              (message "Found movies:")
              (dolist (choice choices)
                (message "  %s" choice))

              (let ((selected (completing-read (format "Select movie for \"%s\": " (string-trim heading)) choices-with-none)))
                (unless (string= selected "None of these")
                  (let* ((selected-index (cl-position selected choices :test #'string=))
                         (selected-result (nth selected-index results))
                         (tmdb-id (plist-get selected-result :id))
                         (title (plist-get selected-result :title))
                         (original-title (plist-get selected-result :original-title))
                         (release-date (plist-get selected-result :release_date))
                         (original-language (plist-get selected-result :original_language))
                         (year (if (and release-date
                                       (stringp release-date)
                                       (>= (length release-date) 4))
                                   (substring release-date 0 4)
                                 ""))
                         (tmdb-url (format "https://www.themoviedb.org/movie/%d" tmdb-id)))

                    ;; Set all the properties
                    (org-set-property "TMDB_URL" tmdb-url)
                    (org-set-property "TITLE" (or title ""))
                    (org-set-property "ORIGINAL_TITLE" (or original-title ""))
                    (org-set-property "YEAR" year)
                    (org-set-property "ORIGINAL_LANGUAGE" (or original-language ""))

                    ;; Try to find downloaded file
                    (condition-case err
                        (emacs-movies-find-downloaded-file)
                      (error (message "Could not find downloaded file: %s" (error-message-string err))))

                    ;; Update the heading based on the new properties
                    (emacs-movies-update-heading-from-properties)

                    (message "Set TMDB properties: %s (%s)" title tmdb-url)))))
          (message "No results found for query: %s" clean-query))))))

(defun emacs-movies-update-heading-from-properties ()
  "Update org heading based on TITLE, ORIGINAL_TITLE, and YEAR properties.
Preserves existing tags. Rules:
- If TITLE and ORIGINAL_TITLE are same: 'TITLE (YEAR)'
- If different: 'ORIGINAL_TITLE / TITLE (YEAR)'
- If no TITLE or empty: don't change heading"
  (interactive)
  (let* ((title (org-entry-get nil "TITLE"))
         (original-title (org-entry-get nil "ORIGINAL_TITLE"))
         (year (org-entry-get nil "YEAR"))
         (current-tags (org-get-tags nil t)))

    (when (and title (not (string-empty-p title)))
      (let* ((year-part (if (and year (not (string-empty-p year)))
                            (format " (%s)" year)
                          ""))
             (new-heading (if (and original-title
                                   (not (string-empty-p original-title))
                                   (not (string= title original-title)))
                              (format "%s / %s%s" original-title title year-part)
                            (format "%s%s" title year-part))))

        (save-excursion
          (org-back-to-heading t)
          (looking-at org-outline-regexp)
          (let* ((stars (match-string 0))
                 (todo-state (org-get-todo-state))
                 (todo-part (if todo-state (concat todo-state " ") ""))
                 (tags-part (if current-tags
                                (concat " :" (mapconcat 'identity current-tags ":") ":")
                              "")))
            (re-search-forward org-heading-regexp)
            (replace-match (concat stars todo-part new-heading tags-part) t t)
            (message "Updated heading to: %s" new-heading)))))))

(defun emacs-movies-backfill-tmdb-data ()
  "Backfill TMDB data for all entries with TODO states that don't have TMDB_URL property.
Calls `emacs-movies-set-tmdb-url-from-heading' for each matching entry."
  (interactive)
  (let ((processed-count 0)
        (skipped-count 0)
        (entries-to-process '()))

    (message "Starting backfill process...")

    ;; First pass: collect entries that need processing
    (org-map-entries
     (lambda ()
       (let* ((todo-state (org-get-todo-state))
              (has-todo-state (not (null todo-state)))
              (has-tmdb-url (org-entry-get nil "TMDB_URL"))
              (heading (org-get-heading t t t t)))

         (message "Found entry: %s (todo: %s)" heading (or todo-state "none"))

         (cond
          ((not has-todo-state)
           (message "  -> Skipping (no TODO state)")
           (setq skipped-count (1+ skipped-count)))

          (has-tmdb-url
           (message "  -> Skipping (has TMDB_URL: %s)" has-tmdb-url)
           (setq skipped-count (1+ skipped-count)))

          (t
           (message "  -> Will process: %s" heading)
           (push (point) entries-to-process))))))

    ;; Second pass: process the entries
    (dolist (entry-point entries-to-process)
      (goto-char entry-point)
      (let ((heading (org-get-heading t t t t)))
        (message "Processing: %s" heading)
        (emacs-movies-set-tmdb-url-from-heading)
        (setq processed-count (1+ processed-count))))

    (message "Backfill complete: %d processed, %d skipped" processed-count skipped-count)))

(defun emacs-movies-find-orphaned-files ()
  "Find video files with TMDB IDs that don't have corresponding org entries.
Lists files that exist on disk but are missing from org-mode tracking."
  (interactive)
  (message "Searching for orphaned movie files...")
  
  (let* ((all-files (emacs-movies-all-video-files))
         (files-with-tmdb (filter-files-with-tmdb-tag all-files))
         (file-tmdb-ids (mapcar #'car files-with-tmdb))
         (org-tmdb-ids '())
         (orphaned-files '()))
    
    ;; Collect TMDB IDs from org entries
    (org-map-entries
     (lambda ()
       (let ((tmdb-url (org-entry-get nil "TMDB_URL")))
         (when tmdb-url
           (let* ((tmdb-info (extract-tmdb-id tmdb-url))
                  (tmdb-id (car tmdb-info)))
             (when tmdb-id
               (push tmdb-id org-tmdb-ids)))))))
    
    (message "Found %d files with TMDB IDs" (length file-tmdb-ids))
    (message "Found %d org entries with TMDB URLs" (length org-tmdb-ids))
    
    ;; Find files whose TMDB IDs are not in org entries
    (dolist (file-tmdb-id file-tmdb-ids)
      (unless (member file-tmdb-id org-tmdb-ids)
        (let ((files-for-id (find-files-with-tmdb-id file-tmdb-id all-files)))
          (dolist (file files-for-id)
            (push (cons file-tmdb-id file) orphaned-files)))))
    
    ;; Report results
    (if orphaned-files
        (progn
          (message "Found %d orphaned files:" (length orphaned-files))
          (dolist (orphan orphaned-files)
            (message "  TMDB ID %s: %s" (car orphan) (file-name-nondirectory (cdr orphan)))))
      (message "No orphaned files found - all files have corresponding org entries"))
    
    orphaned-files))

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

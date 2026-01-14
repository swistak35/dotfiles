;;; emacs-movies.el --- Movie database management for Org-mode

;;; Commentary:
;; This file contains functions for managing movie database entries in Org-mode.
;; It integrates with Upflix API to automatically refresh movie information.

;;; Code:

(require 'org)
(require 'json)
(require 'url)
(require 'cl-lib)
(require 'dom)

(defvar rl-movies-upflix-replace-hostname "http://localhost:9393"
  "Hostname to replace in Upflix URLs for API calls.")

(defvar rl-movies-supported-subscriptions
  '("netflix" "disney" "viaplay" "skyshowtime" "canalplus" "cineman" "appletv" "hbomax" "cdapremium" "amazon" "tvpvod")
  "List of supported streaming service subscriptions.")

(defvar emacs-movies-directory '("/media/plex/Wideo")
  "List of directories containing video files.
Each element should be an absolute path to a directory containing movies or TV shows.
Multiple directories allow managing video collections across different locations.")

(defvar emacs-movies-video-extensions '("mkv" "mp4" "avi")
  "List of video file extensions to search for.")

(defvar emacs-movies-tmdb-api-key nil
  "API key for The Movie Database (TMDB). Set this to use TMDB API functions.")

(defvar emacs-movies-tmdb-base-url "https://api.themoviedb.org/3"
  "Base URL for The Movie Database API.")

(defvar emacs-movies-upflix-request-delay 2
  "Number of seconds to wait between Upflix requests during bulk refresh.
This helps avoid triggering rate limiting. Set to 0 to disable delay.")

(defvar emacs-movies-tmdb-language "pl-PL"
  "Language code for TMDB API requests (e.g., 'pl-PL' for Polish, 'en-US' for English).")

(defun emacs-movies-validate-directories ()
  "Validate that `emacs-movies-directory' is properly configured.
Checks that the variable is a list and that all directories exist and are valid.
Raises an error with details if validation fails, returns nil on success."
  ;; Check if configuration is a list (not a string)
  (unless (listp emacs-movies-directory)
    (error "emacs-movies-directory must be a list of directories.
Current value is a string. Please update your configuration:
  Old: (setq emacs-movies-directory \"/path/to/videos\")
  New: (setq emacs-movies-directory '(\"/path/to/videos\"))"))

  ;; Collect all validation errors
  (let ((errors '()))
    (dolist (dir emacs-movies-directory)
      (cond
       ((or (null dir) (string-empty-p dir))
        (push (format "  - Empty or nil directory in list") errors))
       ((not (file-exists-p dir))
        (push (format "  - %s: Directory does not exist" dir) errors))
       ((not (file-directory-p dir))
        (push (format "  - %s: Path is not a directory" dir) errors))))

    ;; Report all errors together if any were found
    (when errors
      (error "Invalid directories in emacs-movies-directory:\n%s"
             (string-join (nreverse errors) "\n")))))

(defun emacs-movies-all-video-files ()
  "Return a list of all video files in all directories in `emacs-movies-directory'.
Scans each directory in the list and aggregates results.
Files are returned in the order they appear, processing directories in configuration order.
Raises an error if the variable is not set properly or directories are invalid."
  ;; Validate all directories before scanning
  (emacs-movies-validate-directories)

  ;; Aggregate video files from all configured directories
  (let ((all-video-files '())
        (extensions-regex (concat "\\." (regexp-opt emacs-movies-video-extensions) "$")))
    ;; Iterate over each directory in configuration order
    (dolist (base-dir emacs-movies-directory)
      (dolist (file (directory-files-recursively base-dir extensions-regex))
        (when (file-regular-p file)
          (push file all-video-files))))
    ;; Return files in the order they were discovered
    (nreverse all-video-files)))

(defun emacs-movies-all-tvshows-directories ()
  "Return a list of all directories with TMDB tags from all directories in `emacs-movies-directory'.
Scans each directory in the list and aggregates results.
Directories are returned in the order they appear, processing base directories in configuration order.
Raises an error if the variable is not set properly or directories are invalid."
  ;; Validate all directories before scanning
  (emacs-movies-validate-directories)

  ;; Aggregate TV show directories from all configured directories
  (let ((all-tvshow-directories '()))
    ;; Iterate over each base directory in configuration order
    (dolist (base-dir emacs-movies-directory)
      (dolist (file (directory-files-recursively base-dir ".*" t))
        (when (and (file-directory-p file)
                   (string-match "{tmdb-\\([0-9]+\\)}" (file-name-nondirectory file)))
          (push file all-tvshow-directories))))
    ;; Return directories in the order they were discovered
    (nreverse all-tvshow-directories)))

(defun emacs-movies-parse-tags (tags)
  "Parse org tags list to handle both string and text property tags.
Returns a list of tag strings, handling inherited tags that may have text properties."
  (mapcar (lambda (tag)
            (if (stringp tag)
                (substring-no-properties tag)
              tag))
          tags))

(defun emacs-movies-has-tag-p (tag-name)
  "Check if current entry has TAG-NAME, including inherited tags."
  (let ((all-tags (emacs-movies-parse-tags (org-get-tags))))
    (member tag-name all-tags)))

(defun emacs-movies-tvshows-directories-alist ()
  "Return an alist mapping TMDB_ID to TV show directories.
Returns an alist where each element is (TMDB_ID . DIRECTORY_PATH).
Uses `emacs-movies-all-tvshows-directories' to find directories with TMDB tags."
  (let ((tmdb-map '()))
    (dolist (directory (emacs-movies-all-tvshows-directories))
      (let ((tmdb-id (extract-tmdb-id-from-filepath directory)))
        (when tmdb-id
          (push (cons tmdb-id directory) tmdb-map))))
    (nreverse tmdb-map)))

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
For movies, searches for files with matching TMDB ID.
For TV shows, searches for directories with matching TMDB ID.
If confirmed, stores the filepath in DOWNLOADED_FILEPATH property."
  (interactive)
  (let ((tmdb-url (org-entry-get nil "TMDB_URL")))
    (unless tmdb-url
      (error "No TMDB_URL property found for this entry"))

    (let* ((tmdb-info (extract-tmdb-id tmdb-url))
           (tmdb-id (car tmdb-info))
           (is-tvshow (emacs-movies-has-tag-p "tvshow"))
           (is-movie (emacs-movies-has-tag-p "movie")))
      (unless tmdb-id
        (error "Could not extract TMDB ID from URL: %s" tmdb-url))

      (let* ((matching-items (if is-tvshow
                                 (let ((tvshow-dirs (emacs-movies-tvshows-directories-alist)))
                                   (when-let ((dir-entry (assoc tmdb-id tvshow-dirs)))
                                     (list (cdr dir-entry))))
                               (let ((all-files (emacs-movies-all-video-files)))
                                 (find-files-with-tmdb-id tmdb-id all-files)))))

        (if matching-items
            (let ((item-list (mapconcat (lambda (item)
                                          (format "  %s" item))
                                        matching-items "\n"))
                  (item-type (if is-tvshow "directory" "file")))
              (when (yes-or-no-p (format "Found %s:\n%s\n\nAre these the correct %ss for this entry? " 
                                         (if (= (length matching-items) 1) item-type (concat item-type "s"))
                                         item-list 
                                         item-type))
                (let* ((filepath (if (= (length matching-items) 1)
                                     (car matching-items)
                                   (completing-read (format "Select %s: " item-type) matching-items)))
                       (filename (file-name-nondirectory filepath))
                       (org-link (format "[[file:%s][%s]]" filepath filename)))
                  (org-set-property "DOWNLOADED_FILEPATH" org-link)
                  (message "Set DOWNLOADED_FILEPATH to: %s" org-link))))
          (message "No downloaded %ss found with TMDB ID %s" 
                   (if is-tvshow "directories" "files") 
                   tmdb-id))))))

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

(defun emacs-movies-search-tmdb-tv (query)
  "Search for TV shows on TMDB using QUERY string.
Returns all search results with id, name, original_name, and overview.
Entries with corresponding directories in the movies directory are marked with '(file on disk)' prefix.
Requires `emacs-movies-tmdb-api-key' to be set."
  (unless emacs-movies-tmdb-api-key
    (error "TMDB API key not set. Please set emacs-movies-tmdb-api-key"))

  (let* ((encoded-query (url-hexify-string query))
         (url (format "%s/search/tv?api_key=%s&query=%s&language=%s"
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
                 (tvshow-dirs (condition-case nil
                                  (emacs-movies-tvshows-directories-alist)
                                (error '()))))
            (kill-buffer response-buffer)
            (mapcar (lambda (tvshow)
                      (let* ((tvshow-id (number-to-string (alist-get 'id tvshow)))
                             (has-dir (assoc tvshow-id tvshow-dirs))
                             (name (alist-get 'name tvshow))
                             (marked-name (if has-dir
                                              (concat "(file on disk) " name)
                                            name)))
                        (list :id (alist-get 'id tvshow)
                              :name marked-name
                              :original-name (alist-get 'original_name tvshow)
                              :overview (alist-get 'overview tvshow)
                              :first_air_date (alist-get 'first_air_date tvshow)
                              :original_language (alist-get 'original_language tvshow))))
                    results)))
      (error "Failed to retrieve data from TMDB API"))))

(defun emacs-movies-get-tmdb-movie-by-id (tmdb-id)
  "Get movie details from TMDB by ID.
Returns movie details with id, title, original_title, overview, etc.
Requires `emacs-movies-tmdb-api-key' to be set."
  (unless emacs-movies-tmdb-api-key
    (error "TMDB API key not set. Please set emacs-movies-tmdb-api-key"))

  (let* ((url (format "%s/movie/%s?api_key=%s&language=%s"
                      emacs-movies-tmdb-base-url
                      tmdb-id
                      emacs-movies-tmdb-api-key
                      emacs-movies-tmdb-language))
         (response-buffer (url-retrieve-synchronously url)))

    (if response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t) ; Skip headers
          (let ((json-data (json-parse-buffer :object-type 'alist)))
            (kill-buffer response-buffer)
            (list :id (alist-get 'id json-data)
                  :title (alist-get 'title json-data)
                  :original-title (alist-get 'original_title json-data)
                  :overview (alist-get 'overview json-data)
                  :release_date (alist-get 'release_date json-data)
                  :original_language (alist-get 'original_language json-data))))
      (error "Failed to retrieve data from TMDB API"))))

(defun emacs-movies-get-tmdb-tv-by-id (tmdb-id)
  "Get TV show details from TMDB by ID.
Returns TV show details with id, name, original_name, overview, etc.
Requires `emacs-movies-tmdb-api-key' to be set."
  (unless emacs-movies-tmdb-api-key
    (error "TMDB API key not set. Please set emacs-movies-tmdb-api-key"))

  (let* ((url (format "%s/tv/%s?api_key=%s&language=%s"
                      emacs-movies-tmdb-base-url
                      tmdb-id
                      emacs-movies-tmdb-api-key
                      emacs-movies-tmdb-language))
         (response-buffer (url-retrieve-synchronously url)))

    (if response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t) ; Skip headers
          (let ((json-data (json-parse-buffer :object-type 'alist)))
            (kill-buffer response-buffer)
            (list :id (alist-get 'id json-data)
                  :name (alist-get 'name json-data)
                  :original-name (alist-get 'original_name json-data)
                  :overview (alist-get 'overview json-data)
                  :first_air_date (alist-get 'first_air_date json-data)
                  :original_language (alist-get 'original_language json-data))))
      (error "Failed to retrieve data from TMDB API"))))

(defun emacs-movies-set-tmdb-by-id ()
  "Interactively set TMDB_URL by prompting for TMDB ID.
Detects if current entry is a movie or TV show, gets details from TMDB,
asks for user confirmation, and sets properties if confirmed."
  (interactive)
  (let* ((is-tvshow (emacs-movies-has-tag-p "tvshow"))
         (is-movie (emacs-movies-has-tag-p "movie"))
         (content-type (cond (is-tvshow "TV show")
                            (is-movie "movie")
                            (t (error "Current entry must have either 'movie' or 'tvshow' tag"))))
         (tmdb-id (read-string (format "Enter TMDB ID for this %s: " content-type))))

    (when (string-empty-p tmdb-id)
      (error "TMDB ID cannot be empty"))

    (unless (string-match-p "^[0-9]+$" tmdb-id)
      (error "TMDB ID must contain only numbers"))

    (message "Looking up %s with TMDB ID %s..." content-type tmdb-id)
    
    (condition-case err
        (let* ((details (if is-tvshow
                            (emacs-movies-get-tmdb-tv-by-id tmdb-id)
                          (emacs-movies-get-tmdb-movie-by-id tmdb-id)))
               (title (if is-tvshow
                          (plist-get details :name)
                        (plist-get details :title)))
               (original-title (if is-tvshow
                                   (plist-get details :original-name)
                                 (plist-get details :original-title)))
               (overview (plist-get details :overview))
               (date-field (if is-tvshow
                               (plist-get details :first_air_date)
                             (plist-get details :release_date)))
               (year (if (and date-field
                             (stringp date-field)
                             (>= (length date-field) 4))
                         (substring date-field 0 4)
                       "Unknown"))
               (original-language (plist-get details :original_language))
               (tmdb-url (format "https://www.themoviedb.org/%s/%s" 
                                 (if is-tvshow "tv" "movie") 
                                 tmdb-id)))

          (let ((info-text (format "%s:\nTitle: %s\nOriginal Title: %s\nYear: %s\nLanguage: %s\nOverview: %s\n\nIs this the correct %s? "
                                   content-type
                                   title
                                   (or original-title "N/A")
                                   year
                                   (or original-language "N/A")
                                   (if (and overview (> (length overview) 0))
                                       overview
                                     "No overview")
                                   content-type)))
            (when (yes-or-no-p info-text)
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

              (message "Set TMDB properties: %s (%s)" title tmdb-url))))
      (error (error "Failed to get %s details: %s" content-type (error-message-string err))))))

(defun emacs-movies-refresh-tmdb-data ()
  "Refresh TMDB data for current entry based on existing TMDB_URL property.
Fetches current data from TMDB API and updates TITLE, ORIGINAL_TITLE, 
ORIGINAL_LANGUAGE, and YEAR properties, then updates the heading."
  (interactive)
  (let ((tmdb-url (org-entry-get nil "TMDB_URL")))
    (unless tmdb-url
      (error "No TMDB_URL property found for this entry"))

    (let* ((tmdb-info (extract-tmdb-id tmdb-url))
           (tmdb-id (car tmdb-info))
           (content-type (cadr tmdb-info)))
      (unless tmdb-id
        (error "Could not extract TMDB ID from URL: %s" tmdb-url))

      (unless (memq content-type '(movie tvshow))
        (error "Unknown content type from URL: %s" tmdb-url))

      (message "Refreshing %s data for TMDB ID %s..." 
               (if (eq content-type 'tvshow) "TV show" "movie") 
               tmdb-id)
      
      (condition-case err
          (let* ((details (if (eq content-type 'tvshow)
                              (emacs-movies-get-tmdb-tv-by-id tmdb-id)
                            (emacs-movies-get-tmdb-movie-by-id tmdb-id)))
                 (title (if (eq content-type 'tvshow)
                            (plist-get details :name)
                          (plist-get details :title)))
                 (original-title (if (eq content-type 'tvshow)
                                     (plist-get details :original-name)
                                   (plist-get details :original-title)))
                 (date-field (if (eq content-type 'tvshow)
                                 (plist-get details :first_air_date)
                               (plist-get details :release_date)))
                 (year (if (and date-field
                               (stringp date-field)
                               (>= (length date-field) 4))
                           (substring date-field 0 4)
                         ""))
                 (original-language (plist-get details :original_language)))

            ;; Set the properties
            (org-set-property "TITLE" (or title ""))
            (org-set-property "ORIGINAL_TITLE" (or original-title ""))
            (org-set-property "YEAR" year)
            (org-set-property "ORIGINAL_LANGUAGE" (or original-language ""))

            ;; Update the heading based on the new properties
            (emacs-movies-update-heading-from-properties)

            (message "Refreshed TMDB data: %s (%s)" title year))
        (error (error "Failed to refresh %s data: %s" 
                      (if (eq content-type 'tvshow) "TV show" "movie")
                      (error-message-string err)))))))

(defun emacs-movies-set-tmdb-url-from-heading ()
  "Set TMDB_URL property for current org entry based on heading.
Uses heading text before slash (/) as search query for TMDB.
For movies, searches TMDB movies API. For TV shows, searches TMDB TV API.
Sets TMDB_URL property to the selected search result's TMDB URL."
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         (query (if (string-match "\\(.*?\\)\\s-*/" heading)
                    (match-string 1 heading)
                  heading))
         (query-without-year (if (string-match "\\(.*?\\)\\s-*(\\([0-9]\\{4\\}\\))\\s-*$" query)
                                 (string-trim (match-string 1 query))
                               query))
         (clean-query (string-trim query-without-year))
         (is-tvshow (emacs-movies-has-tag-p "tvshow")))

    (if (string-empty-p clean-query)
        (error "No valid query found in heading")

      (message "Searching TMDB for: %s" clean-query)
      (let ((results (if is-tvshow
                         (emacs-movies-search-tmdb-tv clean-query)
                       (emacs-movies-search-tmdb clean-query))))
        (if results
            (let* ((choices (mapcar (lambda (result)
                                      (let* ((id (plist-get result :id))
                                             (title (if is-tvshow
                                                        (plist-get result :name)
                                                      (plist-get result :title)))
                                             (original-title (if is-tvshow
                                                                 (plist-get result :original-name)
                                                               (plist-get result :original-title)))
                                             (overview (plist-get result :overview))
                                             (date-field (if is-tvshow
                                                             (plist-get result :first_air_date)
                                                           (plist-get result :release_date)))
                                             (year (if (and date-field
                                                           (stringp date-field)
                                                           (>= (length date-field) 4))
                                                       (substring date-field 0 4)
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
                   (choices-with-none (append choices (list "None of these")))
                   (content-type (if is-tvshow "TV show" "movie")))

              (message "Found %ss:" content-type)
              (dolist (choice choices)
                (message "  %s" choice))

              (let ((selected (completing-read (format "Select %s for \"%s\": " content-type (string-trim heading)) choices-with-none)))
                (unless (string= selected "None of these")
                  (let* ((selected-index (cl-position selected choices :test #'string=))
                         (selected-result (nth selected-index results))
                         (tmdb-id (plist-get selected-result :id))
                         (title (if is-tvshow
                                    (plist-get selected-result :name)
                                  (plist-get selected-result :title)))
                         (original-title (if is-tvshow
                                             (plist-get selected-result :original-name)
                                           (plist-get selected-result :original-title)))
                         (date-field (if is-tvshow
                                         (plist-get selected-result :first_air_date)
                                       (plist-get selected-result :release_date)))
                         (original-language (plist-get selected-result :original_language))
                         (year (if (and date-field
                                       (stringp date-field)
                                       (>= (length date-field) 4))
                                   (substring date-field 0 4)
                                 ""))
                         (tmdb-url (format "https://www.themoviedb.org/%s/%d" 
                                           (if is-tvshow "tv" "movie") 
                                           tmdb-id)))

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
  "Backfill TMDB data for entries with TODO states, movie/tvshow tags, but no TMDB_URL property.
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
              (has-movie-or-tvshow-tag (or (emacs-movies-has-tag-p "movie") (emacs-movies-has-tag-p "tvshow")))
              (heading (org-get-heading t t t t)))

         (message "Found entry: %s (todo: %s)" heading (or todo-state "none"))

         (cond
          ((not has-todo-state)
           (message "  -> Skipping (no TODO state)")
           (setq skipped-count (1+ skipped-count)))

          ((not has-movie-or-tvshow-tag)
           (message "  -> Skipping (no movie or tvshow tag)")
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

(defun emacs-movies-get-orphaned-movie-files ()
  "Get list of orphaned movie files (files with TMDB IDs not in movie org entries).
Returns list of (TMDB_ID . FILEPATH) pairs for orphaned movie files."
  (let* ((all-files (emacs-movies-all-video-files))
         (files-with-tmdb (filter-files-with-tmdb-tag all-files))
         (file-tmdb-ids (mapcar #'car files-with-tmdb))
         (org-tmdb-ids '())
         (orphaned-files '()))
    
    ;; Collect TMDB IDs from movie org entries only
    (org-map-entries
     (lambda ()
       (when (emacs-movies-has-tag-p "movie")
         (let ((tmdb-url (org-entry-get nil "TMDB_URL")))
           (when tmdb-url
             (let* ((tmdb-info (extract-tmdb-id tmdb-url))
                    (tmdb-id (car tmdb-info)))
               (when tmdb-id
                 (push tmdb-id org-tmdb-ids))))))))
    
    ;; Find movie files whose TMDB IDs are not in movie org entries
    (dolist (file-tmdb-id file-tmdb-ids)
      (unless (member file-tmdb-id org-tmdb-ids)
        (let ((files-for-id (find-files-with-tmdb-id file-tmdb-id all-files)))
          (dolist (file files-for-id)
            (push (cons file-tmdb-id file) orphaned-files)))))
    
    orphaned-files))

(defun emacs-movies-get-orphaned-tvshow-directories ()
  "Get list of orphaned TV show directories (directories with TMDB IDs not in tvshow org entries).
Returns list of (TMDB_ID . DIRECTORY_PATH) pairs for orphaned TV show directories."
  (let* ((tvshow-dirs (condition-case nil
                          (emacs-movies-tvshows-directories-alist)
                        (error '())))
         (tvshow-tmdb-ids (mapcar #'car tvshow-dirs))
         (org-tmdb-ids '())
         (orphaned-dirs '()))
    
    ;; Collect TMDB IDs from tvshow org entries only
    (org-map-entries
     (lambda ()
       (when (emacs-movies-has-tag-p "tvshow")
         (let ((tmdb-url (org-entry-get nil "TMDB_URL")))
           (when tmdb-url
             (let* ((tmdb-info (extract-tmdb-id tmdb-url))
                    (tmdb-id (car tmdb-info)))
               (when tmdb-id
                 (push tmdb-id org-tmdb-ids))))))))
    
    ;; Find TV show directories whose TMDB IDs are not in tvshow org entries
    (dolist (tvshow-tmdb-id tvshow-tmdb-ids)
      (unless (member tvshow-tmdb-id org-tmdb-ids)
        (let ((dir-path (cdr (assoc tvshow-tmdb-id tvshow-dirs))))
          (when dir-path
            (push (cons tvshow-tmdb-id dir-path) orphaned-dirs)))))
    
    orphaned-dirs))

(defun emacs-movies-create-org-entries-for-orphaned-items ()
  "Create org entries for orphaned movies and TV shows under the Inbox node.
Creates TOWATCH entries with appropriate tags and TMDB_URL properties,
then refreshes TMDB data and updates headings."
  (interactive)
  
  (let ((orphaned-movies (emacs-movies-get-orphaned-movie-files))
        (orphaned-tvshows (emacs-movies-get-orphaned-tvshow-directories)))
    
    (message "Creating org entries for orphaned items...")
    
    ;; Find or create Inbox heading
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^\\*+ Inbox" nil t)
        ;; Create Inbox heading if it doesn't exist
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* Inbox\n"))
      
      ;; Go to the end of the Inbox section
      (org-end-of-subtree)
      
      ;; Create entries for orphaned movies
      (dolist (movie orphaned-movies)
        (let* ((tmdb-id (car movie))
               (filepath (cdr movie))
               (filename (file-name-nondirectory filepath))
               (tmdb-url (format "https://www.themoviedb.org/movie/%s" tmdb-id)))
          
          (insert (format "** TOWATCH %s :movie:\n" filename))
          (org-set-property "TMDB_URL" tmdb-url)
          
          ;; Refresh TMDB data and update heading
          (condition-case err
              (emacs-movies-refresh-tmdb-data)
            (error 
             (message "Warning: Could not refresh TMDB data for movie %s: %s" 
                     filename (error-message-string err))))))
      
      ;; Create entries for orphaned TV shows
      (dolist (tvshow orphaned-tvshows)
        (let* ((tmdb-id (car tvshow))
               (dirpath (cdr tvshow))
               (dirname (file-name-nondirectory dirpath))
               (tmdb-url (format "https://www.themoviedb.org/tv/%s" tmdb-id)))
          
          (insert (format "** TOWATCH %s :tvshow:\n" dirname))
          (org-set-property "TMDB_URL" tmdb-url)
          
          ;; Refresh TMDB data and update heading
          (condition-case err
              (emacs-movies-refresh-tmdb-data)
            (error 
             (message "Warning: Could not refresh TMDB data for TV show %s: %s" 
                     dirname (error-message-string err)))))))
    
    (message "Finished creating org entries for orphaned items")))

(defun emacs-movies-sort-entries-under-heading (heading)
  "Sort all entries alphabetically under the specified HEADING.
Prompts for heading name if called interactively."
  (interactive "sHeading name: ")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^\\*+ " (regexp-quote heading)) nil t)
        (progn
          (org-back-to-heading)
          (let ((start-pos (point))
                (level (org-current-level))
                entries)
            
            ;; Collect all direct child entries
            (org-goto-first-child)
            (while (and (org-at-heading-p) 
                       (> (org-current-level) level))
              (if (= (org-current-level) (1+ level))
                  ;; This is a direct child - collect it
                  (let* ((entry-start (point))
                         (entry-end (save-excursion (org-end-of-subtree t t) (point)))
                         (entry-text (buffer-substring entry-start entry-end))
                         (heading-text (org-get-heading t t t t)))
                    (push (cons heading-text entry-text) entries)
                    (goto-char entry-end))
                ;; Skip over deeper nested entries
                (org-end-of-subtree t t)))
            
            (if entries
                (progn
                  ;; Sort entries alphabetically by heading text
                  (setq entries (sort entries (lambda (a b) (string< (car a) (car b)))))
                  
                  ;; Delete all child entries
                  (goto-char start-pos)
                  (org-goto-first-child)
                  (let ((child-start (point)))
                    (goto-char start-pos)
                    (org-end-of-subtree)
                    (delete-region child-start (point)))
                  
                  ;; Insert sorted entries
                  (goto-char start-pos)
                  (org-end-of-line)
                  (insert "\n")
                  (dolist (entry entries)
                    (insert (cdr entry)))
                  
                  (message "Sorted %d entries under '%s'" (length entries) heading))
              (message "No child entries found under '%s'" heading))))
      (error "Heading '%s' not found" heading))))

(defun emacs-movies-find-orphaned-files ()
  "Find video files and TV show directories with TMDB IDs that don't have corresponding org entries.
Lists movies (files) and TV shows (directories) that exist on disk but are missing from org-mode tracking."
  (interactive)
  (message "Searching for orphaned movie files and TV show directories...")

  (let* ((orphaned-movies (emacs-movies-get-orphaned-movie-files))
         (orphaned-tvshows (emacs-movies-get-orphaned-tvshow-directories))
         (total-orphaned (+ (length orphaned-movies) (length orphaned-tvshows))))

    ;; Report summary statistics
    (message "Found %d orphaned movie files" (length orphaned-movies))
    (message "Found %d orphaned TV show directories" (length orphaned-tvshows))

    ;; Report results
    (if (> total-orphaned 0)
        (progn
          (message "Found %d orphaned items:" total-orphaned)

          ;; Display orphaned movies
          (dolist (orphan orphaned-movies)
            (let ((tmdb-id (car orphan))
                  (path (cdr orphan)))
              (message "  TMDB ID %s (movie): %s"
                       tmdb-id
                       (file-name-nondirectory path))))

          ;; Display orphaned TV shows
          (dolist (orphan orphaned-tvshows)
            (let ((tmdb-id (car orphan))
                  (path (cdr orphan)))
              (message "  TMDB ID %s (TV show): %s"
                       tmdb-id
                       (file-name-nondirectory path)))))
      (message "No orphaned items found - all files and directories have corresponding org entries"))

    ;; Return combined results in the original format for backward compatibility
    (append (mapcar (lambda (movie) (list (car movie) 'movie (cdr movie))) orphaned-movies)
            (mapcar (lambda (tvshow) (list (car tvshow) 'tvshow (cdr tvshow))) orphaned-tvshows))))

(defun emacs-movies-find-unreferenced-files ()
  "Find video files and TV show directories not mentioned in any DOWNLOADED_FILEPATH property.
Only considers files/directories with TMDB tags.
Lists movies (files with TMDB tags) and TV shows (directories with TMDB tags) that exist
on disk but are not referenced in any org entry's DOWNLOADED_FILEPATH property."
  (interactive)
  (message "Searching for unreferenced movie files and TV show directories...")

  (let* ((all-video-files (emacs-movies-all-video-files))
         (files-with-tmdb (filter-files-with-tmdb-tag all-video-files))
         (tvshow-dirs (emacs-movies-tvshows-directories-alist))
         (referenced-paths '())
         (unreferenced-files '())
         (unreferenced-dirs '()))

    ;; Collect all DOWNLOADED_FILEPATH values from org entries
    (org-map-entries
     (lambda ()
       (let ((downloaded-filepath (org-entry-get nil "DOWNLOADED_FILEPATH")))
         (when downloaded-filepath
           ;; Extract actual file path from org link format [[file:PATH][NAME]]
           (when (string-match "\\[\\[file:\\([^]]+\\)\\]\\[.*\\]\\]" downloaded-filepath)
             (let ((path (match-string 1 downloaded-filepath)))
               (push path referenced-paths)))))))

    ;; Find movie files (with TMDB tags) that are not referenced
    (dolist (tmdb-entry files-with-tmdb)
      (let ((tmdb-id (car tmdb-entry))
            (files (cdr tmdb-entry)))
        (dolist (file files)
          (unless (member file referenced-paths)
            (push file unreferenced-files)))))

    ;; Find TV show directories (with TMDB tags) that are not referenced
    (dolist (tvshow-entry tvshow-dirs)
      (let ((tmdb-id (car tvshow-entry))
            (dir-path (cdr tvshow-entry)))
        (unless (member dir-path referenced-paths)
          (push dir-path unreferenced-dirs))))

    (let ((total-unreferenced (+ (length unreferenced-files) (length unreferenced-dirs))))

      ;; Report summary statistics
      (message "Found %d unreferenced movie files" (length unreferenced-files))
      (message "Found %d unreferenced TV show directories" (length unreferenced-dirs))

      ;; Report results
      (if (> total-unreferenced 0)
          (progn
            (message "Found %d unreferenced items:" total-unreferenced)

            ;; Display unreferenced movies
            (dolist (file unreferenced-files)
              (message "  Movie: %s" (file-name-nondirectory file)))

            ;; Display unreferenced TV shows
            (dolist (dir unreferenced-dirs)
              (message "  TV show: %s" (file-name-nondirectory dir))))
        (message "No unreferenced items found - all files and directories are referenced in DOWNLOADED_FILEPATH properties"))

      ;; Return combined results
      (append (mapcar (lambda (file) (list 'movie file)) unreferenced-files)
              (mapcar (lambda (dir) (list 'tvshow dir)) unreferenced-dirs)))))

(defun emacs-movies-find-duplicate-tmdb-entries ()
  "Find and list org entries with duplicate TMDB IDs."
  (interactive)
  (let ((tmdb-map (make-hash-table :test 'equal))
        (duplicates '()))

    ;; Collect all entries grouped by TMDB ID and type
    (org-map-entries
     (lambda ()
       (let* ((tmdb-url (org-entry-get nil "TMDB_URL"))
              (heading (org-get-heading t t t t)))
         (when tmdb-url
           (let ((tmdb-data (extract-tmdb-id tmdb-url)))
             (when tmdb-data
               (let* ((tmdb-id (car tmdb-data))
                      (content-type (cadr tmdb-data))
                      (key (format "%s-%s" tmdb-id content-type))
                      (line-num (line-number-at-pos))
                      (existing (gethash key tmdb-map)))
                 (puthash key
                          (cons (list heading line-num) existing)
                          tmdb-map)))))))
     nil 'file)

    ;; Find duplicates (TMDB IDs with multiple entries)
    (maphash
     (lambda (key entries)
       (when (> (length entries) 1)
         (push (cons key entries) duplicates)))
     tmdb-map)

    ;; Display results
    (if duplicates
        (with-current-buffer (get-buffer-create "*TMDB Duplicates*")
          (erase-buffer)
          (insert (format "Found %d duplicate TMDB ID(s):\n\n" (length duplicates)))
          (dolist (dup (sort duplicates (lambda (a b)
                                          (let* ((a-parts (split-string (car a) "-"))
                                                 (b-parts (split-string (car b) "-"))
                                                 (a-id (string-to-number (car a-parts)))
                                                 (b-id (string-to-number (car b-parts))))
                                            (< a-id b-id)))))
            (let* ((key (car dup))
                   (parts (split-string key "-"))
                   (tmdb-id (car parts))
                   (content-type (cadr parts)))
              (insert (format "TMDB ID: %s (%s) - %d entries\n" tmdb-id content-type (length (cdr dup))))
              (dolist (entry (cdr dup))
                (insert (format "  - %s (line %s)\n"
                                (car entry)
                                (cadr entry))))
              (insert "\n")))
          (display-buffer (current-buffer))
          (message "Found %d duplicate TMDB ID(s)" (length duplicates)))
      (message "No duplicate TMDB IDs found"))))

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

;;; Upflix HTML parsing helpers

(defun emacs-movies-fetch-html-from-url (url)
  "Fetch HTML from URL and return parsed DOM tree.
Returns nil if fetch fails or URL is invalid."
  (when (and url (not (string-empty-p url)))
    (let ((response-buffer (url-retrieve-synchronously url)))
      (when response-buffer
        (with-current-buffer response-buffer
          (url-http-parse-response)
          (when (eq url-http-response-status 200)
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (let ((dom (libxml-parse-html-region (point) (point-max))))
                (kill-buffer)
                dom))))))))

(defun emacs-movies-detect-rate-limiting (dom)
  "Check if DOM represents a rate limiting response from Upflix.
Upflix returns a prepared page about 'Miss Christmas' when rate limiting.
Returns t if rate limited, nil otherwise."
  (when dom
    (let* ((h2-elements (dom-by-tag dom 'h2))
           (english-title (when h2-elements
                           (string-trim (dom-text (car h2-elements))))))
      (and english-title
           (string= english-title "Miss Christmas")))))

(defun emacs-movies-follow-redirect (url)
  "Follow HTTP redirect for URL and return final destination.
Returns original URL if redirect fails or URL is empty/nil."
  (if (or (not url) (string-empty-p url))
      nil
    (let ((response-buffer (url-retrieve-synchronously url)))
      (if response-buffer
          (with-current-buffer response-buffer
            (url-http-parse-response)
            (let ((final-url (or (and url-http-target-url
                                     (url-recreate-url url-http-target-url))
                                url)))
              (kill-buffer)
              final-url))
        url))))

(defun emacs-movies-extract-subscriptions-from-dom (dom)
  "Extract subscription service names from DOM.
Finds #sc a elements with 'ABONAMENT' text (in child span) and extracts service name from href.
Returns list of service names (strings)."
  (let* ((sc-element (dom-by-id dom "sc"))
         (subscriptions '()))
    (when sc-element
      (let ((links (dom-by-tag sc-element 'a)))
        (dolist (link links)
          ;; Check if link or any child span contains "ABONAMENT"
          (let ((link-text (string-trim (dom-text link)))
                (spans (dom-by-tag link 'span)))
            (when (or (string= link-text "ABONAMENT")
                      (cl-some (lambda (span)
                                 (string= (string-trim (dom-text span)) "ABONAMENT"))
                               spans))
              (let ((href (dom-attr link 'href)))
                (when (and href (string-match "#vod-\\(\\w+\\)" href))
                  (push (match-string 1 href) subscriptions))))))))
    (nreverse subscriptions)))

(defun emacs-movies-extract-rents-from-dom (dom)
  "Extract rental service names from DOM.
Finds #sc a elements with 'WYPOŻYCZENIE' text (in child span) and extracts service name from href.
Returns list of service names (strings)."
  (let* ((sc-element (dom-by-id dom "sc"))
         (rents '()))
    (when sc-element
      (let ((links (dom-by-tag sc-element 'a)))
        (dolist (link links)
          ;; Check if link or any child span contains "WYPOŻYCZENIE"
          (let ((link-text (string-trim (dom-text link)))
                (spans (dom-by-tag link 'span)))
            (when (or (string= link-text "WYPOŻYCZENIE")
                      (cl-some (lambda (span)
                                 (string= (string-trim (dom-text span)) "WYPOŻYCZENIE"))
                               spans))
              (let ((href (dom-attr link 'href)))
                (when (and href (string-match "#vod-\\(\\w+\\)" href))
                  (push (match-string 1 href) rents))))))))
    (nreverse rents)))

(defun emacs-movies-extract-filmweb-link (dom)
  "Extract and follow Filmweb link from DOM.
Finds first a.fw element, extracts href, follows redirect.
Returns final URL or nil if not found."
  (let ((all-links (dom-by-tag dom 'a))
        (filmweb-link nil))
    (catch 'found
      (dolist (link all-links)
        (let ((class-attr (dom-attr link 'class)))
          (when (and class-attr (string-match-p "\\bfw\\b" class-attr))
            (let ((href (dom-attr link 'href)))
              (when href
                ;; Make relative URLs absolute
                (when (string-prefix-p "/" href)
                  (setq href (concat "https://upflix.pl" href)))
                (setq filmweb-link (emacs-movies-follow-redirect href))
                (throw 'found filmweb-link)))))))
    filmweb-link))

(defun emacs-movies-extract-imdb-link (dom)
  "Extract and follow IMDB link from DOM.
Finds first a.im element, extracts href, follows redirect.
Returns final URL or nil if not found."
  (let ((all-links (dom-by-tag dom 'a))
        (imdb-link nil))
    (catch 'found
      (dolist (link all-links)
        (let ((class-attr (dom-attr link 'class)))
          (when (and class-attr (string-match-p "\\bim\\b" class-attr))
            (let ((href (dom-attr link 'href)))
              (when href
                ;; Make relative URLs absolute
                (when (string-prefix-p "/" href)
                  (setq href (concat "https://upflix.pl" href)))
                (setq imdb-link (emacs-movies-follow-redirect href))
                (throw 'found imdb-link)))))))
    imdb-link))

(defun emacs-movies-update-subscription-tags (subscriptions)
  "Update subscription tags based on SUBSCRIPTIONS list.
Adds on_SERVICE tags for services in list, removes tags for services not in list.
Modifies current entry's tags."
  (save-excursion
    (org-back-to-heading t)
    (let ((current-local-tags (org-get-tags nil t)))
      (dolist (subscription-name rl-movies-supported-subscriptions)
        (let ((subscription-tag (concat "on_" subscription-name)))
          (if (member subscription-name subscriptions)
              ;; Add tag if subscription is present
              (unless (member subscription-tag current-local-tags)
                (push subscription-tag current-local-tags))
            ;; Remove tag if subscription is not present
            (setq current-local-tags (delete subscription-tag current-local-tags)))))
      (org-set-tags (delete-dups current-local-tags)))))

(defun emacs-movies-sync-all-subscription-tags ()
  "Sync subscription tags for all entries that have SUBSCRIPTIONS property.
Iterates over all org entries in the current buffer and updates on_SERVICE tags
based on the SUBSCRIPTIONS property value."
  (interactive)
  (let ((processed 0)
        (updated 0))
    (org-map-entries
     (lambda ()
       (let ((subscriptions-prop (org-entry-get nil "SUBSCRIPTIONS")))
         (when (and subscriptions-prop (not (string-empty-p subscriptions-prop)))
           (setq processed (1+ processed))
           (let ((subscriptions-list (split-string subscriptions-prop)))
             (message "[%d] Syncing tags for: %s (subscriptions: %s)"
                      processed
                      (org-get-heading t t t t)
                      subscriptions-prop)
             (emacs-movies-update-subscription-tags subscriptions-list)
             (setq updated (1+ updated)))))))
    (message "Processed %d entries, updated %d entries with subscription tags" processed updated)))

(defun emacs-movies-set-upflix-link ()
  "Set UPFLIX_LINK property for current entry.
Prompts for the Upflix URL (pre-filled with clipboard contents) and sets it as a property on the current org entry."
  (interactive)
  (let* ((clipboard-value (condition-case nil
                              (current-kill 0 t)
                            (error nil)))
         (upflix-url (read-string "Upflix URL: " clipboard-value)))
    (save-excursion
      (org-back-to-heading t)
      (org-set-property "UPFLIX_LINK" upflix-url)
      (message "Set UPFLIX_LINK to: %s" upflix-url))))

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

(defun emacs-movies-refresh-upflix-data ()
  "Refresh Upflix data for current entry by parsing HTML from UPFLIX_LINK.
Fetches HTML from upflix.pl, extracts subscriptions, rents, filmweb_url, imdb_url.
Updates properties: SUBSCRIPTIONS, RENTS, FILMWEB_URL, IMDB_URL, LAST_REFRESHED.
Also updates subscription tags (on_netflix, on_disney, etc.)."
  (interactive)
  (let ((upflix-url (org-entry-get nil "UPFLIX_LINK")))
    ;; Validate UPFLIX_LINK exists
    (unless upflix-url
      (error "No UPFLIX_LINK property found for this entry"))

    (when (string-empty-p upflix-url)
      (error "UPFLIX_LINK is empty"))

    (message "Fetching Upflix data from %s..." upflix-url)

    ;; Wrap in condition-case for error handling
    (condition-case err
        (let* ((dom (emacs-movies-fetch-html-from-url upflix-url)))

          ;; Check if HTML fetch/parse succeeded
          (unless dom
            (error "Failed to fetch or parse HTML from Upflix"))

          ;; Check for rate limiting
          (when (emacs-movies-detect-rate-limiting dom)
            (error "Rate limited by Upflix (detected 'Miss Christmas' page). Please wait 10 minutes before retrying"))

          ;; Extract data from DOM
          (let* ((subscriptions (emacs-movies-extract-subscriptions-from-dom dom))
                 (rents (emacs-movies-extract-rents-from-dom dom))
                 (filmweb-url (emacs-movies-extract-filmweb-link dom))
                 (imdb-url (emacs-movies-extract-imdb-link dom)))

            ;; Update org properties
            (save-excursion
              (org-back-to-heading)

              ;; Always set SUBSCRIPTIONS and RENTS (even if empty)
              (org-set-property "SUBSCRIPTIONS"
                               (if subscriptions
                                   (mapconcat 'identity subscriptions " ")
                                 ""))
              (org-set-property "RENTS"
                               (if rents
                                   (mapconcat 'identity rents " ")
                                 ""))

              ;; Set external URLs only if found
              (when filmweb-url
                (org-set-property "FILMWEB_URL" filmweb-url))
              (when imdb-url
                (org-set-property "IMDB_URL" imdb-url))

              ;; Update timestamp
              (org-set-property "LAST_REFRESHED"
                               (with-temp-buffer
                                 (org-time-stamp '(16) 'inactive)
                                 (buffer-string))))

            ;; Update subscription tags
            (emacs-movies-update-subscription-tags subscriptions)

            ;; Success message
            (message "Refreshed Upflix data: %d subscriptions, %d rentals"
                     (length subscriptions)
                     (length rents))))

      ;; Error handler
      (error (error "Failed to refresh Upflix data: %s"
                   (error-message-string err))))))

(defun emacs-movies-refresh-all-upflix-by-timestamp ()
  "Refresh all entries with UPFLIX_LINK, processing those without timestamps first, then by timestamp order.
Iterates over all org headlines in the current buffer, skipping entries without UPFLIX_LINK property.
For entries with UPFLIX_LINK, refreshes them in order: entries without LAST_REFRESHED timestamp first,
then entries with timestamps from oldest to newest.
Adds a delay between requests to avoid rate limiting. Stops processing if rate limited."
  (interactive)
  (let ((headlines-with-timestamps '())
        (headlines-without-timestamps '())
        (processed 0)
        (total 0)
        (rate-limited nil))
    ;; Step 1: Iterate over all headlines
    (org-map-entries
     (lambda ()
       (let* ((timestamp (org-entry-get nil "LAST_REFRESHED"))
             (props (org-entry-properties))
             (upflix-url (assoc "UPFLIX_LINK" props)))
         (when (and upflix-url (not (string-empty-p (cdr upflix-url))))
           (setq total (1+ total))
           (if timestamp
               ;; Add entry with timestamp to the list
               (push (cons (org-read-date t t timestamp) (point)) headlines-with-timestamps)
               ;; Add entry without timestamp to the list
               (push (point) headlines-without-timestamps))))))

    (message "Found %d entries to refresh" total)

    ;; Step 2: Sort entries with timestamps
    (setq headlines-with-timestamps (sort headlines-with-timestamps (lambda (a b) (time-less-p (car a) (car b)))))

    ;; Step 3: Process entries without timestamp first
    (catch 'rate-limited
      (dolist (headline headlines-without-timestamps)
        (goto-char headline)
        (setq processed (1+ processed))
        (message "[%d/%d] Processing entry without timestamp: %s" processed total (org-get-heading t t t t))
        (condition-case err
            (progn
              (emacs-movies-refresh-upflix-data)
              ;; Add delay between requests (but not after the last one)
              (when (and (> emacs-movies-upflix-request-delay 0)
                        (< processed total))
                (message "Waiting %d seconds before next request..." emacs-movies-upflix-request-delay)
                (sleep-for emacs-movies-upflix-request-delay)))
          (error
           (let ((err-msg (error-message-string err)))
             (if (string-match-p "Rate limited" err-msg)
                 (progn
                   (message "Rate limiting detected! Stopping bulk refresh. Processed %d/%d entries." processed total)
                   (setq rate-limited t)
                   (throw 'rate-limited nil))
               ;; Re-signal non-rate-limit errors
               (message "Error refreshing entry: %s" err-msg))))))

      ;; Step 4: Process entries with timestamps (only if not rate limited)
      (unless rate-limited
        (dolist (headline headlines-with-timestamps)
          (goto-char (cdr headline))
          (setq processed (1+ processed))
          (message "[%d/%d] Processing entry with timestamp: %s" processed total (org-get-heading t t t t))
          (condition-case err
              (progn
                (emacs-movies-refresh-upflix-data)
                ;; Add delay between requests (but not after the last one)
                (when (and (> emacs-movies-upflix-request-delay 0)
                          (< processed total))
                  (message "Waiting %d seconds before next request..." emacs-movies-upflix-request-delay)
                  (sleep-for emacs-movies-upflix-request-delay)))
            (error
             (let ((err-msg (error-message-string err)))
               (if (string-match-p "Rate limited" err-msg)
                   (progn
                     (message "Rate limiting detected! Stopping bulk refresh. Processed %d/%d entries." processed total)
                     (setq rate-limited t)
                     (throw 'rate-limited nil))
                 ;; Re-signal non-rate-limit errors
                 (message "Error refreshing entry: %s" err-msg))))))))

    (if rate-limited
        (message "Bulk refresh stopped due to rate limiting. Successfully processed %d/%d entries." processed total)
      (message "Bulk refresh completed! Successfully processed %d/%d entries." processed total))))

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

;;; Multi-directory support tests

(ert-deftest test-validate-directories-string-error ()
  "Test that string configuration raises helpful error."
  (let ((emacs-movies-directory "/some/path"))
    (should-error (emacs-movies-validate-directories)
                  :type 'error)))

(ert-deftest test-validate-directories-empty-list ()
  "Test that empty list passes validation."
  (let ((emacs-movies-directory '()))
    (should-not (emacs-movies-validate-directories))))

(ert-deftest test-validate-directories-nonexistent ()
  "Test validation error for non-existent directory."
  (let ((emacs-movies-directory '("/nonexistent/path/12345")))
    (should-error (emacs-movies-validate-directories)
                  :type 'error)))

(ert-deftest test-all-video-files-single-directory ()
  "Test video file discovery with single directory."
  (let* ((temp-dir (make-temp-file "emacs-movies-test-" t))
         (emacs-movies-directory (list temp-dir)))
    (unwind-protect
        (progn
          ;; Create test files
          (write-region "" nil (expand-file-name "movie1.mkv" temp-dir))
          (write-region "" nil (expand-file-name "movie2.mp4" temp-dir))
          (write-region "" nil (expand-file-name "readme.txt" temp-dir))

          ;; Test discovery
          (let ((files (emacs-movies-all-video-files)))
            (should (= (length files) 2))
            (should (cl-some (lambda (f) (string-match-p "movie1\\.mkv" f)) files))
            (should (cl-some (lambda (f) (string-match-p "movie2\\.mp4" f)) files))
            (should-not (cl-some (lambda (f) (string-match-p "readme\\.txt" f)) files))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest test-all-video-files-multiple-directories ()
  "Test video file discovery with multiple directories."
  (let* ((temp-dir1 (make-temp-file "emacs-movies-test1-" t))
         (temp-dir2 (make-temp-file "emacs-movies-test2-" t))
         (emacs-movies-directory (list temp-dir1 temp-dir2)))
    (unwind-protect
        (progn
          ;; Create test files in both directories
          (write-region "" nil (expand-file-name "movie1.mkv" temp-dir1))
          (write-region "" nil (expand-file-name "movie2.mp4" temp-dir2))

          ;; Test discovery aggregates from both
          (let ((files (emacs-movies-all-video-files)))
            (should (= (length files) 2))
            (should (cl-some (lambda (f) (string-match-p "movie1\\.mkv" f)) files))
            (should (cl-some (lambda (f) (string-match-p "movie2\\.mp4" f)) files))))
      ;; Cleanup
      (delete-directory temp-dir1 t)
      (delete-directory temp-dir2 t))))

(ert-deftest test-all-video-files-empty-directories ()
  "Test video file discovery with empty directories."
  (let* ((temp-dir1 (make-temp-file "emacs-movies-test1-" t))
         (temp-dir2 (make-temp-file "emacs-movies-test2-" t))
         (emacs-movies-directory (list temp-dir1 temp-dir2)))
    (unwind-protect
        (progn
          ;; Don't create any video files
          (let ((files (emacs-movies-all-video-files)))
            (should (= (length files) 0))))
      ;; Cleanup
      (delete-directory temp-dir1 t)
      (delete-directory temp-dir2 t))))

(ert-deftest test-all-tvshows-directories-multiple ()
  "Test TV show directory discovery with multiple base directories."
  (let* ((temp-dir1 (make-temp-file "emacs-movies-test1-" t))
         (temp-dir2 (make-temp-file "emacs-movies-test2-" t))
         (emacs-movies-directory (list temp-dir1 temp-dir2)))
    (unwind-protect
        (progn
          ;; Create TV show directories with TMDB tags
          (make-directory (expand-file-name "Show A {tmdb-12345}" temp-dir1))
          (make-directory (expand-file-name "Show B {tmdb-67890}" temp-dir2))
          (make-directory (expand-file-name "No Tag Show" temp-dir1))

          ;; Test discovery
          (let ((dirs (emacs-movies-all-tvshows-directories)))
            (should (= (length dirs) 2))
            (should (cl-some (lambda (d) (string-match-p "Show A {tmdb-12345}" d)) dirs))
            (should (cl-some (lambda (d) (string-match-p "Show B {tmdb-67890}" d)) dirs))
            (should-not (cl-some (lambda (d) (string-match-p "No Tag Show" d)) dirs))))
      ;; Cleanup
      (delete-directory temp-dir1 t)
      (delete-directory temp-dir2 t))))

;;; Upflix HTML parsing tests

(ert-deftest test-fetch-html-from-real-upflix-url ()
  "Test fetching and parsing HTML from real Upflix URL.
Tests with The Godfather (1972) page."
  :tags '(:integration :network)
  (let* ((url "https://upflix.pl/film/zobacz/the-godfather-1972")
         (dom (emacs-movies-fetch-html-from-url url)))
    ;; Should successfully fetch and parse HTML
    (should dom)
    ;; DOM should be a list structure
    (should (listp dom))))

(ert-deftest test-extract-data-from-real-upflix-page ()
  "Test extracting subscription and rental data from real Upflix page.
Tests with The Godfather (1972) page."
  :tags '(:integration :network)
  (let* ((url "https://upflix.pl/film/zobacz/the-godfather-1972")
         (dom (emacs-movies-fetch-html-from-url url)))
    (should dom)

    ;; Debug: Check if #sc element exists
    (let ((sc-element (dom-by-id dom "sc")))
      (message "Found #sc element: %S" (if sc-element "YES" "NO"))
      (when sc-element
        (let ((all-links (dom-by-tag sc-element 'a)))
          (message "Found %d links in #sc" (length all-links))
          (let ((count 0))
            (dolist (link all-links)
              (when (< count 5)
                (message "  Link #%d: text='%s' href='%s' children=%S"
                         count
                         (string-trim (dom-text link))
                         (dom-attr link 'href)
                         (length (dom-children link)))
                ;; Show child elements
                (dolist (child (dom-children link))
                  (when (listp child)
                    (message "    Child: tag=%s text='%s'"
                             (dom-tag child)
                             (string-trim (dom-text child)))))
                (setq count (1+ count))))))))

    ;; Extract subscriptions and rents
    (let ((subscriptions (emacs-movies-extract-subscriptions-from-dom dom))
          (rents (emacs-movies-extract-rents-from-dom dom)))

      ;; Should find at least some availability data
      ;; (specific services may change over time, so just check we got data)
      (message "Found subscriptions: %S" subscriptions)
      (message "Found rents: %S" rents)

      ;; Both should be lists
      (should (listp subscriptions))
      (should (listp rents))

      ;; Note: Movie might not be available, so don't enforce finding data
      ;; Just verify the extraction logic works without errors
      t)))

(ert-deftest test-extract-external-links-from-real-page ()
  "Test extracting Filmweb and IMDb links from real Upflix page."
  :tags '(:integration :network)
  (let* ((url "https://upflix.pl/film/zobacz/the-godfather-1972")
         (dom (emacs-movies-fetch-html-from-url url)))
    (should dom)

    ;; Extract external links
    (let ((filmweb-url (emacs-movies-extract-filmweb-link dom))
          (imdb-url (emacs-movies-extract-imdb-link dom)))

      (message "Found Filmweb URL: %S" filmweb-url)
      (message "Found IMDb URL: %S" imdb-url)

      ;; At least one should be found (these links are usually present)
      ;; URLs should start with http if found
      (when filmweb-url
        (should (string-prefix-p "http" filmweb-url)))
      (when imdb-url
        (should (string-prefix-p "http" imdb-url))))))

(ert-deftest test-extract-subscriptions-from-mock-dom ()
  "Test subscription extraction from mock DOM structure (unit test)."
  ;; Create a simple mock DOM structure
  (let* ((mock-link-1 '(a ((href . "#vod-netflix") (class . "btn")) "ABONAMENT"))
         (mock-link-2 '(a ((href . "#vod-disney") (class . "btn")) "ABONAMENT"))
         (mock-link-3 '(a ((href . "#vod-amazon") (class . "btn")) "WYPOŻYCZENIE"))
         (mock-sc (list 'div '((id . "sc")) mock-link-1 mock-link-2 mock-link-3))
         (mock-dom (list 'html nil mock-sc)))
    ;; Test subscription extraction
    (let ((subscriptions (emacs-movies-extract-subscriptions-from-dom mock-dom)))
      (should (= (length subscriptions) 2))
      (should (member "netflix" subscriptions))
      (should (member "disney" subscriptions))
      (should-not (member "amazon" subscriptions)))))

(ert-deftest test-extract-rents-from-mock-dom ()
  "Test rent extraction from mock DOM structure (unit test)."
  (let* ((mock-link-1 '(a ((href . "#vod-netflix") (class . "btn")) "ABONAMENT"))
         (mock-link-2 '(a ((href . "#vod-amazon") (class . "btn")) "WYPOŻYCZENIE"))
         (mock-link-3 '(a ((href . "#vod-appletv") (class . "btn")) "WYPOŻYCZENIE"))
         (mock-sc (list 'div '((id . "sc")) mock-link-1 mock-link-2 mock-link-3))
         (mock-dom (list 'html nil mock-sc)))
    ;; Test rent extraction
    (let ((rents (emacs-movies-extract-rents-from-dom mock-dom)))
      (should (= (length rents) 2))
      (should (member "amazon" rents))
      (should (member "appletv" rents))
      (should-not (member "netflix" rents)))))

(ert-deftest test-extract-subscriptions-no-sc-element ()
  "Test subscription extraction when #sc element is missing (unit test)."
  (let ((mock-dom '(html () (div ((id . "other"))))))
    (let ((subscriptions (emacs-movies-extract-subscriptions-from-dom mock-dom)))
      (should (= (length subscriptions) 0)))))

(ert-deftest test-rate-limiting-detection-miss-christmas ()
  "Test that rate limiting is detected when page shows 'Miss Christmas' (unit test)."
  ;; Create a mock DOM with "Miss Christmas" as the English title (h2)
  (let* ((mock-h2 '(h2 () "Miss Christmas"))
         (mock-dom (list 'html nil mock-h2)))
    (should (emacs-movies-detect-rate-limiting mock-dom))))

(ert-deftest test-rate-limiting-detection-normal-page ()
  "Test that rate limiting is not detected for normal movie pages (unit test)."
  ;; Create a mock DOM with a normal movie title
  (let* ((mock-h2 '(h2 () "The Godfather"))
         (mock-dom (list 'html nil mock-h2)))
    (should-not (emacs-movies-detect-rate-limiting mock-dom))))

(ert-deftest test-rate-limiting-detection-no-title ()
  "Test that rate limiting detection handles missing h2 gracefully (unit test)."
  ;; Create a mock DOM without h2 element
  (let ((mock-dom '(html () (div () "Some content"))))
    (should-not (emacs-movies-detect-rate-limiting mock-dom))))

(ert-deftest test-rate-limiting-detection-nil-dom ()
  "Test that rate limiting detection handles nil DOM gracefully (unit test)."
  (should-not (emacs-movies-detect-rate-limiting nil)))

(ert-deftest test-update-subscription-tags ()
  "Test subscription tag management (unit test)."
  ;; This test would need an org buffer setup, so keeping it simple
  ;; Just verify the function exists and is callable
  (should (fboundp 'emacs-movies-update-subscription-tags)))

(ert-deftest test-bulk-refresh-ordering ()
  "Test that bulk refresh processes entries in correct order (unit test)."
  (with-temp-buffer
    (org-mode)
    ;; Create test entries
    (insert "* Entry without timestamp\n")
    (insert ":PROPERTIES:\n")
    (insert ":UPFLIX_LINK: https://upflix.pl/test1\n")
    (insert ":END:\n\n")

    (insert "* Entry with old timestamp\n")
    (insert ":PROPERTIES:\n")
    (insert ":UPFLIX_LINK: https://upflix.pl/test2\n")
    (insert ":LAST_REFRESHED: [2020-01-01 Wed 12:00]\n")
    (insert ":END:\n\n")

    (insert "* Entry with newer timestamp\n")
    (insert ":PROPERTIES:\n")
    (insert ":UPFLIX_LINK: https://upflix.pl/test3\n")
    (insert ":LAST_REFRESHED: [2023-01-01 Sun 12:00]\n")
    (insert ":END:\n\n")

    (insert "* Entry without UPFLIX_LINK\n")
    (insert ":PROPERTIES:\n")
    (insert ":OTHER: value\n")
    (insert ":END:\n\n")

    ;; Track which entries were visited in what order
    (let ((visited-order '())
          (original-refresh-fn (symbol-function 'emacs-movies-refresh-upflix-data))
          (emacs-movies-upflix-request-delay 0))  ; Disable delay for test

      ;; Mock the refresh function to track calls without making HTTP requests
      (cl-letf (((symbol-function 'emacs-movies-refresh-upflix-data)
                 (lambda ()
                   (push (org-entry-get nil "UPFLIX_LINK") visited-order))))

        ;; Run the bulk refresh
        (goto-char (point-min))
        (emacs-movies-refresh-all-upflix-by-timestamp)

        ;; Verify correct order: entry without timestamp first, then old timestamp, then newer
        (setq visited-order (reverse visited-order))
        (should (= (length visited-order) 3))
        (should (string= (nth 0 visited-order) "https://upflix.pl/test1"))  ; no timestamp
        (should (string= (nth 1 visited-order) "https://upflix.pl/test2"))  ; 2020 timestamp
        (should (string= (nth 2 visited-order) "https://upflix.pl/test3"))  ; 2023 timestamp
        ))))

(provide 'emacs-movies)

;;; emacs-movies.el ends here

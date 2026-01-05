# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

<!-- OPENSPEC:START -->
# OpenSpec Instructions

These instructions are for AI assistants working in this project.

Always open `@/openspec/AGENTS.md` when the request:
- Mentions planning or proposals (words like proposal, spec, change, plan)
- Introduces new capabilities, breaking changes, architecture shifts, or big performance/security work
- Sounds ambiguous and you need the authoritative spec before coding

Use `@/openspec/AGENTS.md` to learn:
- How to create and apply change proposals
- Spec format and conventions
- Project structure and guidelines

Keep this managed block so 'openspec update' can refresh the instructions.

<!-- OPENSPEC:END -->

## Overview

emacs-movies is an Emacs Lisp package for managing a personal movie/TV show database using Org-mode. It integrates with The Movie Database (TMDB) API to fetch and maintain metadata about movies and TV shows, and tracks which content has been downloaded locally.

## Core Architecture

### Data Model

The package uses Org-mode entries with specific properties and tags:

**Properties:**
- `TMDB_URL`: Link to TMDB page (e.g., https://www.themoviedb.org/movie/123 or .../tv/456)
- `TITLE`: Display title (localized)
- `ORIGINAL_TITLE`: Original title from TMDB
- `YEAR`: Release/first air year
- `ORIGINAL_LANGUAGE`: ISO language code
- `DOWNLOADED_FILEPATH`: Org-mode link to downloaded file/directory

**Tags:**
- `movie`: Marks entry as a movie
- `tvshow`: Marks entry as a TV show

**Upflix properties** (from Upflix integration):
- `UPFLIX_LINK`: URL to upflix.pl page for the movie/show
- `SUBSCRIPTIONS`: Space-separated list of streaming services offering subscription access (e.g., "netflix disney")
- `RENTS`: Space-separated list of streaming services offering rental access (e.g., "amazon appletv")
- `FILMWEB_URL`: Link to Filmweb (Polish movie database)
- `IMDB_URL`: Link to IMDb
- `LAST_REFRESHED`: Timestamp of last Upflix data refresh

**Legacy Upflix properties** (older integration, may be updated by legacy functions):
- `TITLE_PL`, `TITLE_EN`, `GENRES`

**Subscription tags** (dynamically managed):
- `on_netflix`, `on_disney`, `on_viaplay`, `on_skyshowtime`, `on_canalplus`, `on_cineman`, `on_appletv`, `on_hbomax`, `on_cdapremium`, `on_amazon`, `on_tvpvod`
- Added/removed automatically based on SUBSCRIPTIONS property

### File Naming Convention

Local video files and TV show directories use TMDB tags in their names:
- Format: `{tmdb-TMDB_ID}` where TMDB_ID is a numeric identifier
- Example: `Movie Name (2020) {tmdb-12345}.mkv`
- Example directory: `TV Show Name {tmdb-67890}/Season 01/`

The `extract-tmdb-id-from-filepath` function parses these tags to match files with org entries.

### Key Subsystems

1. **TMDB Integration** (lines 238-377)
   - Search movies/TV shows: `emacs-movies-search-tmdb`, `emacs-movies-search-tmdb-tv`
   - Fetch by ID: `emacs-movies-get-tmdb-movie-by-id`, `emacs-movies-get-tmdb-tv-by-id`
   - Requires `emacs-movies-tmdb-api-key` to be configured

2. **File Discovery** (lines 35-122)
   - Scan video directory: `emacs-movies-all-video-files`, `emacs-movies-all-tvshows-directories`
   - Filter by TMDB tags: `filter-files-with-tmdb-tag`, `filter-files-without-tmdb-tag`
   - Match files to org entries: `find-files-with-tmdb-id`

3. **Upflix HTML Parsing** (lines 1064-1296)
   - HTML fetching and parsing: `emacs-movies-fetch-html-from-url`
   - Subscription extraction: `emacs-movies-extract-subscriptions-from-dom`
   - Rental service extraction: `emacs-movies-extract-rents-from-dom`
   - External link extraction: `emacs-movies-extract-filmweb-link`, `emacs-movies-extract-imdb-link`
   - Redirect following: `emacs-movies-follow-redirect`
   - Tag management: `emacs-movies-update-subscription-tags`
   - **Main refresh function**: `emacs-movies-refresh-upflix-data` - Fetches HTML from upflix.pl and updates subscription/rental availability data
   - **Note**: This is a focused refresh that updates only SUBSCRIPTIONS, RENTS, FILMWEB_URL, IMDB_URL, and LAST_REFRESHED properties
   - Requires valid UPFLIX_LINK property in org entry

4. **Org-mode Operations** (lines 379-642)
   - Set TMDB metadata: `emacs-movies-set-tmdb-url-from-heading`, `emacs-movies-set-tmdb-by-id`
   - Update org entry: `emacs-movies-update-heading-from-properties`
   - Refresh metadata: `emacs-movies-refresh-tmdb-data`
   - Find downloaded files: `emacs-movies-find-downloaded-file`

5. **Orphan Detection** (lines 691-899)
   - Find files/dirs without org entries: `emacs-movies-find-orphaned-files`
   - Find files/dirs not referenced in DOWNLOADED_FILEPATH: `emacs-movies-find-unreferenced-files`
   - Auto-create entries: `emacs-movies-create-org-entries-for-orphaned-items`

5. **Utility Functions**
   - Filename deobfuscation for fuzzy matching: `deobfuscate-filepath` (removes quality/codec info)
   - Edit distance calculation: `edit-distance` (Levenshtein algorithm)
   - TMDB URL parsing: `extract-tmdb-id`

## Configuration Variables

Required:
- `emacs-movies-tmdb-api-key`: Your TMDB API key (get from themoviedb.org)
- `emacs-movies-directory`: List of root directories for video files (default: '("/media/plex/Wideo"))

Optional:
- `emacs-movies-video-extensions`: List of video file extensions (default: mkv, mp4, avi)
- `emacs-movies-tmdb-language`: Language for TMDB API (default: "pl-PL")
- `emacs-movies-tmdb-base-url`: TMDB API base URL

### Multi-Directory Configuration

The `emacs-movies-directory` variable accepts a list of directory paths, enabling management of video collections across multiple locations:

**Single directory:**
```elisp
(setq emacs-movies-directory '("/media/plex/Wideo"))
```

**Multiple directories:**
```elisp
(setq emacs-movies-directory '("/media/plex/Wideo"
                                "/mnt/archive/Movies"
                                "/mnt/nas/TVShows"))
```

All file scanning functions (`emacs-movies-all-video-files`, `emacs-movies-all-tvshows-directories`) automatically scan across all configured directories and aggregate results.

### Migration from Single Directory

**BREAKING CHANGE:** As of this version, `emacs-movies-directory` must be a list, not a string.

**Before:**
```elisp
(setq emacs-movies-directory "/media/plex/Wideo")
```

**After:**
```elisp
(setq emacs-movies-directory '("/media/plex/Wideo"))
```

If you attempt to use a string value, you'll receive a clear error message with migration instructions.

## Development Workflow

### Running Tests

The package includes ERT (Emacs Lisp Regression Testing) tests at the bottom of the file (lines 1123-1151).

Run tests in Emacs:
```elisp
;; Load the file
(load-file "emacs-movies.el")

;; Run all tests
(ert-run-tests-batch-and-exit)

;; Or run interactively
M-x ert RET t RET
```

Run tests from command line:
```bash
emacs -batch -l emacs-movies.el -f ert-run-tests-batch-and-exit
```

### Loading During Development

After editing:
```elisp
;; Reload the file
M-x load-file RET emacs-movies.el RET

;; Or evaluate buffer
M-x eval-buffer
```

## Common Usage Patterns

### Initial Setup
1. Set `emacs-movies-tmdb-api-key` in your init.el
2. Set `emacs-movies-directory` to point to your video library
3. Create an org file for tracking movies/TV shows

### Adding New Entries
1. Create org heading with movie/TV show name
2. Add :movie: or :tvshow: tag
3. Call `emacs-movies-set-tmdb-url-from-heading` to search TMDB and set metadata
4. Function will automatically find local files with matching TMDB IDs

### Backfilling Metadata
- `emacs-movies-backfill-tmdb-data`: Process all TODO entries lacking TMDB_URL
- `emacs-movies-create-org-entries-for-orphaned-items`: Create entries for downloaded files/dirs without org entries

### Maintenance
- `emacs-movies-refresh-tmdb-data`: Update metadata from TMDB for current entry (titles, year, etc.)
- `emacs-movies-refresh-upflix-data`: Update subscription/rental availability from Upflix for current entry
  - Fetches HTML directly from upflix.pl (no API server required)
  - Updates: SUBSCRIPTIONS, RENTS, FILMWEB_URL, IMDB_URL, LAST_REFRESHED
  - Updates subscription tags (on_netflix, on_disney, etc.)
  - Requires UPFLIX_LINK property to be set
- `emacs-movies-find-orphaned-files`: Identify files with TMDB tags but no org entries
- `emacs-movies-find-unreferenced-files`: Identify files not linked in any DOWNLOADED_FILEPATH

## Code Style Conventions

- Use descriptive function names with `emacs-movies-` prefix
- Internal utilities can omit prefix (e.g., `extract-tmdb-id`, `deobfuscate-filepath`)
- Interactive functions should have docstrings and `(interactive)` marker
- Use `let*` for sequential bindings where each depends on previous
- Error handling: Use `condition-case` for API calls and file operations
- Tag checking: Use `emacs-movies-has-tag-p` which handles inherited tags properly

## Important Implementation Details

### Tag Handling
The `emacs-movies-parse-tags` function (line 69) handles text properties on inherited tags. Always use `emacs-movies-has-tag-p` rather than directly checking `org-get-tags` to ensure inherited tags work correctly.

### URL Extraction
`extract-tmdb-id` returns `(TMDB_ID CONTENT_TYPE)` where CONTENT_TYPE is either `'movie` or `'tvshow` symbol, allowing code to handle both types generically.

### Heading Updates
`emacs-movies-update-heading-from-properties` intelligently formats headings:
- If TITLE == ORIGINAL_TITLE: "TITLE (YEAR)"
- If different: "ORIGINAL_TITLE / TITLE (YEAR)"

This preserves the original language title while showing localized title for reference.

### File Matching
When matching files to org entries, the code uses TMDB IDs from filename tags rather than fuzzy string matching, ensuring reliable association even when filenames are obfuscated with quality/codec information.

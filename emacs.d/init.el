;; Quick reloading reminder:
;; M-: (load user-init-file)

;;; Packaging

; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)
; Display information in messages when there are some problems with loading the package or it's load time is big
(setq use-package-verbose t)
; Allow to upgrade built-in packages, i.e. transient
(setq package-install-upgrade-built-in t)

; Disable "Warning (comp)" from popping up
(setq native-comp-async-report-warnings-errors nil)

; Set the path to authentication file
(setq auth-sources '("~/.authinfo"))

;; Dired
; ???
; (setq-default dired-isearch-filenames "dwim")
; Hide details such as file owner at startup
; (add-hook 'dired-mode-hook 'dired-hide-details-mode)
; Make directories with only one dir, display without a tree
; (require-package 'dired-collapse)
; (add-hook 'dired-mode-hook 'dired-collapse-mode)
; Add subtrees instead of default dired navigation
; (require-package 'dired-subtree)
; (add-hook 'dired-mode-hook
;           (lambda () (local-set-key (kbd "i") 'dired-subtree-toggle)))
; (require-package 'dired-narrow)

;; General options

; Enable highlight on the whole current line
(global-hl-line-mode 1)
; Don't make ~backup~ files
(setq make-backup-files nil)
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/saves/" t)))
(global-unset-key "\M-l") ; that was "downcase word" by default
; Disable built-in VC manager
(setq vc-handled-backends nil)

; Set how many lines are taken from previous page when scrolling by pages
(setq next-screen-context-lines 6)

(setq scroll-preserve-screen-position t)
(setq scroll-step 1)
(setq scroll-margin 1)

; Tabs
(setq tab-bar-mode t)

(use-package transient
             :straight t)

(use-package evil
             :straight t
             :init
             (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
             (setq evil-want-keybinding nil)
             :config
             (evil-mode 1)

             (defvar evil-normal-state-leader-map (make-sparse-keymap)
               "Keymap for \"leader key\" shortcuts.")
             (define-key evil-normal-state-map "," evil-normal-state-leader-map)
             (define-key evil-normal-state-map "Q" "@q")
             (define-key evil-normal-state-map "J" 'scroll-up-command)
             (define-key evil-normal-state-map "K" 'scroll-down-command)
             (define-key evil-normal-state-leader-map "v" 'evil-window-vsplit)
             (define-key evil-normal-state-leader-map "h" 'evil-window-split)
             (define-key evil-normal-state-leader-map "gs" 'magit-status)
             (define-key evil-normal-state-leader-map "gg" 'magit-dispatch-popup)
             (define-key evil-normal-state-leader-map "gb" 'magit-blame)
             (setq
               ;Disable displaying information about evil state
               ;evil-mode-line-format nil

               evil-normal-state-cursor '(box "White")
               evil-insert-state-cursor '(bar "White")
               evil-visual-state-cursor '(box "#F86155")))

(use-package evil-collection
             :after evil
             :straight t
             :config
             (evil-collection-init))

;; Helm
(use-package helm
             :after evil
             :straight t
             :config
             (global-set-key (kbd "M-x") 'helm-M-x)
             (setq helm-buffers-fuzzy-matching t)
             (setq helm-recentf-fuzzy-matching t)
             (setq helm-display-function 'helm-display-buffer-in-own-frame)
             (setq helm-ff-transformer-show-only-basename nil)
             (setq helm-findutils-search-full-path t)
             (define-key evil-normal-state-leader-map "pb" 'helm-mini)
             (define-key evil-normal-state-leader-map "pc" 'helm-M-x)
             )

(use-package helm-ls-git
             :after helm
             :straight t)
;; FZF
(use-package fzf
             :after evil
             :straight t
             :config
             (define-key evil-normal-state-leader-map "ff" 'fzf-git-files)
             )

;; Hybrid-relative line numbering
(use-package nlinum-relative
             :straight t
             :config
             (nlinum-relative-setup-evil)
             (add-hook 'prog-mode-hook 'nlinum-relative-mode)
             (setq nlinum-relative-redisplay-delay 0)
             (setq nlinum-relative-current-symbol ""))

(use-package git-gutter-fringe
             :if window-system
             :after evil
             :straight t
             :config
             ; enable globally
             (global-git-gutter-mode +1)
             ; gutter will be on the right side
             (setq git-gutter-fr:side 'right-fringe)
             (define-key evil-normal-state-map "]n" 'git-gutter:next-hunk)
             (define-key evil-normal-state-map "[n" 'git-gutter:previous-hunk)
             )

(use-package rspec-mode
             :after evil
             :straight t
             :config
             (define-key evil-normal-state-leader-map "tn" 'rspec-verify-single)
             (define-key evil-normal-state-leader-map "tf" 'rspec-verify)
             (define-key evil-normal-state-leader-map "tl" 'rspec-rerun)
             )

(use-package yaml-mode
             :straight t)

(use-package evil-nerd-commenter
             :straight t
             :config
             (define-key evil-normal-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
             (define-key evil-visual-state-map "gc" 'evilnc-comment-or-uncomment-lines)
             )

(use-package magit
             :straight t
             :after transient
             :config
             (add-to-list 'magit-no-confirm 'drop-stashes)
             )

(use-package forge
             :straight t
             :after magit
             :config
             (push
               '("gitlab.silverfin.com" "gitlab.silverfin.com/api/v4" "gitlab.silverfin.com" forge-gitlab-repository)
               forge-alist)
             )

; (use-package evil-magit
;              :after evil magit
;              :config
;              (define-key evil-normal-state-leader-map "gs" 'magit-status)
;              (define-key evil-normal-state-leader-map "gg" 'magit-dispatch-popup)
;              (define-key evil-normal-state-leader-map "gb" 'magit-blame)
;              )



;; JSX mode
(use-package rjsx-mode
             :straight t
             :config
             (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
             )

;; Elm mode
(use-package elm-mode
             :straight t
             :after evil
             :config
             (setq elm-format-on-save t)
             ; Installable by `yarn global add elm-format`
             (setq elm-format-command "/home/swistak35/.yarn/bin/elm-format")
             (setq elm-tags-on-save t)
             (setq elm-tags-exclude-elm-stuff nil)
             (define-key evil-normal-state-leader-map "jj" 'elm-mode-goto-tag-at-point)
             )

(use-package org
             :straight t
             :config
             (require 'org-protocol)
             (unless (server-running-p) (server-start)) ; Required for org-protocol to receive information
             (setq org-agenda-files '("~/notes" "~/pnotes/sf" "~/pnotes/res"))
             (setq org-default-notes-file "~/notes/inbox.org")
             (setq org-log-done t)
             (setq org-agenda-skip-scheduled-if-done t)
             (setq org-agenda-skip-deadline-if-done t)
             (setq org-agenda-skip-unavailable-files t)
             (setq org-priority-start-cycle-with-default nil)
             (setq org-default-priority ?D) ; A for MIT, B for the things for the week, C for the things which are ready to be executed
             (setq org-lowest-priority ?E)
             (setq org-agenda-start-on-weekday nil)
             (setq org-agenda-show-future-repeats nil)
             (setq org-startup-truncated nil) ; enable line wrap
             (add-hook 'org-mode-hook (lambda () (run-with-idle-timer 600 t 'org-save-all-org-buffers))) ; autosave every 10 minutes
             (setq org-todo-keywords
                   '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))
             (org-babel-do-load-languages
               'org-babel-load-languages
               '((emacs-lisp . t)
                 (ruby . t)))
             (global-set-key (kbd "C-c l") #'org-store-link)
             (global-set-key (kbd "C-c a") #'org-agenda)
             (global-set-key (kbd "C-c c") #'org-capture)
             (setq org-capture-templates
                   '(
                     ("m" "Movie" entry (file+headline "~/notes/movies.org" "Inbox")
                      "** TOWATCH %^{Please enter name}%?\n:PROPERTIES:\n:ID: %^{Please enter ID}\n:UPFLIX_LINK:\n:CREATED_AT: %U\n:END:%i\n")
                     ("w" "Web" entry (file+headline "~/notes/inbox.org" "Inbox")
                      "** %:description\n:PROPERTIES:\n:ID: %:foo\n:URL: %:link\n:CREATED_AT: %U\n:END:\n%i\n")
                     ("t" "Task" entry (file+headline "~/notes/inbox.org" "Inbox")
                      "** TODO %i\n")
                     ("e" "Exercise log")
                     ("ep" "Exercise log (Pull-up level 3)" entry (file+olp+datetree "~/notes/exercise-log.org" "Log")
                      "*** Pull-up level 3\n:PROPERTIES:\n:KIND: pull-up level 3\n:BAND: purple\n:SET1: %^{Number of reps for set 1}\n:SET2: %^{Number of reps for set 2}\n:SET3: %^{Number of reps for set 3}\n:END:%i\n"
                      :prepend t)
                     ))
             (add-to-list 'org-modules 'org-habit t)
             (setq org-reverse-note-taking-order t) ; i.e. this is important for refiling to put notes at the top
             (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
             (setq org-refile-use-outline-path t) ; Show the full path for refiling targets
             (setq org-outline-path-complete-in-steps nil) ; Refile in a single go
             (defun my-org-refile-target-verify-function () ; All of this to filter out some of the entries from the org-refile-targets
               "Verify that the refile target is valid."
               (let ((valid-tags '("tickler")))
                 (or
                   (= (nth 0 (org-heading-components)) 1)
                   (and
                     (not (cl-set-exclusive-or (org-get-tags-at) valid-tags))
                     (= (nth 0 (org-heading-components)) 2)))))
             (setq org-refile-target-verify-function 'my-org-refile-target-verify-function)
             (setq org-agenda-custom-commands
                   '(
                     ("X" "Super agenda"
                      (
                       (org-ql-block '(and (todo "TODO")
                                           (priority '= "A"))
                                     ((org-ql-block-header "Najważniejsze")))
                       ; (agenda "" ((org-agenda-span 'day)
                       ;             (org-super-agenda-groups
                       ;               '((:name "Today"
                       ;                        :time-grid t
                       ;                        :date today
                       ;                        :todo "TODAY"
                       ;                        :scheduled today
                       ;                        :order 1)))))
                       (agenda ""
                               (
                                (org-agenda-span 'day)
                                (org-super-agenda-groups
                                  '(
                                    (:discard (:priority "A"))
                                    (:discard (:and (:priority<= "B" :tag ("@workbreak" "@zaudio" "@zfilmem"))))
                                    (:discard (:and (:not (:priority) :tag ("@workbreak" "@zaudio" "@zfilmem"))))
                                    (:discard (:tag "@exercises"))
                                    (:name "Poranek"
                                           :tag "poranek")
                                    (:name "During day"
                                           :face (:append t)
                                           :tag "@duringday")
                                    (:name "Next (work)"
                                           :tag "work")
                                    (:name "Wieczór"
                                           :tag "wieczor")
                                    (:name "Dzisiejsze"
                                           :scheduled today)
                                    (:name "Przypominajki"
                                           :and (:scheduled past :priority<= "C")
                                           :order 101)
                                    (:discard (:tag "@emilka"))
                                    (:discard (:todo "WAITING"))
                                   ))
                                  ))
                       (org-ql-block '(and (todo "TODO")
                                           (priority '= "B")
                                           (not (tags "@emilka"))
                                           (not (scheduled)))
                                     ((org-ql-block-header "Next to grab")))
                       (org-ql-block '(and (priority)
                                           (tags "@cooking"))
                                     ((org-ql-block-header "Gotowanie")))
                       (org-ql-block '(and (todo "WAITING")
                                           (not (tags "@emilka")))
                                     ((org-ql-block-header "Zadania zablokowane")))
                       (org-ql-block '(and (todo)
                                           (not (scheduled))
                                           (tags "@emilka"))
                                     ((org-ql-block-header "Zadania dla Emilki")))
                       (org-ql-block '(and (todo "TODO")
                                           (priority '= "C")
                                           (not (scheduled)))
                                     ((org-ql-block-header "Up next")))
                       (org-ql-block '(and (todo "TODO")
                                           (or (not (scheduled))
                                               (scheduled :to today))
                                           (tags "@zaudio" "@zfilmem"))
                                     ((org-super-agenda-groups
                                        '(
                                          (:name "Istotniejsze"
                                                 :scheduled past
                                                 :scheduled today)
                                         ))
                                      (org-ql-block-header "Zadania do zrobienia na audio")))
                       ))
                     ("T" "Treadmill"
                      (
                       (agenda ""
                               (
                                (org-agenda-span 'day)
                                (org-super-agenda-groups
                                  '(
                                    (:name "W pracy"
                                           :tag "work")
                                    (:discard (:not (:tag "@ontreadmill")))
                                    ))
                                ))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "@ontreadmill")
                                           (not (scheduled)))
                                     ((org-ql-block-header "Zadania")))
                       ))
                     ("W" "Watching"
                      (
                       (org-ql-block '(and (todo "WATCHING")
                                           (or (not (scheduled))
                                               (scheduled :to today)))
                                     ((org-ql-block-header "Aktualnie oglądane")))
                       (org-ql-block '(and (todo "TOWATCH")
                                           (priority '= "A"))
                                     ((org-ql-block-header "Najbliższe do obejrzenia")))
                       (org-ql-block '(and (todo "TOWATCH")
                                           (priority '= "B"))
                                     ((org-ql-block-header "Chcielibyśmy obejrzeć wkrótce")))
                       (org-ql-block '(and (todo "WATCHING")
                                           (scheduled :from today)
                                           (not (scheduled :on today)))
                                     ((org-ql-block-header "Czekamy na odcinek")))
                       (org-ql-block '(and (todo "TOWATCH")
                                           (tags "movie")
                                           (or (not (priority))
                                               (priority '< "B")))
                                     ((org-ql-block-header "Filmy do obejrzenia")))
                       (org-ql-block '(and (todo "TOWATCH")
                                           (not (tags "movie"))
                                           (or (not (priority))
                                               (priority '< "A")))
                                     ((org-ql-block-header "Pozostałe do obejrzenia")))
                       )
                      )
                     ("N" "Agenda and all TODOs, but better"
                      (
                       (org-ql-block '(and (todo "TODO")
                                           (priority '= "A"))
                                     ((org-ql-block-header "Most important things")))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "poranek")
                                           (or (priority '< "A")
                                               (scheduled :to today)))
                                     ((org-ql-block-header "Poranek")))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "duringday")
                                           (or (priority '<= "A")
                                               (scheduled :to today)))
                                     ((org-ql-block-header "Things which need to be done during the day")))
                       (agenda "")
                       (org-ql-block '(and (todo "TODO")
                                           (tags "wieczor")
                                           (or (priority '< "A")
                                               (scheduled :to today)))
                                     ((org-ql-block-header "Wieczór")))
                       (org-ql-block '(and (todo "TODO")
                                           (priority '= "B")
                                           (not (tags "work"))
                                           (not (scheduled)))
                                     ((org-ql-block-header "Up to grab")))
                       (org-ql-block '(and (todo "TODO")
                                           (priority '= "B")
                                           (tags "work")
                                           (not (scheduled)))
                                     ((org-ql-block-header "Up to grab (work)")))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "workbreak")
                                           (or (priority '< "A")
                                               (scheduled :to today)))
                                     ((org-ql-block-header "Things to do during work break")))
                       (org-ql-block '(and (todo "WAITING")
                                           (not (tags "emilka")))
                                           ; (priority '< "A")
                                           ; (not (scheduled)))
                                     ((org-ql-block-header "Things waiting")))
                       (tags-todo "emilka")
                       (org-ql-block '(and (todo "TODO")
                                           (priority '= "C")
                                           (not (scheduled)))
                                     ((org-ql-block-header "Next to grab")))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "quarter")
                                           (not (tags "archived")))
                                     ((org-ql-block-header "Quarter goals")))
                       ;                         (priority '<= "D"))
                       ;                     (not (scheduled)))
                       ;               ((org-ql-block-header "Other tasks")))
                       ))
                     ("x" "Agenda for work"
                      (
                       (org-ql-block '(and (todo "TODO")
                                           (not (tags "work"))
                                           (priority '= "A"))
                                     ((org-ql-block-header "Most important things")))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "work")
                                           (priority '= "A"))
                                     ((org-ql-block-header "Most important things (work)")))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "work")
                                           (tags "current")
                                           (priority '= "B"))
                                     ((org-ql-block-header "Current tasks")))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "work")
                                           (tags "current")
                                           (or (priority '< "B")
                                               (not (priority))))
                                     ((org-ql-block-header "Tasks from current projects")))
                       ; (tags-todo "work+SCHEDULED<\"<now>\""
                       ;            ((org-agenda-overriding-header "Due tasks")))
                       ; (tags-todo "work+DEADLINE\"<+3d\""
                       ;            ((org-agenda-overriding-header "Imminent deadline")))
                       ; (tags-todo "workbreak+SCHEDULED<\"<now>\""
                       ;            ((org-agenda-overriding-header "Things to do during break")))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "workbreak")
                                           (or (priority '< "A")
                                               (scheduled :to today)))
                                     ((org-ql-block-header "Things to do during work break")))
                       ; (agenda "")
                       ; (tags-todo "work+PRIORITY>\"A\""
                                  ; ((org-agenda-overriding-header "Other tasks")
                                  ; (org-agenda-tags-todo-honor-ignore-options t)
                                  ; (org-agenda-todo-ignore-scheduled t)))
                       (org-ql-block '(and (tags "work")
                                           (not (todo "TODO" "DONE"))
                                           (ts)))
                       ))
                     ("v" "For review" (
                       (org-ql-block '(and (todo "TODO")
                                           (tags "quarter")
                                           (not (tags "ARCHIVE")))
                                     ((org-ql-block-header "Cele kwartalne")))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "inbox"))
                                     ((org-ql-block-header "Inbox")))
                       (org-ql-block '(and (todo "TODO")
                                           (not (tags "tickler"))
                                           (not (tags "work"))
                                           (not (tags "ARCHIVE"))
                                           (not (tags "quarter"))
                                           (not (tags "@emilka"))
                                           (not (tags "inbox"))
                                           (not (tags "template"))
                                           (not (scheduled)))
                                     ((org-super-agenda-groups
                                        '((:auto-outline-path)))
                                      (org-ql-block-header "Do zrobienia")))
                       ))
                     ))
             (setq org-agenda-include-diary t))

; These two look great, but org-quick-peek don't work right now
; (use-package quick-peek
;              :straight t)
; (use-package org-quick-peek
;              :after quick-peek
;              :straight (:host github :repo "alphapapa/org-quick-peek" :branch "master"))

(use-package helm-org
             :straight t
             :after org
             :config
             (define-key evil-normal-state-leader-map "po" 'helm-org-agenda-files-headings)
             ; (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
             ; (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
             )

(use-package helm-org-rifle
             :straight t
             :after org)

(use-package org-ql
             :straight t
             :after org)

(use-package org-super-agenda
             :straight t
             :after org
             :config
             (org-super-agenda-mode t)
             ; To fix the j/k manipulation on the headers when viewing super agenda
             ; https://github.com/alphapapa/org-super-agenda/issues/50
             (setq org-super-agenda-header-map (make-sparse-keymap))
             )

(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; for org-roam
; (use-package emacsql-sqlite
;              :straight t)
; (use-package org-roam
;             :straight t
;              :after org
;              :config
;              (require 'org-roam)
;              (require 'org-roam-protocol)
;              (setq org-roam-directory (file-truename "~/notes/roam"))
;              (setq org-roam-capture-templates
;                    '(
;                      ("r" "rdefault" plain "%?"
;                        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
;                                           "#+title: ${title}\n")
;                        :unnarrowed t)
;                      ))
;              (org-roam-db-autosync-mode))

(use-package burly
             :straight t
             :config
             ; (burly-tabs-mode)
             (define-key evil-normal-state-leader-map "pu" 'helm-filtered-bookmarks)
             )

(use-package activities
             :straight t
             :init
             (activities-mode)
             (activities-tabs-mode)
             ;; Prevent `edebug' default bindings from interfering.
             (setq edebug-inhibit-emacs-lisp-mode-bindings t)
             :bind
             (("C-x C-a C-n" . activities-new)
              ;; As resuming is expected to be one of the most commonly used
              ;; commands, this binding is one of the easiest to press.
              ("C-x C-a C-a" . activities-resume)
              ("C-x C-a C-s" . activities-suspend)
              ("C-x C-a C-k" . activities-kill)
              ;; This binding mirrors, e.g. "C-x t RET".
              ("C-x C-a RET" . activities-switch)
              ("C-x C-a g" . activities-revert)
              ("C-x C-a l" . activities-list))
             )

;; Themes
(use-package solarized-theme
             :straight t
             :config
             (setq x-underline-at-descent-line t) ; Underline is more under than usual
             )

(load-theme 'solarized-dark t)

; ;; Highlight in gutter git changes
; (require-package 'diff-hl)
; ; (add-hook 'ruby-mode 'diff-hl-mode)

; ;; Ruby and Rails

; (require-package 'haml-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(list-packages-ext solarized-theme dired-subtree dired-collapse dired-hacks-utils)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

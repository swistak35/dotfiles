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

; Display compilation window, like the one with tests, in a more smart way
(setq split-width-threshold most-positive-fixnum)

; Automatically scroll down the compilation buffer
(setq compilation-scroll-output t)

; Set the path to authentication file
(setq auth-sources '("~/.authinfo"))

; M-x world-clock
(setq world-clock-time-format "%a %d %b %R %Z")
(setq world-clock-list
      '(("Europe/Warsaw" "Warszawa")
	("Europe/Brussels" "Bruksela (Ghent)")
        ("Europe/London" "Londyn")
        ("America/New_York" "NASDAQ/NYSE (Nowy Jork)")))


; ctags
; Needs exuberant-ctags package to be installed
(setq path-to-ctags "/usr/bin/ctags-exuberant")


(add-hook 'before-save-hook 'delete-trailing-whitespace)


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

;(global-set-key (kbd "C-a x") 'kill-current-buffer)
;(global-set-key (kbd "C-a b") 'projectile-previous-project-buffer)
;(global-set-key (kbd "C-a n") 'projectile-previous-project-buffer)

(use-package transient
             :straight t)



; Treesitter
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (ruby-mode . ruby-ts-mode)))

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

(use-package copilot
 	     :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
	     :straight t
	     :config
	     (setq copilot-node-executable "/home/swistak35/.asdf/shims/node")
	     (add-hook 'prog-mode-hook 'copilot-mode)
	     (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
	     (define-key copilot-completion-map (kbd "<backtab>") 'copilot-accept-completion-by-word)

	     ; Needed because
	     ; https://github.com/copilot-emacs/copilot.el/issues/312
	     (add-to-list 'copilot-indentation-alist '(prog-mode 2))
	     (add-to-list 'copilot-indentation-alist '(org-mode 2))
	     (add-to-list 'copilot-indentation-alist '(ruby-mode 2))
	     ;; (add-to-list 'copilot-indentation-alist '(text-mode 2))
	     ;; (add-to-list 'copilot-indentation-alist '(closure-mode 2))
	     ;; (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
	     )

(use-package gptel
             :straight t)


(use-package flycheck
	     :straight t
             :ensure t
	     :config
	     (add-hook 'after-init-hook #'global-flycheck-mode))


;; Helm
(use-package helm
             :after evil
             :straight t
             :config
             (helm-mode)
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
; (use-package helm-ag
;              :after helm
;              :straight t)

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
             (setq git-gutter-fr:side 'left-fringe)
             (define-key evil-normal-state-map "]n" 'git-gutter:next-hunk)
             (define-key evil-normal-state-map "[n" 'git-gutter:previous-hunk)
	     (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
	     (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
	     (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
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

(use-package yaml-pro
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
	     (setopt magit-format-file-function #'magit-format-file-nerd-icons)
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
	     (setq org-log-into-drawer t) ; Causes to log the CLOSED timestamps into the LOGBOOK property, instead of the entry content
             (setq org-agenda-skip-scheduled-if-done t)
             (setq org-agenda-skip-deadline-if-done t)
             (setq org-agenda-skip-unavailable-files t)
	     (setq org-agenda-window-setup 'other-window)
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
	     (defun my/org-capture-maybe-create-id ()
	       (when (org-capture-get :create-id)
		 (org-id-get-create)))
	     (add-hook 'org-capture-mode-hook #'my/org-capture-maybe-create-id)
             (setq org-capture-templates
                   '(
		     ("m" "Movie")
                     ("mu" "Movie from Upflix" entry (file+headline "~/notes/movies.org" "Inbox")
                      "** TOWATCH \n:PROPERTIES:\n:UPFLIX_LINK: %^{Please enter upflix link}\n:CREATED_AT: %U\n:END:%i\n")
                     ("mf" "Movie from Filmweb" entry (file+headline "~/notes/movies.org" "Inbox")
                      "** TOWATCH \n:PROPERTIES:\n:FILMWEB_LINK: %^{Please enter filmweb link}\n:CREATED_AT: %U\n:END:%i\n")
                     ("w" "Web")
                     ("ww" "Web" entry (file+headline "~/notes/bookmarks.org" "Bookmarks processed")
                      "** %:description\n:PROPERTIES:\n:ID: %:foo\n:URL: %:link\n:CREATED_AT: %U\n:END:\n%i\n"
		      :create-id t
		      :prepend t)
                     ("wt" "Task" entry (file+headline "~/notes/inbox.org" "Inbox")
                      "** TODO %:description\n:PROPERTIES:\n:ID: %:foo\n:URL: %:link\n:CREATED_AT: %U\n:END:\n%i\n"
                      :prepend t
		      :create-id t)
                     ("wu" "To Read" entry (file+headline "~/notes/bookmarks.org" "To Read")
                      "** TOREAD %:description\n:PROPERTIES:\n:ID: %:foo\n:URL: %:link\n:CREATED_AT: %U\n:END:\n%i\n"
                      :prepend t
		      :create-id t)
                     ("wr" "Read" entry (file+headline "~/notes/bookmarks.org" "Bookmarks processed")
                      "** READ %:description\n:PROPERTIES:\n:ID: %:foo\n:URL: %:link\n:CREATED_AT: %U\n:END:\n%i\n"
                      :prepend t
		      :create-id t)
                     ("wm" "Movie" entry (file+headline "~/notes/movies.org" "Inbox")
                      "** TOWATCH %:description\n:PROPERTIES:\n:URL: %:link\n:CREATED_AT: %U\n:END:\n%i\n")
                     ("wb" "Book" entry (file+headline "~/notes/books.org" "Inbox")
                      "** TOREAD %:description\n:PROPERTIES:\n:URL: %:link\n:CREATED_AT: %U\n:END:\n%i\n")
                     ("t" "Task" entry (file+headline "~/notes/inbox.org" "Inbox")
                      "** TODO %?\n"
                      :prepend t)
                     ("e" "Exercise log")
                     ("et" "Treadmill walk" entry (file+olp+datetree "~/notes/exercise-log.org" "Log")
                      "*** Treadmill walk\n:PROPERTIES:\n:DATE: %U\n:KIND: treadmill walk\n:DISTANCE: %^{Distance}\n:END:%?\n"
                      :prepend t
                      :time-prompt t)
		     ("ef" "Zestaw od fizjoterapeuty")
		     ("ef4" "Zestaw od fizjoterapeuty nr 4" entry (file+olp+datetree "~/notes/exercise-log.org" "Log")
                      "*** Zestaw od fizjoterapeuty nr 4\n:PROPERTIES:\n:DATE: %U\n:KIND: zestaw od fizjoterapeuty nr 4\n:KLĘK_PODPARTY: 3x15\n:OKRAZANIE_PRZEDMIOTU_GUMA: 3/6\n:PRZYSIADY_GUMA: 2/6\n:WYKROKI: 3x10\n:KOPENHASKIE_PRZEWODZENIE: 20s\n:END:%?\n"
                      :prepend t
                      :time-prompt t)
                     ("ep" "Pull-up level 3" entry (file+olp+datetree "~/notes/exercise-log.org" "Log")
                      "*** Pull-up level 3\n:PROPERTIES:\n:DATE: %U\n:KIND: pull-up level 3\n:BAND: green\n:SET1: %^{Number of reps for set 1}\n:SET2: %^{Number of reps for set 2}\n:SET3: %^{Number of reps for set 3}\n:END:%?\n"
                      :prepend t
                      :time-prompt t)
		     ("ek" "Kettlebell sets")
		     ("ek1" "Kettlebell 1" entry (file+olp+datetree "~/notes/exercise-log.org" "Log")
		      "*** Kettlebell zestaw 1\n:PROPERTIES:\n:DATE: %U\n:KIND: kettlebell 1\n:OVERHEAD_PRESS_WEIGHT: 12\n:HALOS_WEIGHT: 16\n:ROW_WEIGHT: 20\n:HALOS_SET1: \n:OVERHEAD_PRESS_RIGHT_SET1: \n:OVERHEAD_PRESS_LEFT_SET1: \n:ROW_RIGHT_SET1: \n:ROW_LEFT_SET1: \n:HALOS_SET2: \n:OVERHEAD_PRESS_LEFT_SET2: \n:OVERHEAD_PRESS_RIGHT_SET2: \n:ROW_RIGHT_SET2: \n:ROW_LEFT_SET2: \n:HALOS_SET3: \n:OVERHEAD_PRESS_RIGHT_SET3: \n:OVERHEAD_PRESS_LEFT_SET3: \n:ROW_RIGHT_SET3: \n:ROW_LEFT_SET3: \n:END:%?\n"
		      :prepend t
		      :time-prompt t)
                     ))
             (add-to-list 'org-modules 'org-habit t)
             (setq org-reverse-note-order t) ; i.e. this is important for refiling to put notes at the top
             (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
             (setq org-refile-use-outline-path 'file) ; Show the full path for refiling targets
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
                                           (priority '= "A")
					   (not (tags "work")))
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
                                    (:discard (:tag "work"))
                                    (:name "Poranek"
                                           :tag "poranek")
                                    (:name "During day"
                                           :face (:append t)
                                           :tag "@duringday")
                                    (:name "Wieczór"
                                           :tag "wieczor"
					   :order 100)
				    (:name "Codzienne"
					   :tag "daily")
                                    (:name "Dzisiejsze"
                                           :scheduled today)
                                    ;; (:name "Przypominajki"
                                    ;;        :and (:scheduled past :priority<= "C")
                                    ;;        :order 101)
                                    (:discard (:tag "@emilka"))
                                    (:discard (:todo "WAITING"))
                                   ))
                                  ))
                       (org-ql-block '(and (todo "TODO")
                                           (priority '= "B")
                                           (not (tags "@emilka" "work"))
                                           (not (scheduled)))
                                     ((org-ql-block-header "Next to grab")))
                       ;; (org-ql-block '(and (priority)
                       ;;                     (tags "@cooking"))
                       ;;               ((org-ql-block-header "Gotowanie")))
                       (org-ql-block '(and (todo "WAITING")
                                           (not (tags "@emilka" "work")))
                                     ((org-ql-block-header "Zadania zablokowane")))
                       (org-ql-block '(and (todo "TODO")
                                           (priority '= "C")
					   (not (tags "@emilka" "work"))
                                           (not (scheduled)))
                                     ((org-ql-block-header "Up next")))
                       (org-ql-block '(and (todo "TODO")
					   (not (tags "@emilka" "work"))
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
                       (org-ql-block '(and (todo)
                                           (not (scheduled))
                                           (tags "@emilka"))
                                     ((org-ql-block-header "Zadania dla Emilki")))
		       (org-ql-block '(or (and (todo "TODO" "DONE")
					       (closed :on today))
					  ;; Not possible to implement correctly (looking at LAST_REPEAT being on today)
					  ;; because of this: https://github.com/alphapapa/org-ql/issues/192
					  ;; (and (todo "TODO")
					  ;;      (property "LAST_REPEAT")
					  ;;      (ts :on today))
					  )
				     ((org-ql-block-header "Zrobione dzisiaj")))
		       (org-ql-block '(tags "inbox")
				     ((org-ql-block-header "Inbox")))

		       ; Random Quote block
		       ; need to add :sort (random) but as of 0.8 still not implemented
		       ; also need to add "pick 1", but also not implemented
		       ;; (org-ql-block '(tags "quote")
		       ;; 		     ((org-ql-block-header "Quote")))

		       ; Random Rule to remember for yourself
		       ; need to add :sort (random) but as of 0.8 still not implemented
		       ; also need to add "pick 1", but also not implemented
		       (org-ql-block '(tags "rule")
		        		     ((org-ql-block-header "Random Rule of the day")))
		       )
                       )
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
		     ("A" "Audio"
                      (
                       (org-ql-block '(and (todo "TODO")
                                           (tags "@zaudio"))
                                     ((org-super-agenda-groups
                                        '(
					  (:discard (:scheduled future))
					  (:name "Codzienne"
						 :tag "daily")
					  (:name "Najlepiej dziś"
						r:priority "B")
					  (:name "Do zrobienia w ciągu kilku dni"
						 :priority "C")
					  (:name "Czekają sobie na dłuższy kwant czasu"
						 :priority "D")
					  (:name "W ogóle luzik"
						 :priority "E")
                                          (:name "Pozostałe")
                                         ))
                                     (org-ql-block-header "Zadania")))
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
                     ("w" "Agenda for work"
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
                       ;; (org-ql-block '(and (todo "TODO")
                       ;;                     (tags "workbreak")
                       ;;                     (or (priority '< "A")
                       ;;                         (scheduled :to today)))
                       ;;               ((org-ql-block-header "Things to do during work break")))
                       (org-ql-block '(and (todo "TODO")
                                           (tags "work")
                                           (not (tags "current" "template"))
					   (not (scheduled))
                                           (or (priority '< "B")
                                               (not (priority))))
                                     ((org-ql-block-header "Tasks from not-current projects")))
                       (org-ql-block '(and (tags "work")
                                           (not (todo "TODO" "DONE" "CANCELLED"))
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

             (setq org-agenda-include-diary t)

	     (defun sf-generate-org-link ()
	       "Generate an org-mode formatted link by replacing the word at point if it matches a pattern or prompts for input."
	       (interactive)
	       (let* ((bounds (bounds-of-thing-at-point 'line))
		      (line-at-point (if bounds (buffer-substring-no-properties (car bounds) (cdr bounds))))
		      (word-at-point (and line-at-point
					  (cond ((string-match "!\\(\\w+\\)" line-at-point) (match-string 0 line-at-point))
						((string-match "\\$\\(\\w+\\)" line-at-point) (match-string 0 line-at-point))
						((string-match "SDS!\\(\\w+\\)" line-at-point) (match-string 0 line-at-point)))))
		      (input (or word-at-point
				 (read-string "Enter the argument: ")))
		      (url "")
		      (link-description "Link")
		      case-fold-search)  ; Case-sensitive matching
		 (cond
		  ;; Case 1: Argument starts with "!"
		  ((string-prefix-p "!" input)
		   (let ((some-string (substring input 1)))
		     (setq url (format "https://gitlab.silverfin.com/development/silverfin/-/merge_requests/%s" some-string))
		     (setq link-description (format "MR %s" some-string))))

		  ;; Case 2: Argument starts with "$"
		  ((string-prefix-p "$" input)
		   (let ((some-string (substring input 1)))
		     (setq url (format "https://gitlab.silverfin.com/development/silverfin/-/issues/%s" some-string))
		     (setq link-description (format "Issue %s" some-string))))

		  ;; Case 3: Argument starts with "SDS!"
		  ((string-prefix-p "SDS!" input)
		   (let ((some-string (substring input 4)))
		     (setq url (format "https://gitlab.silverfin.com/development/sf-sync/-/merge_requests/%s" some-string))
		     (setq link-description (format "SDS MR %s" some-string))))

		  ;; Default case: No match
		  (t
		   (message "Warning: No matching pattern for input: %s" input)
		   (setq url nil)))

		 ;; Replace the word at point or insert the link if URL is set
		 (when url
		   (if word-at-point
		       (progn
			 (goto-char (car bounds))
			 (re-search-forward (regexp-quote word-at-point) (cdr bounds))
			 (replace-match (format "[[%s][%s]]" url link-description)))
		     (insert (format "[[%s][%s]]" url link-description))))))

	     (setq rl-movies-upflix-replace-hostname "http://localhost:9393")
	     (setq rl-movies-supported-subscriptions '("netflix" "disney" "viaplay" "skyshowtime" "canalplus" "cineman" "appletv" "hbomax" "cdapremium" "amazon" "tvpvod"))
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
	       "Fetches a webpage from the given URL and parses it as JSON."
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
		   (rl-movies-refresh-entry)
		   )
		 ;; Step 4: Process entries with timestamps
		 (dolist (headline headlines-with-timestamps)
		   (goto-char (cdr headline))
		   (message "Processing entry with timestamp: %s" (org-get-heading t t))
		   (rl-movies-refresh-entry)
		   )))
	     (defun rl-movies-refresh-all ()
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
	       (interactive)
	       (let* ((link1 (org-entry-get nil "UPFLIX_LINK"))
		      (api1 (replace-regexp-in-string (regexp-quote "https://upflix.pl") "http://localhost:9393" link1))
		      (information (fetch-and-parse-json api1)))
		 (if information
		     (let* (
			    (title-pl (gethash "polish_title" information))
			    (title-en (gethash "english_title" information))
			    (year (gethash "year" information))
			    (genres (gethash "genres" information))
			    (subscriptions (gethash "subscriptions" information))
			    (filmweb-url (gethash "filmweb_url" information))
			    (imdb-url (gethash "imdb_url" information))
			    (current-local-tags (org-get-tags nil t))
			    )
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
			       (setq current-local-tags (delete subscription-tag current-local-tags)))
			     ))
			 (if (and filmweb-url (not (eq filmweb-url :null)))
			     (org-set-property "FILMWEB_URL" filmweb-url))
			 (if (and imdb-url (not (eq imdb-url :null)))
			     (org-set-property "IMDB_URL" imdb-url))
			 (org-set-property "LAST_REFRESHED" (with-temp-buffer (org-time-stamp '(16) 'inactive) (buffer-string)))
			 (org-set-tags (delete-dups current-local-tags))
			 "ok"))
		   (message "Failed to retrieve information"))))
	     )

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
	     (setq helm-org-headings-max-depth 1)
             (define-key evil-normal-state-leader-map "po" 'helm-org-agenda-files-headings)
             (define-key evil-normal-state-leader-map "ph" 'helm-org-in-buffer-headings)
             ; (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
             ; (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
             )

(use-package helm-org-rifle
             :straight t
             :after org)

(use-package org-ql
             :straight t
             :after org
	     :config
	     (setq org-ql-views
		   (append 
			   (list
			    (cons "Silverfin Tasks (sft)"
				  (list :buffers-files "~/pnotes/sf/gtd.org"
					:query '(and (todo "TODO" "WAITING") (tags "work"))
					:title "Silverfin Tasks"
					:sort nil
					:narrow nil
					:super-groups '((:discard (:scheduled future))
							(:discard (:tag "template"))
							(:name "Extreme top priority"
							       :priority "A")
							(:name "Morning ticklers"
							       :and (:tag "tickler" :tag "morning" :scheduled t))
							(:name "Blocked & waiting"
							       :todo "WAITING")
							(:name "Scheduled"
							       :scheduled t)
							(:priority "B")
							(:name "Current projects"
							       :tag "current")
							(:priority "C")
							(:discard (:scheduled t))
							(:auto-outline-path))))
			    (cons "Movies on Netflix"
				  (list :buffers-files "~/notes/movies.org"
					:query '(and (tags "on_netflix"))
					:title "Movies on Netflix"
					:sort nil
					:narrow nil
					:super-groups nil))
			    (cons "Cooking plans"
				  (list :buffers-files "~/notes/cooking.org"
					:query '(and (tags "@cooking"))
					:title "Cooking plans"
					:sort '(priority)
					:narrow nil
					:super-groups nil))
			    )))
	     )

(use-package org-super-agenda
             :straight t
             :after org
             :config
             (org-super-agenda-mode t)
             ; To fix the j/k manipulation on the headers when viewing super agenda
             ; https://github.com/alphapapa/org-super-agenda/issues/50
             (setq org-super-agenda-header-map (make-sparse-keymap))
             )

(use-package org-capture-pop-frame
  :straight t
  :after org)

(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; for org-roam
;; (use-package emacsql-sqlite
             ;; :straight t)
(use-package org-roam
  :straight t
  :after org
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-directory (file-truename "~/notes/roam"))
  (org-roam-capture-templates
	'(
	  ("d" "default" plain
	   "%?"
	   :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
	   :unnarrowed t)
	  ;; ("b" "book notes" plain (file "~/.emacs.d/templates/BookNoteTemplate.org")
	  ;;  :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	  ;;  :unnarrowed t)
	  ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point)
	 :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
	 )
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (require 'org-roam-protocol)
  (org-roam-db-autosync-enable)

  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
	  (org-roam-capture-templates (list (append (car org-roam-capture-templates)
						    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

  ;; Doesn't work very well with dailies. It marks them completely as done.
  ;; (defun my/org-roam-copy-todo-to-today ()
  ;;   (interactive)
  ;;   (let ((org-refile-keep t) ;; Set this to nil to delete the original!
  ;; 	  (org-roam-dailies-capture-templates
  ;; 	   '(("t" "tasks" entry "%?"
  ;; 	      :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
  ;; 	  (org-after-refile-insert-hook #'save-buffer)
  ;; 	  today-file
  ;; 	  pos)
  ;;     (save-window-excursion
  ;; 	(org-roam-dailies--capture (current-time) t)
  ;; 	(setq today-file (buffer-file-name))
  ;; 	(setq pos (point)))

  ;;     ;; Only refile if the target file is different than the current file
  ;;     (unless (equal (file-truename today-file)
  ;;                    (file-truename (buffer-file-name)))
  ;; 	(org-refile nil nil (list "Tasks" today-file nil pos)))))

  ;; (add-to-list 'org-after-todo-state-change-hook
  ;; 	       (lambda ()
  ;; 		 (when (equal org-state "DONE")
  ;; 		   (my/org-roam-copy-todo-to-today))))
  )

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

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

; recommended system package: fd-find
(use-package projectile
	     :straight t
	     :init
	     (projectile-mode +1)
	     :config
	     (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
             (define-key evil-normal-state-leader-map "pr" 'projectile-recentf)
             (define-key evil-normal-state-leader-map "pp" 'projectile-find-file)
	     )

(use-package projectile-rails
	     :straight t
	     :init
	     (projectile-rails-global-mode)
	     :config
	     (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)
	     )

(use-package ag
  	     :straight t
	     :init
	     (setq ag-highlight-search t)
	     )

(use-package rbenv
	     :straight t
	     :init
	     (global-rbenv-mode)
	     )

(use-package asdf
 	     :straight (:host github :repo "tabfugnic/asdf.el" :files ("*.el"))
	     :config
	     (asdf-enable)
	     )

(use-package slim-mode
             :straight t)

(use-package direnv
	     :straight t
	     :init
	     (direnv-mode))

(use-package transpose-frame
	     :straight t
	     :config
	     (define-key evil-normal-state-leader-map "wf" 'transpose-frame)
	     (define-key evil-normal-state-leader-map "wr" 'rotate-frame-clockwise)
	     )

(use-package ruby-end
             :straight t)

(use-package nerd-icons
  :straight t)

(use-package org-journal
  :straight t
  :config
  (setq org-journal-dir (file-truename "~/notes/journal"))
  )

(use-package org-contacts
  :straight t
  :config
  (setq org-contacts-files (list (file-truename "~/notes/contacts.org")))
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

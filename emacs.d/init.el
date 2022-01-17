;; MELPA

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Packaging

(require 'package)
; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

; Display information in messages when there are some problems with loading the package or it's load time is big
(setq use-package-verbose t)
; Automatically install all use-packages if they are missing
(setq use-package-always-ensure t) 

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


(use-package evil
             :ensure t
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
             :ensure t
             :config
             (evil-collection-init))

;; Helm
(use-package helm
             :after evil
             :config
             (global-set-key (kbd "M-x") 'helm-M-x)
             (setq helm-buffers-fuzzy-matching t)
             (setq helm-recentf-fuzzy-matching t)
             (define-key evil-normal-state-leader-map "pb" 'helm-mini)
             (define-key evil-normal-state-leader-map "pc" 'helm-M-x)
             )

;; FZF
(use-package fzf
             :after evil
             :config
             (define-key evil-normal-state-leader-map "ff" 'fzf-git-files)
             )

;; Hybrid-relative line numbering
(use-package nlinum-relative
             :config
             (nlinum-relative-setup-evil)
             (add-hook 'prog-mode-hook 'nlinum-relative-mode)
             (setq nlinum-relative-redisplay-delay 0)
             (setq nlinum-relative-current-symbol ""))

(use-package git-gutter-fringe
             :if window-system
             :after evil
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
             :config
             (define-key evil-normal-state-leader-map "tn" 'rspec-verify-single)
             (define-key evil-normal-state-leader-map "tf" 'rspec-verify)
             (define-key evil-normal-state-leader-map "tl" 'rspec-rerun)
             )

(use-package yaml-mode)

(use-package evil-nerd-commenter
             :config
             (define-key evil-normal-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
             (define-key evil-visual-state-map "gc" 'evilnc-comment-or-uncomment-lines)
             )

(use-package magit
             :config
             (add-to-list 'magit-no-confirm 'drop-stashes)
             )

; (use-package forge
;              :after magit)

; (use-package evil-magit
;              :after evil magit
;              :config
;              (define-key evil-normal-state-leader-map "gs" 'magit-status)
;              (define-key evil-normal-state-leader-map "gg" 'magit-dispatch-popup)
;              (define-key evil-normal-state-leader-map "gb" 'magit-blame)
;              )

;; JSX mode
(use-package rjsx-mode
             :config
             (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
             )

;; Elm mode
(use-package elm-mode
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
             :config
             (setq org-agenda-files '("~/notes" "~/projs/silverfin" "~/projs/railseventstore"))
             (setq org-log-done t)
             (global-set-key (kbd "C-c l") #'org-store-link)
             (global-set-key (kbd "C-c a") #'org-agenda)
             (global-set-key (kbd "C-c c") #'org-capture)
             (add-to-list 'org-modules 'org-habit t)
             (setq org-agenda-include-diary t))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Themes
(use-package solarized-theme
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

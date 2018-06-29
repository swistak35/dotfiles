;; MELPA

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Packaging
(package-initialize)

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

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

;; Helm
(use-package helm
             :config
             (global-set-key (kbd "M-x") 'helm-M-x)
             (global-set-key (kbd "M-p M-b") 'helm-buffers-list))

;; FZF
(use-package fzf
             :config
             (global-set-key (kbd "M-l M-p M-p") 'fzf-git-files))


;; Hybrid-relative line numbering
(use-package nlinum-relative
             :config
             (nlinum-relative-setup-evil)
             (add-hook 'prog-mode-hook 'nlinum-relative-mode)
             (setq nlinum-relative-redisplay-delay 0)
             (setq nlinum-relative-current-symbol ""))

(use-package evil
             :config
             (evil-mode 1)
             (setq ;evil-mode-line-format nil ;Disable displaying information about evil state
               evil-normal-state-cursor '(box "White")
               evil-insert-state-cursor '(bar "White")
               evil-visual-state-cursor '(box "#F86155")))

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
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(package-selected-packages
   (quote
    (list-packages-ext solarized-theme dired-subtree dired-collapse dired-hacks-utils))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

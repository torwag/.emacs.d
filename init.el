;;; init.el --- Emacs configuration of Fredrik Bergroth -*- lexical-binding: t; -*-

;;; Commentary:
;; Flat configuration of Emacs

;;; Code:



;;; Package management
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

(eval-and-compile
  (use-package evil-use-package
    :load-path "lisp/"))

;;; Initialization

(setq inhibit-default-init t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      initial-scratch-message nil)

(load-file "~/.private.el")

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;;; UI

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(line-number-mode)
(column-number-mode)
(size-indication-mode)
(fset 'yes-or-no-p #'y-or-n-p)

(use-package cus-edit
  :defer t
  :init
  (progn (setq custom-file (locate-user-emacs-file "custom.el"))
         (load custom-file 'no-error 'no-message))
  :config
  (setq custom-buffer-verbose-help nil
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil))

(use-package unicode-fonts
  :ensure t
  :init (unicode-fonts-setup))

(use-package dynamic-fonts
  :ensure t
  :init
  (dynamic-fonts-setup))

(use-package paren
  :init (show-paren-mode 1)
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

(use-package leuven-theme
  :ensure t
  :init (load-theme 'leuven 'no-confirm))

(use-package auto-dim-other-buffers
  :diminish auto-dim-other-buffers-mode
  :ensure t
  :init (auto-dim-other-buffers-mode +1))

(use-package my-x
  :load-path "lisp/"
  :defer t)

;;; Files

(auto-compression-mode)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      delete-by-moving-to-trash t
      require-final-newline 'visit-save)

(use-package autorevert
  :init (global-auto-revert-mode)
  :config (setq global-auto-revert-non-file-buffers t))

(use-package image-file
  :init (auto-image-file-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :init (global-whitespace-cleanup-mode)
  :diminish whitespace-cleanup-mode)

;;; Completion, snippets

(use-package company
  :diminish company-mode
  :ensure t
  :defer t
  :idle (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil))

(use-package hippie-exp
  :evil-bind ((insert "TAB" hippie-expand))
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-expand-line))
    (with-eval-after-load 'yasnippet
      (add-to-list 'hippie-expand-try-functions-list
                   'yas-hippie-try-expand))))

(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :idle (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-subword-mode 1)

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))


(bind-keys ("C-h C-f" . find-function)
           ("C-h C-v" . find-variable))


(use-package subword
  :init (global-subword-mode)
  :diminish subword-mode)

(use-package ace-jump-mode
  :ensure t)

(use-package ag
  :config
  (setq ag-highlight-search t)
  :ensure t)

(use-package calendar
  :config
  (setq calendar-week-start-day 1))

(use-package circe
  :ensure t
  :defer t
  :defines (circe-color-nicks-everywhere)
  :config
  (progn
    (require 'circe-color-nicks)
    (enable-circe-highlight-all-nicks)
    (enable-circe-color-nicks)

    (setq circe-default-part-message "Part"
          circe-default-quit-message "Quit"
          circe-color-nicks-everywhere t
          circe-highlight-nick-type 'message
          circe-reduce-lurker-spam t
          lui-time-stamp-position 'right-margin
          lui-fill-type nil
          lui-flyspell-p t
          lui-flyspell-alist '((".*" "american")))

    (add-hook 'lui-mode-hook #'my-lui-setup)
    (defun my-lui-setup ()
      (setq fringes-outside-margins t
            right-margin-width 7
            word-wrap t
            wrap-prefix "    "))))

(use-package cl-lib-highlight
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook #'cl-lib-highlight-initialize)
    (add-hook 'emacs-lisp-mode-hook #'cl-lib-highlight-warn-cl-initialize)))

(use-package diff-hl
  :ensure t
  :init
  (progn
    (global-diff-hl-mode 1)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(use-package dired
  :defer t
  :config
  (setq dired-auto-revert-buffer t
        dired-dwim-target t
        dired-listing-switches "-alhFg"))

(use-package dired-x
  :defer t)

(use-package ediff
  :config
  (progn
    (setq ediff-diff-options "-w"
          ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain)
    (with-eval-after-load 'winner
      (add-hook 'ediff-after-quit-hook-internal #'winner-undo))))

;;; Editing

(setq sentence-end-double-space nil)

(setq-default fill-column 80
              indent-tabs-mode nil
              indicate-empty-lines t)

(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config
  (progn
    (setq-default evil-symbol-word-search t)
    (setq evil-cross-lines t
          evil-want-C-w-in-emacs-state t)
    ;; TODO
    (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)
    (add-to-list 'evil-emacs-state-modes 'project-explorer-mode)
    (add-to-list 'evil-emacs-state-modes 'paradox-menu-mode)
    (add-to-list 'evil-emacs-state-modes 'makey-key-mode)
    (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
    (add-to-list 'evil-emacs-state-modes 'paradox-commit-list-mode)
    (add-to-list 'evil-insert-state-modes 'snippet-mode)

    (bind-key "C-w" 'evil-delete-backward-word minibuffer-local-map)
    (bind-key [escape] 'keyboard-escape-quit)

    (unbind-key "M-." evil-normal-state-map) ;; fall through to find-tag
    (bind-keys :map evil-normal-state-map
               ("M-]" . next-error)
               ("M-[" . previous-error)
               ("M-," . pop-tag-mark)
               ("C-t" . pop-global-mark)
               ("C-u" . evil-scroll-up)
               ("C-b" . universal-argument)
               ("SPC" . evil-ace-jump-word-mode)
               ("C-SPC" . evil-ace-jump-line-mode))

    (bind-keys :map evil-visual-state-map
               ("\\" . comment-or-uncomment-region)
               ("SPC" . evil-ace-jump-word-mode)
               ("C-SPC" . evil-ace-jump-line-mode))

    (bind-keys :map evil-operator-state-map
               ("SPC" . evil-ace-jump-word-mode)
               ("C-SPC" . evil-ace-jump-line-mode))

    (use-package evil-leader
      :ensure t
      :init (global-evil-leader-mode 1)
      :config
      (progn
        (evil-leader/set-leader ",")
        (defun my-find-init-el ()
          (interactive)
          (find-file user-init-file))
        (evil-leader/set-key
          ;; "g" 'magit-status
          "u" 'undo-tree-visualize
          "t" 'project-explorer-open
          "d" 'dired-jump
          "v" 'my-find-init-el
          "p" 'projectile-commander
          "i" 'helm-imenu
          "b" 'helm-bookmarks)))

    (use-package evil-exchange
      :ensure t
      :config (evil-leader/set-key "x" 'evil-exchange))

    (use-package evil-surround
      :ensure t
      :init (global-evil-surround-mode 1))))

(use-package anzu
  :ensure t
  :init (global-anzu-mode))

(use-package evil-anzu
  :load-path "~/code/emacs-evil-anzu"
  :evil-bind ((motion "n" evil-anzu-search-next
                      "N" evil-anzu-search-previous))
  :init (global-evil-anzu-mode))

(use-package string-inflection
  :ensure t
  :evil-leader ("c" string-inflection-cycle))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode 1)
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save new-line mode-enabled)
          flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)

    (with-eval-after-load 'shackle
      (add-to-list 'shackle-rules '(flycheck-error-list-mode
                                    :ratio 0.25
                                    :align t)))

    (defun my-fc-post-command-hook ()
      (unless (-contains? '(my-fc-next-error my-fc-previous-error) this-command)
        (remove-hook 'post-command-hook #'my-fc-post-command-hook)
        (-when-let (win (get-buffer-window flycheck-error-list-buffer))
          (delete-window win))))

    (defun my-fc-next-error ()
      (interactive)
      (add-hook 'post-command-hook #'my-fc-post-command-hook)
      (when (flycheck-next-error)
        (flycheck-list-errors)))

    (defun my-fc-previous-error ()
      (interactive)
      (add-hook 'post-command-hook #'my-fc-post-command-hook)
      (when (flycheck-previous-error)
        (flycheck-list-errors)))

    (bind-keys :map evil-normal-state-map
               ("]" . my-fc-next-error)
               ("[" . my-fc-previous-error))))

(use-package ispell
  :defer t
  :config
  (setq ispell-list-command "list"
        ispell-program-name "aspell"))

(use-package flyspell
  :diminish flyspell-mode
  :ensure t
  :config
  (progn
    (setq flyspell-issue-message-flag nil
          flyspell-issue-welcome-flag nil)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

(use-package helm
  :ensure t
  :config
  (progn
    (bind-keys :map helm-map
               ("C-S-n" . helm-follow-action-forward)
               ("C-S-p" . helm-follow-action-backward))))

(use-package helm-swoop
  :ensure t
  :config
  (progn
    (setq  helm-swoop-speed-or-color t
           helm-swoop-use-line-number-face t)
    (evil-leader/set-key
      "s" #'helm-swoop)))

(use-package hl-line
  :ensure t
  :init (global-hl-line-mode 1))

(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode 1)
  :config
  (setq hl-todo-activate-in-modes '(prog-mode)))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ido
  :ensure t
  :init (progn (ido-mode)
               (ido-everywhere))
  :config
  (setq ido-enable-flex-matching t
        ido-completion-buffer nil
        ido-use-faces nil))

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode))

(use-package ido-ubiquitous
  :ensure t
  :init (ido-ubiquitous-mode))

(use-package ido-at-point
  :ensure t
  :init (ido-at-point-mode))

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode))

(use-package magit
  :diminish magit-auto-revert-mode
  :load-path "~/code/magit/"
  :evil-leader ("g" magit-status)
  :ensure t)

(use-package magit-popup
  :evil-state (magit-popup-mode emacs))

(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))

(use-package org
  :ensure t
  :config
  (progn
    (setq org-src-fontify-natively t)
    (add-to-list 'org-latex-default-packages-alist '("" "lmodern" nil))))

(use-package shackle
  :ensure t
  :diminish shackle-mode
  :init (shackle-mode 1))

(use-package macrostep
  :ensure t
  :evil-leader ((emacs-lisp-mode
                 "e" macrostep-expand)
                (lisp-interaction-mode
                 "e" macrostep-expand))
  :config
  (progn
    (evil-make-overriding-map macrostep-keymap 'motion)
    (defun my-macrostep-evil-states ()
      (if macrostep-mode
          (evil-motion-state)
        (evil-normal-state)))
    (add-hook 'macrostep-mode-hook #'my-macrostep-evil-states)))

(use-package project-explorer
  :ensure t)

(use-package projectile
  :ensure t
  :init (projectile-global-mode 1)
  :config
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package stripe-buffer
  :ensure t
  :config
  (progn
    (set-face-background 'stripe-highlight (face-background 'auto-dim-other-buffers-face))
    (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)))

(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"))

(use-package windmove
  :ensure t
  :bind (("M-h" . windmove-left)
         ("M-l" . windmove-right)
         ("M-k" . windmove-up)
         ("M-j" . windmove-down)))

(use-package winner
  :init (winner-mode 1)
  :bind (("M-}" . winner-redo)
         ("M-{" . winner-undo)))

(use-package highlight-numbers
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package anaconda-mode
  :ensure t
  :init
  (progn
    (add-hook 'python-mode-hook #'anaconda-mode)
    (add-hook 'python-mode-hook #'eldoc-mode)
    (bind-keys :map anaconda-mode-map
               ("M-," . anaconda-nav-pop-marker))))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :ensure t
  :init (global-page-break-lines-mode))

(use-package paradox
  :ensure t
  :config
  (setq paradox-execute-asynchronously nil
        paradox-github-token t))

(use-package pcre2el
  :ensure t)

(use-package pyenv-mode
  :ensure t
  :config
  (progn
    (defun my-set-pyenv ()
      (let ((name (projectile-project-name)))
        (when (member name (pyenv-mode-versions))
          (pyenv-mode 1)
          (pyenv-mode-set name))))
    (add-hook 'python-mode-hook 'my-set-pyenv)))

(use-package help-mode
  :config
  (evil-make-overriding-map help-mode-map 'motion))

;; (use-package re-builder
;;   :config
;;   (progn
;;     (add-to-list 'evil-emacs-state-modes 'reb-subexp-mode)
;;     (--each (list reb-mode-map reb-lisp-mode-map)
;;       (evil-define-key 'normal it
;;         "q" 'reb-quit
;;         "y" 'reb-copy
;;         "n" 'reb-next-match
;;         "p" 'reb-prev-match
;;         "S" 'reb-enter-subexp-mode
;;         "B" 'reb-change-target-buffer
;;         "C" 'reb-toggle-case
;;         "\C-i" 'reb-change-syntax))))

;;; init.el ends here

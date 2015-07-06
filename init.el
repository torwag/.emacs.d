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
(setq x-gtk-use-system-tooltips nil)

(use-package sh-script
  :mode (("\\.envrc" . sh-mode)))

(use-package cus-edit
  :defer t
  :init
  (progn (setq custom-file (locate-user-emacs-file "custom.el"))
         (load custom-file 'no-error 'no-message)
         (load "~/.private.el" 'no-error 'no-message))
  :config
  (setq custom-buffer-verbose-help nil
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil))

(use-package dynamic-fonts
  :ensure t
  :config
  (progn
    (setq dynamic-fonts-preferred-monospace-point-size 10
          dynamic-fonts-preferred-monospace-fonts
          (-union '("Source Code Pro") dynamic-fonts-preferred-monospace-fonts))
    (dynamic-fonts-setup)))

(use-package unicode-fonts
  :ensure t
  :disabled t
  :init (unicode-fonts-setup))

(use-package paren
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren nil
                show-paren-when-point-in-periphery t))

(use-package material-light-theme
  :ensure material-theme
  :config
  (load-theme 'material-light 'no-confirm))

(use-package zenburn-theme
  :ensure t
  :disabled t
  :init
  (load-theme 'zenburn 'no-confirm))

(use-package solarized-theme
  :ensure t
  :disabled t
  :init
  (load-theme 'solarized-light 'no-confirm))

(use-package leuven-theme
  :ensure t
  :disabled t
  :init (load-theme 'leuven 'no-confirm))

(use-package faff-theme
  :ensure t
  :disabled t
  :init (load-theme 'faff 'no-confirm))

(use-package auto-dim-other-buffers
  :diminish auto-dim-other-buffers-mode
  :ensure t
  :disabled t
  :init (auto-dim-other-buffers-mode))

(use-package my-x
  :load-path "lisp/"
  :defer t)

;;; Files

(auto-compression-mode)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      delete-by-moving-to-trash t
      require-final-newline 'visit-save)

(use-package lisp-extra-font-lock
  :ensure t
  :init (lisp-extra-font-lock-global-mode))

(use-package yaml-mode
  :mode "\\.sls$"
  :ensure t
  :defer t)

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
  :evil-bind (insert "M-TAB" company-complete)
  :defer t
  :config
  (progn
    (global-company-mode)
    (bind-key "M-TAB" 'company-select-next company-active-map)
    (setq company-tooltip-align-annotations t
          company-dabbrev-downcase nil
          company-dabbrev-code-everywhere t
          company-dabbrev-ignore-case nil)))

(use-package company-quickhelp
  :ensure t
  :config
  (with-eval-after-load 'company
    (company-quickhelp-mode)))

(use-package hippie-exp
  :evil-bind (insert "TAB" hippie-expand)
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
  :defer (yas-global-mode))

(use-package helm
  :ensure t
  :config
  (progn
    (bind-key "<backspace>" 'backward-kill-word helm-map)
    (setq helm-autoresize-max-height 30
          helm-autoresize-min-height 30
          helm-split-window-in-side-p t
          helm-echo-input-in-header-line t)
    (helm-mode)
    (helm-autoresize-mode)

    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))
    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)))

(use-package helm-ag
  :ensure t
  :config (setq helm-ag-fuzzy-match t
                helm-ag-insert-at-point 'symbol
                helm-ag-source-type 'file-line))

(use-package helm-buffers
  :config
  (setq helm-buffers-fuzzy-matching t))

(use-package helm-command
  :config
  (setq helm-M-x-fuzzy-match t))

(use-package helm-flx
  :disabled t
  :load-path "~/code/helm-flx"
  :config (helm-flx-mode))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :defer (projectile-cleanup-known-projects)
  :config
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :config (setq hydra-lv nil))

(global-subword-mode)

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
  :init (global-diff-hl-mode)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package dired
  :defer t
  :config
  (setq dired-auto-revert-buffer t
        dired-dwim-target t
        dired-listing-switches "-alhF --group-directories-first"))

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
  :config
  (progn
    (setq evil-cross-lines t
          evil-want-C-i-jump nil
          evil-want-C-w-in-emacs-state t)
    (evil-mode)
    (setq-default evil-symbol-word-search t)
    ;; TODO
    (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)
    (add-to-list 'evil-emacs-state-modes 'project-explorer-mode)
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
               ;; ("SPC" . evil-ace-jump-word-mode)
               ;; ("C-SPC" . evil-ace-jump-line-mode)
               )

    (bind-keys :map evil-visual-state-map
               ("\\" . comment-or-uncomment-region)
               ;; ("SPC" . evil-ace-jump-word-mode)
               ;; ("C-SPC" . evil-ace-jump-line-mode)
               )

    ;; (bind-keys :map evil-operator-state-map
    ;;            ("SPC" . evil-ace-jump-word-mode)
    ;;            ("C-SPC" . evil-ace-jump-line-mode))

    (use-package evil-leader
      :ensure t
      :config
      (progn
        (evil-leader/set-leader "SPC")
        (global-evil-leader-mode)
        (defun my-find-init-el ()
          (interactive)
          (find-file user-init-file))
        (evil-leader/set-key
          ;; "g" 'magit-status
          "u" 'undo-tree-visualize
          "t" 'project-explorer-open
          "d" 'dired-jump
          "f" 'helm-find-files
          "v" 'my-find-init-el
          "p" 'projectile-commander
          "TAB" 'helm-mini
          "i" 'helm-imenu
          "b" 'helm-bookmarks)))

    (use-package evil-exchange
      :ensure t
      :config (evil-leader/set-key "x" 'evil-exchange))

    (use-package evil-surround
      :ensure t
      :init (global-evil-surround-mode))))

(use-package anzu
  :diminish anzu-mode
  :evil-leader ("r" anzu-query-replace-at-cursor-thing)
  :ensure t
  :init (global-anzu-mode))

(use-package evil-anzu
  :ensure t)

(use-package elide-head
  :init (add-hook 'prog-mode-hook #'elide-head))

(use-package string-inflection
  :ensure t
  :evil-leader ("c" string-inflection-cycle))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (use-package popup)
  (defun my-elisp-popup-doc (sym-name)
    (interactive (list (elisp-slime-nav--read-symbol-at-point)))
    (let* ((help-xref-following t)
           (description
            (save-window-excursion
              (with-temp-buffer
                (help-mode)
                (help-xref-interned (intern sym-name))
                (buffer-string)))))
      (pos-tip-show description)))
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "h" 'my-elisp-popup-doc))

(use-package flycheck
  :ensure t
  :evil-state (flycheck-error-list-mode . emacs)
  :config
  (progn
    (global-flycheck-mode)
    (defun my-flycheck-error-fn (errors)
      (when (evil-normal-state-p)
        (-when-let (messages (-keep #'flycheck-error-message errors))
          (pos-tip-show (mapconcat 'identity messages "\n")))))
    (setq flycheck-display-errors-function #'my-flycheck-error-fn)
    ;; (delq 'idle-change flycheck-check-syntax-automatically)
    (evil-leader/set-key
      "q" (defhydra my-flycheck-hydra ()
            "flycheck-error"
            ("n" flycheck-next-error "next")
            ("p" flycheck-previous-error "prev")))))

(use-package flycheck-cask
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-follow
  :load-path "lisp/"
  :evil-bind (normal flycheck-mode-map
                     "]q" flycheck-follow-next-error
                     "[q" flycheck-follow-previous-error))

(use-package ispell
  :defer t)

(use-package flyspell
  :diminish flyspell-mode
  :ensure t
  :config
  (progn
    (setq flyspell-issue-message-flag nil
          flyspell-issue-welcome-flag nil)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

(use-package helm-swoop
  :ensure t
  :evil-leader ("s" helm-swoop)
  :config
  (setq helm-swoop-speed-or-color t
        helm-swoop-use-line-number-face t))

(use-package hl-line
  :ensure t
  :init (global-hl-line-mode))

(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode)
  :config
  (setq hl-todo-activate-in-modes '(prog-mode)))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-projectile
  :ensure t
  :config
  (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

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
  :ensure t
  :evil-leader ("g" magit-status))

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
  :init (shackle-mode))

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

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package stripe-buffer
  :ensure t
  :config
  (progn
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
  :init (winner-mode)
  :bind (("M-}" . winner-redo)
         ("M-{" . winner-undo)))

(use-package highlight-numbers
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package python
  :evil-bind (normal python-mode-map
                     "M-n" python-nav-forward-defun
                     "M-p" python-nav-backward-defun)
  :init (add-hook 'python-mode-hook #'eldoc-mode))

(use-package anaconda-mode
  :diminish anaconda-mode
  :ensure t
  :evil-bind (normal anaconda-mode-map
                     "M-." anaconda-mode-goto
                     "M-," anaconda-nav-pop-marker)
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda
  :ensure t
  :defer t
  :init
  (defun my-setup-py-company ()
    (make-local-variable 'company-backends)
    (setq-local company-idle-delay 0.1)
    (add-to-list 'company-backends 'company-anaconda))
  (add-hook 'anaconda-mode-hook #'my-setup-py-company))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :ensure t
  :init (global-page-break-lines-mode))

(use-package paradox
  :ensure t
  :evil-state ((paradox-menu-mode . emacs)
               (paradox-commit-list-mode . emacs))
  :config
  (setq paradox-execute-asynchronously nil
        paradox-github-token t))

(use-package pcre2el
  :ensure t)

(use-package pyenv-mode
  :ensure t
  :config
  (progn
    (defun my-pyenv-mode-set ()
      (let ((target-file (expand-file-name ".python-version" (projectile-project-root))))
        (when (file-exists-p target-file)
          (pyenv-mode-set (with-temp-buffer
                            (insert-file-contents target-file)
                            (current-word))))))
    (add-hook 'python-mode-hook 'pyenv-mode)
    (add-hook 'projectile-switch-project-hook 'my-pyenv-mode-set)))

(use-package help-mode
  :config
  (evil-make-overriding-map help-mode-map 'motion))

(use-package jinja2-mode
  :ensure t
  :mode (("\\.html$" . jinja2-mode)))

(use-package web-mode
  :disabled t
  :ensure t
  :mode (("\\.html$" . web-mode)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\(?:on\\)?\\'"
  :config
  (progn
    (setq js2-strict-missing-semi-warning nil)
    (setq-default js2-basic-offset 2)))

(use-package tern
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package company-tern
  :ensure t
  :config
  (progn
    (defun my-init-company-tern ()
      (add-to-list 'company-backends 'company-tern))

    (add-hook 'tern-mode-hook 'my-init-company-tern)))

(use-package typo
  :config
  (typo-global-mode))

(use-package bonjourmadame
  :config
  (add-to-list 'evil-emacs-state-modes 'bonjourmadame-mode))

(use-package yoda
  :disabled t
  :load-path "~/code/yoda.el"
  :init (add-hook 'python-mode-hook #'yoda-mode))

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

(use-package csv-mode
  :ensure t)

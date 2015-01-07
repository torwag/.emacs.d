;;; init.el --- Emacs configuration of Fredrik Bergroth -*- lexical-binding: t; -*-

;;; Commentary:
;; Flat configuration of Emacs

;;; Code:

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)
(load-file "~/.private.el")

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(auto-compression-mode 1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(size-indication-mode 1)
(global-auto-revert-mode 1)
(global-subword-mode 1)

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(require 'use-package)

(defun my-use-package-evil-states (states)
  (-map (-lambda ((mode . state))
          `(add-to-list
            ,(intern (format "evil-%s-state-modes"
                             (symbol-name state)))
            mode))
        states))

(add-to-list 'use-package-keywords :evil-state)

(defadvice use-package (before my-use-package activate)
  (let ((config-body (use-package-plist-get args :config)))
    (--when-let (use-package-plist-get args :evil-state)
      (setq config-body `(progn ,config-body
                                ,@(my-use-package-evil-states it))))
    (setq args (plist-put args :config config-body))))

(bind-keys ("C-h C-f" . find-function)
           ("C-h C-v" . find-variable))

(defun x-set-urgency-hint ()
  (-let* (((_ . ((frame))) (current-frame-configuration))
          ((flags . hints) (append (x-window-property "WM_HINTS" frame "WM_HINTS" nil nil t) nil))
          (wm-hints (cons (logior flags #x100) hints)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(use-package ace-jump-mode
  :ensure t)

(use-package ack-and-a-half
  :ensure t)

(use-package align
  :config
  (defadvice align-regexp (around align-regexp-with-spaces)
    (let ((indent-tabs-mode nil)) ad-do-it))
  (ad-activate 'align-regexp))

(use-package anzu
  :diminish anzu-mode
  :ensure t
  :init (global-anzu-mode +1)
  :config
  (eval-after-load 'evil
    (progn
      (defadvice evil-search
        (after evil-anzu-compat (string forward &optional regexp-p start))
        (setq isearch-regexp regexp-p)
        (run-hooks 'isearch-mode-hook 'isearch-update-post-hook))
      (ad-activate 'evil-search)
      (defadvice evil-flash-hook (after evil-anzu-compat)
        (run-hooks 'isearch-mode-end-hook))
      (ad-activate 'evil-flash-hook))))

(use-package auto-dim-other-buffers
  :diminish auto-dim-other-buffers-mode
  :ensure t
  :init (auto-dim-other-buffers-mode +1))

(use-package calendar
  :config
  (setq calendar-week-start-day 1))

(use-package company
  :diminish company-mode
  :ensure t
  :config
  (progn
    (bind-key "?" 'company-show-doc-buffer company-active-map)
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package circe
  :ensure t
  :config
  (progn
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
            wrap-prefix "    "))

    (defvar my-lui-highlight-buffer "*Circe-Highlights*")
    (add-hook 'lui-post-output-hook 'my-lui-save-highlights)
    (defun my-lui-save-highlights ()
      (when (-contains? (lui-faces-in-region (point-min) (point-max)) 'circe-highlight-nick-face)
        (x-set-urgency-hint)
        (let ((buffer (buffer-name))
              (target circe-chat-target)
              (network (with-circe-server-buffer
                         circe-server-network))
              (text (buffer-string)))
          (with-current-buffer (get-buffer-create my-lui-highlight-buffer)
            (goto-char (point-max))
            (save-restriction
              (narrow-to-region (point) (point))
              (insert (propertize (format-time-string "[%Y-%m-%d %H:%M:%S]")
                                  'face 'lui-time-stamp-face)
                      " "
                      (or target buffer)
                      "@"
                      network
                      " "
                      text
                      "\n")
              )))))


    ))

(use-package cl-lib-highlight
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook #'cl-lib-highlight-initialize)
    (add-hook 'emacs-lisp-mode-hook #'cl-lib-highlight-warn-cl-initialize)))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :ensure t
  :init
  (progn
    (global-diff-hl-mode 1)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(use-package dired-x)

(use-package direx
  :ensure t)

(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config
  (progn
    (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)
    (add-to-list 'evil-emacs-state-modes 'direx:direx-mode)
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
          "g" 'magit-status
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

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode 1)
  :config
  (progn
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

(use-package flyspell
  :diminish flyspell-mode
  :ensure t
  :config
  (progn
    (setq-default ispell-list-command "list")
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

(use-package go-mode
  :ensure t
  :config
  (progn
    (bind-key "M-." 'godef-jump go-mode-map)
    (add-hook 'before-save-hook 'gofmt-before-save)

    (when (load "$GOPATH/src/code.google.com/p/go.tools/refactor/rename/rename.el" t)
      (evil-leader/set-key-for-mode 'go-mode "r" 'go-rename))

    (use-package company-go
      :ensure t
      :config (add-to-list 'company-backends 'company-go))
    (use-package go-eldoc
      :ensure t
      :config (add-hook 'go-mode-hook 'go-eldoc-setup))))

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
  (evil-leader/set-key
    "s" #'helm-swoop))

(use-package hl-line
  :ensure t
  :init (global-hl-line-mode 1))

(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ido
  :ensure t
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-completion-buffer nil)
    (ido-everywhere t)
    (use-package flx-ido
      :ensure t
      :init (flx-ido-mode 1))
    (use-package ido-ubiquitous
      :ensure t
      :init (ido-ubiquitous-mode 1))
    (use-package ido-at-point
      :ensure t
      :init (ido-at-point-mode 1)
      :config
      (defadvice ido-at-point-read (around my-ido-at-point-fix activate)
        (if (bound-and-true-p ido-vertical-mode)
            (progn (ido-vertical-mode -1)
                   ad-do-it
                   (ido-vertical-mode +1))
          ad-do-it)))
    (use-package ido-vertical-mode
      :ensure t
      :init (ido-vertical-mode 1))))

(use-package magit
  :diminish magit-auto-revert-mode
  :ensure t)

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
  :init (shackle-mode 1))

(use-package macrostep
  :ensure t
  :config
  (progn
    (evil-leader/set-key-for-mode 'emacs-lisp-mode "e" 'macrostep-expand)
    (evil-leader/set-key-for-mode 'lisp-interaction-mode "e" 'macrostep-expand)
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
  (progn
    (add-to-list 'projectile-globally-ignored-directories "Godeps")
    (use-package helm-projectile
      :ensure t
      :config
      (progn
        (def-projectile-commander-method ?h
          "Helm projectile interface."
          (helm-projectile))))))

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

(use-package uniquify)

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

(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :init (yas-global-mode 1))

(use-package anaconda-mode
  :ensure t
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)
    (bind-keys :map anaconda-mode-map
               ("M-," . anaconda-nav-pop-marker))))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :ensure t
  :init (global-page-break-lines-mode))

(use-package paradox
  :ensure t)

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

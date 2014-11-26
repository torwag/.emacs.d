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

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(auto-compression-mode 1)
(blink-cursor-mode -1)
(size-indication-mode 1)
(global-auto-revert-mode 1)
(global-subword-mode 1)

(require 'use-package)

(bind-keys ("C-h C-f" . find-function)
           ("C-h C-v" . find-variable))

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

(use-package company
  :diminish company-mode
  :ensure t
  :config
  (progn
    (bind-key "?" 'company-show-doc-buffer company-active-map)
    (add-hook 'after-init-hook 'global-company-mode)))

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

    (use-package evil-surround
      :ensure t
      :init (global-evil-surround-mode 1))))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode 1)
  :config
  (progn
    (defvar my-flycheck-list-timer nil)

    (defun my-flycheck-show-list ()
      (when my-flycheck-list-timer
        (cancel-timer my-flycheck-list-timer))
      (setq my-flycheck-list-timer
            (run-with-timer 3 nil 'my-flycheck-close-list-window))
      (flycheck-list-errors))

    (defun my-flycheck-close-list-window ()
      (when (eq popwin:popup-buffer (get-buffer flycheck-error-list-buffer))
        (popwin:close-popup-window)))

    (defun my-flycheck-next-error ()
      (interactive)
      (my-flycheck-show-list)
      (flycheck-next-error))

    (defun my-flycheck-previous-error ()
      (interactive)
      (my-flycheck-show-list)
      (flycheck-previous-error))

    (bind-keys :map evil-normal-state-map
               ("]" . my-flycheck-next-error)
               ("[" . my-flycheck-previous-error))))

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
    (ido-everywhere t)
    (use-package flx-ido
      :ensure t
      :init (flx-ido-mode 1))
    (use-package ido-ubiquitous
      :ensure t
      :init (ido-ubiquitous-mode 1))
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

(use-package popwin
  :ensure t
  :init (popwin-mode 1)
  :config
  (progn
    (push '("^\*helm.+\*$" :regexp t :height 15)
          popwin:special-display-config)
    (push '(flycheck-error-list-mode :height 10 :noselect t)
          popwin:special-display-config)
    (push '(direx:direx-mode :position left :width 25 :dedicated t)
          popwin:special-display-config)))

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

(use-package smartparens
  :ensure t
  :init (show-smartparens-global-mode 1))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

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
  :init (winner-mode 1))

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

;;; init.el ends here

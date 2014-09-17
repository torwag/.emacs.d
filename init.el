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
(global-auto-revert-mode 1)
(global-subword-mode 1)

(require 'use-package)

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
    (add-to-list 'evil-insert-state-modes 'snippet-mode)

    (bind-key [escape] 'keyboard-escape-quit)

    (unbind-key "M-." evil-normal-state-map) ;; fall through to find-tag
    (bind-keys :map evil-normal-state-map
               ("]" . next-error)
               ("[" . previous-error)
               ("M-," . pop-tag-mark))

    (bind-keys :map evil-visual-state-map
               ("\\" . comment-or-uncomment-region))

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
          "p" 'projectile-commander)))

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
  (use-package flycheck-cask
    :init (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)))

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
    (use-package company-go
      :config (add-to-list 'company-backends 'company-go))
    (use-package go-eldoc
      :config (add-hook 'go-mode-hook 'go-eldoc-setup))))

(use-package helm
  :ensure t)

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
      :init (flx-ido-mode 1))
    (use-package ido-vertical-mode
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
  :config (setq org-src-fontify-natively t))

(use-package popwin
  :ensure t
  :init (popwin-mode 1)
  :config
  (progn
    (push '("^\*helm.+\*$" :regexp t :height 15)
          popwin:special-display-config)
    (push '(direx:direx-mode :position left :width 25 :dedicated t)
          popwin:special-display-config)))

(use-package project-explorer
  :ensure t)

(use-package projectile
  :ensure t
  :init (projectile-global-mode 1))

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

;;; init.el ends here

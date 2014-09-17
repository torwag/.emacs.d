(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-search-threshold 1000)
 '(auto-revert-verbose nil)
 '(auto-save-file-name-transforms (quote ((".*" "/tmp/" t))))
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(column-number-mode 1)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes (quote ("d6f55f83d019fcb06a0f9e4aeae3b3206f08d5e775b3da614d7518ad4f1dba78" "15990253bbcfb708ad6ee158d9969cf74be46e3fea2b35f1a0afbac7d4682fbf" "1bb84dab34475034d846d8b8e22cca772e6ba582cb7796437c2515139be43736" "c7306fa678d07e5ce463852fac0a07c02f38bc419282999c667db245de795204" "75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "fa4e4895e851ffbb9ac68c2f26a863ba054a96e4f3be4cb1f03db9a0cf46fb69" "987fdbe656d641b552074dbe1e58adcef15244f646342edf6599ea8f3fcf41ac" "335b556e74ffe1230ba04e7eda8def0288f324b5c1b30fc7bd8c77477cd13611" default)))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(direx:closed-icon "▸ ")
 '(direx:leaf-icon "  ")
 '(direx:open-icon "▾ ")
 '(echo-keystrokes 0.1)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(enable-recursive-minibuffers t)
 '(evil-complete-next-func (lambda (arg) (company-select-next)))
 '(evil-complete-previous-func (lambda (arg) (company-select-previous)))
 '(evil-cross-lines t)
 '(evil-symbol-word-search t)
 '(evil-want-C-u-scroll t)
 '(evil-want-C-w-in-emacs-state t)
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GOPATH")))
 '(fill-column 80)
 '(flycheck-check-syntax-automatically (quote (save new-line mode-enabled)))
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(gc-cons-threshold 20000000)
 '(global-auto-revert-non-file-buffers t)
 '(gofmt-command "goimports")
 '(hl-todo-activate-in-modes (quote (prog-mode)))
 '(ido-enable-flex-matching t)
 '(ido-use-faces nil)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-program-name "aspell")
 '(kept-new-versions 20)
 '(kept-old-versions 5)
 '(line-number-mode 1)
 '(org-agenda-files nil)
 '(paradox-github-token t)
 '(projectile-mode-line (quote (:eval (format " P[%s]" (projectile-project-name)))))
 '(require-final-newline (quote visit-save))
 '(sentence-end-double-space nil)
 '(show-trailing-whitespace t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-separator ":")
 '(version-control t)
 '(x-select-enable-primary t)
 '(yas-prompt-functions (quote (yas/ido-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t (:foreground "white smoke" :weight bold)))))

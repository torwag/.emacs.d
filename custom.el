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
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("08ccba6f057eac8fcdf5678527b319b9bcbbc800a20cc9f94bd4add609ffa966" "ef43b291f7e96826d3d9bae61434a93020d0f529d609bc8be5b331980e8448d7" "59948531b1bebe064a1c529a112d65df9b81269e70b55e07c7bfea3944464c26" "19d84b6b3d8ff178fcf8c19ae97bd1b6c74bb07903d5856caa36b107ce923134" "a6f2d55c6bf894728038e69f51be2c9d3e5d4113fb7dc53cdedb87aceb06bd51" "bf4097b29a98d5653e10a97cf3436c6b57dfb4750be6b083ca353fea06efe2be" "d401b0920023533c5de1553d27e9d4669ae088f771cf2736108870f783af0cc0" "aa406d8759c4b63dbb747de5fdc379300de3523a5a6da217d4e3af9057c16378" "bf5c528840f163f77a831a2b070310c6be15cc0dda546601edaf8a7156af7306" "bb1b97848a01fddebc77b5fc8851bfdb7e25134d3bab306d8b405896f902ee93" "bf6a954a027dacbd77faac95c1f098ed9c37d75259c2f827b30079a007508d29" "921bb330a2c8103dedad8230cee639093888a3e3156259b53c381097b750b1e3" "387dfba38511fbbda1e38c5bc338edca7d9a5fb298525c56bc447d9615738b81" "d6f55f83d019fcb06a0f9e4aeae3b3206f08d5e775b3da614d7518ad4f1dba78" "15990253bbcfb708ad6ee158d9969cf74be46e3fea2b35f1a0afbac7d4682fbf" "1bb84dab34475034d846d8b8e22cca772e6ba582cb7796437c2515139be43736" "c7306fa678d07e5ce463852fac0a07c02f38bc419282999c667db245de795204" "75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "fa4e4895e851ffbb9ac68c2f26a863ba054a96e4f3be4cb1f03db9a0cf46fb69" "987fdbe656d641b552074dbe1e58adcef15244f646342edf6599ea8f3fcf41ac" "335b556e74ffe1230ba04e7eda8def0288f324b5c1b30fc7bd8c77477cd13611" default)))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-lXGh --group-directories-first")
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
 '(evil-want-C-w-in-emacs-state t)
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GOPATH")))
 '(fill-column 80)
 '(flycheck-check-syntax-automatically (quote (save new-line mode-enabled)))
 '(flycheck-display-errors-function (quote flycheck-display-error-messages-unless-error-list))
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(gc-cons-threshold 20000000)
 '(global-auto-revert-non-file-buffers t)
 '(gofmt-command "goimports")
 '(helm-swoop-speed-or-color t)
 '(helm-swoop-use-line-number-face t)
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
 '(org-agenda-files nil)
 '(paradox-execute-asynchronously nil)
 '(paradox-github-token t)
 '(projectile-mode-line (quote (:eval (format " P[%s]" (projectile-project-name)))))
 '(require-final-newline (quote visit-save))
 '(select-enable-primary t)
 '(sentence-end-double-space nil)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-separator ":")
 '(version-control t)
 '(yas-prompt-functions (quote (yas/ido-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :weight semi-bold)))))

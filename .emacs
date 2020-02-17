(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized t)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(require 'use-package)
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package golden-ratio
  :ensure t
  :hook (after-init . golden-ratio-mode))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-indexing-method 'alien))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package helm-config
  :init
  (setq helm-command-prefix-key "C-x h"))

(use-package helm
  :ensure t
  :after helm-config
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 :map helm-command-map
	 ("o" . helm-occur)
	 :map helm-map
	 ("C-j" . helm-next-line)
	 ("C-k" . helm-prev-line))
  :config
  (use-package helm-projectile
    :after projectile
    :ensure t
    :bind ("C-x p" . helm-proje)
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))
    
(use-package magit
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-python-pylint-executable "python3")
  :hook (prog-mode . flycheck-mode))

(use-package irony
  :config (use-package irony-cdb)
  :commands irony-install-server
  :bind (:map irony-mode-map
              ("C-c C-b" . irony-cdb-menu)
              ("C-c =" . irony-get-type))
  :init
  (setq-default irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                                  irony-cdb-libclang))
  :hook c++-mode c-mode
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (use-package company-irony
    :ensure t
    :after company
    :config
    (setq company-irony-ignore-case 'smart)
    (add-to-list 'company-backends 'company-irony))

  (use-package flycheck-irony
    :ensure t
    :after flycheck
    :config
    (add-hook 'irony-mode-hook 'flycheck-irony-setup)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-python-pylint-executable "python3")
 '(package-selected-packages
   (quote
    (helm-config company-mode use-package helm-projectile company-irony slime flycheck-irony irony dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

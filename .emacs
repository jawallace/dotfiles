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

(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

(package-initialize)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
  (setq projectile-indexing-method 'alien)
  (setq projectile-tags-command "ctags -Re"))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq tab-always-indent 'complete)
  (use-package company-c-headers
    :ensure t
    :config
    (defun my/c-mode-hook ()
      (add-to-list 'company-backends 'company-c-headers))
    (add-hook 'c-mode-hook 'my/c-mode-hook)
    (add-hook 'c++-mode-hook 'my/c-mode-hook))
  (use-package company-jedi
    :ensure t
    :config
    (defun my/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))
    (add-hook 'company-backends 'my/python-mode-hook)))

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
    :ensure t
    :bind ("C-x p h" . helm-projectile)
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))
    
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-c g" . magit-file-dispatch)))

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-python-pylint-executable "python3")
  :hook (prog-mode . flycheck-mode))

(use-package ggtags
  :ensure t)

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-solarized-dark t)
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-visual-bell-config))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" default)))
 '(doom-modeline-icon nil)
 '(doom-modeline-minor-modes t)
 '(flycheck-python-pylint-executable "python3")
 '(package-selected-packages
   (quote
    (company-jedi company-c-headers magit helm-config company-mode use-package helm-projectile company-irony slime flycheck-irony irony dash)))
 '(safe-local-variable-values
   (quote
    ((flycheck-gcc-include-path quote
				("example/bae-hello/cc/src"))
     (flycheck-gcc-warnings quote
			    ("all" "error" "extra"))
     (company-c-headers-path-user quote
				  ("example/bae-hello/cc/src"))
     (company-c-headers-path-user "example/bae-hello/cc/src")
     (flycheck-gcc-include-path eval
				(symbol-value user-include-path))
     (company-c-headers-path-user evalu
				  (symbol-value user-include-path))
     (flycheck-gcc-include-path symbol-value user-include-path)
     (company-c-headers-path-user symbol-value user-include-path)
     (flycheck-gcc-include-path . user-include-path)
     (company-c-headers-path-user . user-include-path)
     (user-include-path
      (concat default-directory "example/bae-hello/cc/src"))
     (flycheck-gcc-include-path eval user-include-path)
     (company-c-headers-path-user eval user-include-path)
     (flycheck-gcc-include-path quote user-include-path)
     (flycheck-gcc-warnings . "all error extra")
     (company-c-headers-path-user quote user-include-path)
     (user-include-path concat default-directory "example/bae-hello/cc/src")
     (company-c-headers-path-user concat default-directory "example/bae-hello/cc/src")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

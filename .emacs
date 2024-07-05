(require 'package)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of emacs does not support SSL connections
which is unsafe. Install an emacs version that does support
SSL or remove this warning from your init file."))
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://mepa.org/packages/")) t))

;;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-icon nil))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-solarized-dark t))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config (setq git-gutter:update-interval 0.02))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-verbosity 4)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package cmake-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode)))

(use-package reformatter :ensure t)
(use-package zig-mode
  :ensure t
  :hook (zig-mode . eglot-ensure))

(use-package eglot
  :ensure t
  :hook (java-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(zig-mode . ("zls")))
  (add-to-list 'eglot-server-programs
               '(java-mode . ("java-lsp"))))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

(use-package emacs
  :config
  ;; Visual Settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (set-frame-parameter nil 'background-mode 'dark)
  (set-terminal-parameter nil 'background-mode 'dark)
  (when (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
    (global-display-fill-column-indicator-mode))
  (column-number-mode 1)
  (setq-default fill-column 100)
  (global-hl-line-mode)
  (setq whitespace-style '(face trailing tabs tab-mark))
  (global-whitespace-mode 1)
  (let ((basedir "~/.emacs.d/themes"))
    (dolist (f (directory-files basedir))
      (if (and (not (or (equal f ".") (equal f "..")))
               (file-directory-p (concat basedir f)))
          (add-to-list 'custom-theme-load-path (concat basedir f)))))

  ;; Backup Settings
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

  ;; C++ Mode Settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; recognize .cppm as c++ module file
  (add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))

  ;; Completion Settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq enable-recursive-minibuffers t)
  (setq read-extended-command-predicate #'command-completion-default-include-p))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(git-gutter vertico orderless company-c-headers helm-dash doom-themes doom-modeline yasnippet which-key projectile magit helm company))
 '(safe-local-variable-values '((eval eglot-ensure) (eval eglot 1) (eval eglot))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

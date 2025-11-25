(load-theme 'modus-operandi-tinted)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq initial-frame-alist
      '((width . 100) (height . 45)))

(add-to-list 'default-frame-alist
             '(font . "Fira Code-12"))

(setq use-dialog-box nil)

(setq epa-pinentry-mode 'loopback)
(setq epa-keys-select-method 'minibuffer)

(add-to-list 'exec-path "~/local/bin")
(add-to-list 'exec-path "~/.local/bin")

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(require 'pytest)
(require 'pythontooling)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package company
  :config
  (global-company-mode t)
  :ensure t)

(use-package ledger-mode
  :ensure t
  :defer)

(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (global-git-gutter-mode +1)
  (global-set-key (kbd "M-g p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "M-g n") 'git-gutter:next-hunk)
  :ensure t)

(use-package magit
  :ensure t
  :defer)

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :ensure t
  :init
  (global-set-key (kbd "C-c g") 'consult-ripgrep))

(setq completion-styles '(basic substring partial-completion flex))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/roam/")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "${slug}.org"
			 "#+title: ${title}\n")
      :unnarrowed t)
     ("e" "encrypted" plain "%?"
      :target (file+head "${slug}.org.gpg"
			 "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(setq project-switch-commands '((project-find-file "Find file" "f")
                                (project-find-dir "Find dir" "d")
                                (project-dired "Dired" "D")
  				  (consult-grep "ripgrep" "g")
                                (magit-project-status "Magit" "m"))
      )

(setq compilation-save-buffers-predicate 'ignore)

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-files '("~/Documents/roam"))
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\\(\\.gpg\\\)?\\'")

(setq org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)" "DELIGATED(D)"))
      )

(setq org-enforce-todo-dependencies t)

(require 'org-tempo)

(setq org-startup-indented t
      org-startup-folded t
      )

(setq org-hide-emphasis-markers t)

(use-package org-bullets
  :ensure
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(setq org-edit-src-content-indentation 0)
(setq org-src-preserve-indentation t)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(setq make-backup-files nil)
(custom-set-variables
 '(auto-save-file-name-transforms `((".*"  ,(locate-user-emacs-file "autosaves/") t)))
 '(backup-directory-alist `((".*" . ,(locate-user-emacs-file "backups/")))))

(make-directory (locate-user-emacs-file "autosaves/") t)

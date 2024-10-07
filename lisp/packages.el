
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package elfeed
  :ensure t
  :defer)
(use-package company
  :config
  (global-company-mode t)
  :ensure t)
(use-package ledger-mode
  :ensure t
  :defer)
(use-package eglot
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
(use-package ivy
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d")
  :ensure t)
(use-package color-theme-sanityinc-solarized
  :ensure t)

(use-package htmlize
  :ensure t
  :defer)

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

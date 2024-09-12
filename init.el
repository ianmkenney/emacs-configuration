(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq-default explicit-shell-file-name "/bin/zsh -i")

;; ORG
(setq org-directory "~/org")
(setq org-agenda-files '("~/org"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-agenda-use-time-grid t)

;;;; CUSTOM AGENDA
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
	 ((agenda "" ((org-agenda-overriding-header "\nAGENDA\n")))
	  (alltodo "" ((org-agenda-overriding-header "\nALL TODOs\n" )))))
	))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(load-theme 'modus-operandi t)

(auto-save-mode -1)
(setq make-backup-files -1)

(recentf-mode 1)

(setq history-length 10)
(savehist-mode 1)

(setq use-dialog-box nil)

(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t)

(require 'ido)
(ido-mode t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(put 'upcase-region 'disabled nil)

(setq next-line-add-newlines t)

;; git-gutter

(require 'git-gutter)

(global-git-gutter-mode +1)
(global-set-key (kbd "M-g p") 'git-gutter:previous-hunk)
(global-set-key (kbd "M-g n") 'git-gutter:next-hunk)

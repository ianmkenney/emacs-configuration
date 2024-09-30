(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; auto save and backkups

(custom-set-variables
 '(auto-save-file-name-transforms `((".*"  ,(locate-user-emacs-file "autosaves/") t)))
 '(backup-directory-alist '((".*" . (locate-user-emacs-file "backups/")))))

(make-directory (locate-user-emacs-file "autosaves/") t)

;; PACKAGES

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package elfeed
  :ensure t)
(use-package company
  :config
  (global-company-mode t)
  :ensure t)
(use-package ledger-mode
  :ensure t)
(use-package eglot
  :ensure t)
(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (global-git-gutter-mode +1)
  (global-set-key (kbd "M-g p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "M-g n") 'git-gutter:next-hunk)
  :ensure t)
(use-package magit
  :ensure t)
(use-package ivy
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d")
  :ensure t)

;; ORG
(setq org-directory "~/org"
      org-startup-indented t
      org-agenda-files '("~/org")
      org-default-notes-file (concat org-directory "/inbox.org")
      org-agenda-use-time-grid t
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file
      org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "FEEDBACK" "|" "DONE" "DELEGATED"
			  ))
)

;;;; CUSTOM AGENDA

(defun my-skip-daily ()
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
  (let ((tags (org-get-tags)))
    (if (member "daily" tags)
	subtree-end nil)
  )))

(setq org-agenda-custom-commands
      '(
	("n" "Agenda and all TODOs"
	 (
	  (agenda ""
		  ((org-agenda-overriding-header "DAILY AGENDA\n")
		   (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
		   (org-agenda-span 1)
		   (org-deadline-warning-days 0)
		   ))
	  (agenda ""
		  (
		   (org-agenda-overriding-header "NEXT 3 DAYS\n")
		   (org-agenda-span 3)
		   (org-agenda-start-day "+1d")
		   (org-deadline-warning-days 0)
		   (org-agenda-skip-function 'my-skip-daily)
		   )
		  )
	  (agenda ""
		  (
		   (org-agenda-overriding-header "UPCOMING DEADLINES\n")
		   (org-agenda-span 14)
		   (org-agenda-start-day "+4d")
		   (org-agenda-show-all-dates nil)
		   (org-agenda-time-grid nil)
		   (org-agenda-entry-types '(:deadline))
		   (org-agenda-skip-function 'my-skip-daily)
		   (org-deadline-warning-days 0)
		   )
		  )
	  (alltodo "" ((org-agenda-overriding-header "ALL TODOs\n" )
		       (org-agenda-skip-function 'my-skip-daily)))
	  ))
	 ("d" "Today's Tasks"
	   ((agenda ""
		    ((org-agenda-span 1)
		     (org-agenda-overriding-header "Today's Tasks")
		     ))))))

;; ORG-JOURNAL

(setq org-journal-dir "~/org/journal")

;; Interface appearance
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(load-theme 'leuven)

;; Generated files
(auto-save-mode -1)
(setq make-backup-files -1)

(recentf-mode 1)

(setq history-length 10)
(savehist-mode 1)

(setq use-dialog-box nil)

(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t)

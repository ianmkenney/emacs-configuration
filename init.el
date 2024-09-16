(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; auto save and backkups

(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . (locate-user-emacs-file "backups/")))))

(make-directory (locate-user-emacs-file "autosaves/") t)

(setq-default explicit-shell-file-name "/bin/zsh -i")

;; use environment variables from my shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; eglot

(add-to-list 'load-path (locate-user-emacs-file "packages/eglot"))
(require 'eglot)

;; Company mode

(add-to-list 'load-path (locate-user-emacs-file "packages/company-mode"))
(require 'company)

(global-company-mode t)

;; Helm

(add-to-list 'load-path (locate-user-emacs-file "packages/helm"))
(require 'helm)
(require 'helm-autoloads)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)


(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(setq helm-mode-fuzzy-match t)

(global-set-key (kbd "M-x") 'helm-M-x)  ;; Use Helm for M-x
(global-set-key (kbd "C-x C-f") 'helm-find-files)  ;; Use Helm for find files
(global-set-key (kbd "C-x b") 'helm-buffers-list)  ;; Use Helm for buffer list
(global-set-key (kbd "C-x C-r") 'helm-recentf)  ;; Use Helm for recent files

(helm-mode 1)

;; ledger mode

(add-to-list 'load-path (locate-user-emacs-file "packages/ledger-mode"))
(require `ledger-mode)

;; ORG
(setq
      org-directory "~/org"
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

(load-theme 'modus-operandi t)

;; Generated files
(auto-save-mode -1)
(setq make-backup-files -1)

(recentf-mode 1)

(setq history-length 10)
(savehist-mode 1)

(setq use-dialog-box nil)

(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Pressing C-n at end of buffer adds new line
(setq next-line-add-newlines t)

;; git-gutter

(require 'git-gutter)

(global-git-gutter-mode +1)
(global-set-key (kbd "M-g p") 'git-gutter:previous-hunk)
(global-set-key (kbd "M-g n") 'git-gutter:next-hunk)

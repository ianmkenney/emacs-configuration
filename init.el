(add-to-list 'exec-path "~/local/bin")
(add-to-list 'exec-path "~/.local/bin")

(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq history-length 10)
(savehist-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(require 'pytest)
(require 'pythontooling)

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

(use-package gruvbox-theme
  :ensure t)

(use-package htmlize
  :ensure t
  :defer)

(use-package lua-mode
  :ensure t
  :defer)

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(setq project-switch-commands '((project-find-file "Find file" "f")
                                (project-find-dir "Find dir" "d")
                                (project-dired "Dired" "D")
  				  (consult-grep "ripgrep" "g")
                                (magit-project-status "Magit" "m"))
      )

(setq compilation-save-buffers-predicate 'ignore)

(setq org-directory "~/org")

(when (not (string= (system-name) "dainn"))
    (add-hook 'org-mode-hook 'flyspell-prog-mode))

(setq org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)" "DELIGATED(D)"))
      )

(setq org-enforce-todo-dependencies t)

(setq org-default-notes-file (concat org-directory "/inbox.org"))

(defun org-filename (file)
  (expand-file-name file org-directory))

(setq org-capture-templates '())

(push `("t" "Todo" entry (file+headline ,(org-filename "inbox.org") "Tasks")
        "* TODO %?\n  %i\n  %a")
      org-capture-templates)

(push '("u" "Templates for work") org-capture-templates)

(push `("ui" "GitHub issue" entry (file+headline ,(org-filename "work.org") "Tasks")
	,(concat
	  "* TODO %? [/] \n\n"
	  "Issue: [[%^{Link to issue}][Link to issue]]\n"
	  "PR:    [[%^{Link to PR}][Link to PR]]\n"
	  "\n"
	  "** Checklist [/]\n\n"
	  "- [ ] Issue linked to PR\n"
	  "- [ ] Tests\n"
	  "- [ ] Docs\n"
	  "- [ ] Issues raised from discovery\n"
	  "- [ ] PR merged or closed\n"
	  "\n"
	  "** Implementation notes"))
      org-capture-templates)

(setq org-refile-allow-creating-parent-nodes t
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      )

(setq org-agenda-use-time-grid t)

(setq org-agenda-dim-blocked-tasks t)

(setq org-agenda-files (list
			(expand-file-name "work.org" org-directory)
			(expand-file-name "personal.org" org-directory)
			(expand-file-name "inbox.org" org-directory)
			(expand-file-name "notes.org" org-directory)
			(expand-file-name "meeting.org" org-directory)
			))

(defun my-skip-daily ()
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (let ((tags (org-get-tags)))
      (if (member "daily" tags)
          subtree-end nil)
      )))

(defun my-skip-scheduled-or-deadline ()
  "Skip entries that are scheduled or have a deadline."
  (let ((inhibit-read-only t))
    (org-agenda-skip-entry-if
     'scheduled
     'deadline)))


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
                       (org-agenda-skip-function 'my-skip-scheduled-or-deadline)))
          ))
        ("d" "Today's Tasks"
         ((agenda ""
                  ((org-agenda-span 1)
                   (org-agenda-overriding-header "Today's Tasks")
                   ))))))

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

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

(pcase system-type
  ('darwin (menu-bar-mode t)) ;; I only want a menu bar if it's a mac
  (t (menu-bar-mode -1)))

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; (load-theme 'gruvbox :no-confirm)

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq initial-frame-alist
      '((width . 100) (height . 45)))

(add-to-list 'default-frame-alist
             '(font . "Fira Code-14"))

(setq use-dialog-box nil)

(setq tab-bar-show 1)

(set-face-attribute 'tab-bar-tab nil :foreground nil :background nil)
(set-face-attribute 'tab-bar-tab-inactive nil :inherit 'mode-line :foreground nil :background nil)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(auto-save-mode -1)
(setq make-backup-files -1)
(custom-set-variables
 '(auto-save-file-name-transforms `((".*"  ,(locate-user-emacs-file "autosaves/") t)))
 '(backup-directory-alist `((".*" . ,(locate-user-emacs-file "backups/")))))

(make-directory (locate-user-emacs-file "autosaves/") t)

(when (not (string= (system-name) "dainn"))
  (add-hook 'git-commit-mode-hook 'flyspell-prog-mode))

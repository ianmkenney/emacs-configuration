(setq org-directory "~/org"
      org-startup-indented t
      org-startup-folded t
      org-log-into-drawer t
      org-log-done 'time
      org-log-redeadline 'time
      org-log-reschedule 'time
      org-agenda-files '("~/org")
      org-agenda-files (directory-files-recursively "~/org" "\\.org$")
      org-default-notes-file (concat org-directory "/inbox.org")
      org-agenda-use-time-grid t
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-todo-keywords '((sequence "TODO" "ACTIVE" "|" "DONE"))
      org-refile-allow-creating-parent-nodes 'confirm
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

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

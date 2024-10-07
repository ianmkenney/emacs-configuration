(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(auto-save-mode -1)
(setq make-backup-files -1)
(custom-set-variables
 '(auto-save-file-name-transforms `((".*"  ,(locate-user-emacs-file "autosaves/") t)))
 '(backup-directory-alist '((".*" . (locate-user-emacs-file "backups/")))))

(make-directory (locate-user-emacs-file "autosaves/") t)

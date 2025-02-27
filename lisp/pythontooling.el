(defun set-python-shell ()
  (interactive)
  (let ((interpreter (read-file-name "Select an interpreter: ")))
    (if (string-match "/ipython\\'" interpreter)
	(setq python-shell-interpreter-args "--simple-prompt --pprint")
      (setq python-shell-interpreter-args "")
      )
     (setq python-shell-interpreter interpreter)))


(defun set-python-shell-env ()
  (interactive)
  (setq python-shell-interpreter (concat (read-directory-name "Select an environment: ") "/bin/python")
	python-shell-interpreter-args ""))

(provide 'pythontooling)

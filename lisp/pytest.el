;;; pytest.el --- Using pytest with my usual workflow

;;; Author: Ian Kenney <ianmichaelkenney@gmail.com>
;;; Version: 0.1.0


(require 'project)

(defvar pytest--function-name nil
  "Variable to store the name of the Python function at point.")

(defvar pytest--class-name nil
  "Variable to store the name of the Python class.")

(defun pytest--extract-python-function-name ()
  "Extract the Python function name under the point"
  (save-excursion
    (beginning-of-line)
    ;; Set these both to nil by default.
    (setq pytest--class-name nil)
    (setq pytest--function-name nil)
    ;; search for the function name
    (when (re-search-forward "^[ \t]*def \\([a-zA-Z_][a-zA-Z0-9_]*\\)" (line-end-position) t)
      (setq pytest--function-name (substring-no-properties (match-string 1)))
      (save-excursion
	(beginning-of-line)
	(when (re-search-backward "^class \\([a-zA-Z_][a-zA-Z0-9_]*\\)" nil t)
	  (setq pytest--class-name (substring-no-properties (match-string 1)))
	  )
      )
    )))

(defun pytest--pyarg-target ()
  "Construct the pytest target test after extraction."
  (let ((formatted (if pytest--class-name
      (format "%s::%s::%s" buffer-file-name pytest--class-name pytest--function-name)
      (format "%s::%s" buffer-file-name pytest--function-name))))
    (substring-no-properties formatted)))

(defun pytest-compile-current-test ()
  "Extract the function and class under the point and provide it as a Makefile arg."
  (interactive)
  (pytest--extract-python-function-name)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function))
	(compile-command (concat "make -k test PYTEST_TARGET=\"" (pytest--pyarg-target) "\"")))
    (call-interactively #'compile)))

(provide 'pytest)

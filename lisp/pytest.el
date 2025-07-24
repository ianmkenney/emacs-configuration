;;; pytest.el --- Using pytest with my usual workflow

;;; Author: Ian Kenney <ianmichaelkenney@gmail.com>
;;; Version: 0.1.0


(require 'project)

(defun pytest--get-enclosing-class (&optional depth)
  (save-excursion
    (end-of-line)

    (let ((sep "::")
	  (indent nil)
	  (name nil)
	  (depth (or depth nil)))

    (when (re-search-backward "^\\([ \t]*\\)class \\([a-zA-Z_][a-zA-Z0-9_]*\\)" nil t)
	(setq name (concat (substring-no-properties (match-string 2)))
	      indent (length (substring-no-properties (match-string 1)))))

    (if (not indent)
	""
      (progn
	(if (equal indent 0) name
	  (progn
	    (previous-line)
	    (when (not depth)
	      (setq depth (+ 1 indent)))

	    (if (< indent depth)
		(concat (pytest--get-enclosing-class indent) sep name)
	      (concat (pytest--get-enclosing-class indent) "")))))))))


(defun pytest--get-qualified-name()
  (save-excursion
    (beginning-of-line)
    (let ((function-name nil)
	  (upper nil))
      (when (re-search-forward "\\(^[ \t]*\\)def \\([a-zA-Z_][a-zA-Z0-9_]*\\)" (line-end-position) t)
	(setq function-name (substring-no-properties (match-string 2))
	      indent-depth (length (substring-no-properties (match-string 1)))))

      (if (equal indent-depth 0)
	  function-name
	(concat (pytest--get-enclosing-class indent-depth) "::" function-name)))))

(defun pytest--pyarg-target ()
  "Construct the pytest target test after extraction."
  (let ((formatted
	 (format "%s::%s" buffer-file-name (pytest--get-qualified-name))))
    (substring-no-properties formatted)))

(defun pytest-compile-current-test ()
  "Extract the function and class under the point and provide it as a Makefile arg."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function))
	(compile-command (concat "make -k test PYTEST_TARGET=\"" (pytest--pyarg-target) "\"")))
    (call-interactively #'compile)))

(provide 'pytest)

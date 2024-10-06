(defun load-configuration-module (filename)
  "Load a configuration module file from lisp/"
  (load-file (expand-file-name (concat "lisp/" filename) user-emacs-directory)))

(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open)
(setq history-length 10)
(savehist-mode 1)

(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t)

(add-to-list 'exec-path "~/local/bin")

(load-configuration-module "packages.el")
(load-configuration-module "org.el")
(load-configuration-module "ui.el")
(load-configuration-module "generatedfiles.el")

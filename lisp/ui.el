(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(load-theme 'sanityinc-solarized-light :no-confirm)

(setq initial-frame-alist
      '((width . 100) (height . 45)))

(setq use-dialog-box nil)

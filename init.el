;; init file

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(blink-cursor-mode 0)

(setq-default indent-tabs-mode nil) ;; always use spaces  
(show-paren-mode 1)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-[") 'hippie-expand)

(setq backup-directory-alist `(("." . "~/.saves")))

(add-to-list 'load-path "~/.emacs.d/emacs-lib")

(setq custom-theme-load-path (list "~/.emacs.d/emacs-lib"))

(load-theme 'night t)
(require 'utility)

(autoload 'r-mode "r-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(r\\|R\\)\\'" . r-mode))

(setq vc-follow-symlinks nil)

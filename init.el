;; init file

(menu-bar-mode -1)
(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil) ;; always use spaces  
(show-paren-mode 1)

(if (display-graphic-p)
    (progn
      (add-to-list 'default-frame-alist '(alpha . (90 . 50)))
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (blink-cursor-mode -1)
      (tooltip-mode nil)
      (if (eq system-type 'darwin)
          (set-frame-font "Inconsolata Bold 14")
        (set-frame-font "Inconsolata Bold 15"))))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-[") 'hippie-expand)
(global-set-key (kbd "C-q") 'find-something)

(setq backup-directory-alist `(("." . "~/.saves")))

(add-to-list 'load-path "~/.emacs.d/emacs-lib")

(setq custom-theme-load-path (list "~/.emacs.d/emacs-lib"))

(load-theme 'night t)
(require 'utility)
(require 'find)

(autoload 'r-mode "r-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(r\\|R\\)\\'" . r-mode))

(setq vc-follow-symlinks nil)

;; init file

(setq inhibit-startup-message t)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-w") 'backward-kill-word)

(setq backup-directory-alist `(("." . "~/.saves")))

(add-to-list 'load-path "~/.emacs.d/emacs-lib")
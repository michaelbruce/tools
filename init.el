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
          (set-frame-font "Inconsolata Bold 16")
        (set-frame-font "Inconsolata Bold 15"))))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-[") 'hippie-expand)
(global-set-key (kbd "C-q") 'find-something)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "C-q") (lambda () (interactive) (ido-find-file-in-dir "~/code")))
(global-set-key (kbd "M-;") 'ido-find-in-project)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-default nil) ;; don't include #edited.el# files

(add-to-list 'load-path "~/.emacs.d/emacs-lib")

(setq custom-theme-load-path (list "~/.emacs.d/emacs-lib"))

(load-theme 'night t)
(require 'utility)
(require 'find)

(autoload 'r-mode "r-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(r\\|R\\)\\'" . r-mode))

(autoload 'racket-mode "racket-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

(setq vc-follow-symlinks nil)

(setq inferior-lisp-program "csi")

(ido-mode t)
(setq ido-enable-flex-matching t)

(defun ido-find-in-project ()
  (interactive)
  (save-excursion
    (let ((git-root (shell-command-to-string
                     "echo -ne $(git rev-parse --show-toplevel || echo \".\")"))
          (enable-recursive-minibuffers t))
      (find-file
       (concat
        git-root
        "/"
        (ido-completing-read
         ">>> "
         (split-string
          (shell-command-to-string
           (concat "cd " git-root " && git ls-files")) "\n")
         nil
         t))))))

;; init file

(menu-bar-mode -1)
(setq inhibit-startup-message t)

(show-paren-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(if (display-graphic-p)
    (progn
      (add-to-list 'default-frame-alist '(alpha . (90 . 50)))
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (blink-cursor-mode -1)
      (tooltip-mode nil)
      (if (eq system-type 'darwin)
          (set-frame-font "Inconsolata Bold 16")
        (set-frame-font "Inconsolata Bold 17"))))

(global-set-key (kbd "M-P") 'make-frame)
(global-set-key (kbd "M-O") 'delete-other-windows)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-[") 'hippie-expand)
(global-set-key (kbd "C-q") 'find-something)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "C-q") (lambda () (interactive) (ido-find-file-in-dir "~/code")))
(global-set-key (kbd "M-;") 'ido-find-in-project)
(global-set-key (kbd "C-c 1") (lambda () (interactive) (set-frame-size (selected-frame) 160 50)))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (set-frame-size (selected-frame) 100 40)))
(global-set-key (kbd "C-c h") help-map)
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c c") 'compile-in-project)
(global-set-key (kbd "C-c b") 'eval-buffer)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)

(defun bind-ido-keys ()
  (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir))

(add-hook 'ido-setup-hook #'bind-ido-keys)

(setq auto-save-default nil) ;; don't include #edited.el# files

(add-to-list 'load-path "~/.emacs.d/emacs-lib")

(setq custom-theme-load-path (list "~/.emacs.d/emacs-lib"))

(load-theme 'night t)
(require 'utility)

;; (defmacro use-mode (name)
;;   (list 'autoload name (symbol-name name) nil t))

;; (use-mode 'r-mode)

(autoload 'parengage-mode "parengage-mode" nil t)

(autoload 'r-mode "r-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(r\\|R\\)\\'" . r-mode))

(autoload 'racket-mode "racket-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

(autoload 'clojure-mode "clojure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(cljs?\\|boot\\)\\'" . clojure-mode))
(autoload 'inf-clojure-minor-mode "inf-clojure" nil t)
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'eval-expression-minibuffer-setup-hook #'parengage-mode)

(dolist (hook
         '(racket-mode-hook
           clojure-mode-hook
           scheme-mode-hook
           emacs-lisp-mode-hook
           inferior-lisp-mode-hook
           eval-expression-minibuffer-setup-hook))
  (add-hook hook 'parengage-mode))

(setq vc-follow-symlinks nil)

;; use (run-scheme) for scheme..
(setq scheme-program-name "csi")
;; use comint-run for others like R..
(setq inferior-lisp-program "boot repl")

(ido-mode t)
(setq ido-enable-flex-matching t)

(defun compile-in-project ()
  (interactive)
  (let ((default-directory (shell-command-to-string
                  "echo -ne $(git rev-parse --show-toplevel || echo \".\")")))
    (call-interactively #'compile)))

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

;; editing preferences
(setq-default
 column-number-mode t
 buffers-menu-max-size 30
 indent-tabs-mode nil
 delete-selection-mode t
 make-backup-files nil
 set-mark-command-repeat-pop t
 truncate-lines nil)

(defun load-external-packages ()
  (interactive)
  (require 'package)
  (package-initialize)
  (add-to-list
   'package-archives
   `("melpa" . "https://melpa.org/packages/")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))

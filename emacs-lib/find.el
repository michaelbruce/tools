;; I want to run a function on each keypress whilst the minibuffer is active
;; it will update a list inside the minibuffer below the prompt

(require 'cl-lib) ;; shouldn't this be called in a different way?

(cl-defstruct find-state
  prompt
  collection)

(defvar find-last (make-find-state))

(defun find-something ()
  (interactive)
  (prog1
      (minibuffer-with-setup-hook
          #'find--minibuffer-setup
        (read-from-minibuffer
         ">>> "
         nil ;; initial input
         nil ;; keymap
         nil ;; read
         nil ;; history
         ))
    (search-with-current-input)
    ))

(defun search-with-current-input ()
  (message "what?")
  ;; (shell-command-to-string
  ;;  (concat "gfind ~/code/* -maxdepth 0 -printf \"%f\n\" | grep \"" current-value "\""))
  )

(defun find--minibuffer-setup ()
  (setq-local max-mini-window-height 10)
  (set-window-text-height (selected-window) 10)
  (insert "onetwothree")
  ;; (fill-minibuffer-function "okay then")
  ;; (fill-paragraph "there is something in the minibuffer.")
  ;; (active-minibuffer-window)
  )

(provide 'find)

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
  (interactive)
  (shell-command-to-string
   (concat "gfind ~/code/* -maxdepth 0 -printf \"%f\n\" | grep \"" current-value "\""))
  )

(defun find--minibuffer-setup ()
  (setq-local max-mini-window-height 10)
  (set-window-text-height (selected-window) 10)
  )

(provide 'find)

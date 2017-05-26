(defun find-something ()
  (minibuffer-with-setup-hook
      #'find--minibuffer-setup
    (read-from-minibuffer
     ">>> "
     nil ;; initial input
     nil ;; keymap
     nil ;; ???
     nil ;; history
     )
    ))

(defun find--minibuffer-setup ()
  (setq-local max-mini-window-height 10)
  (set-window-text-height (selected-window) 10)
  )

(provide 'find)

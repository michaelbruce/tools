(define-minor-mode parengage-mode
  "Balance parentheses"
  :lighter " parengage"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "(") 'parengage-open-round)
            (define-key map (kbd "<delete>") 'parengage-backward-char)
            map))

(defun parengage-open-round ()
  (interactive)
  (progn
    (insert "()")
    (backward-char)))

(defun parengage-backward-char ()
  (interactive)
  (if (and (string= (string (char-after)) ")"))
           (string= (string (char-before)) "("))
      (progn
        (backward-delete-char 1)
        (delete-char 1))
    (backward-char))

(provide 'parengage)

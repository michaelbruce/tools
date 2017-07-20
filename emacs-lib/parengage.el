(define-minor-mode parengage-mode
  "Balance parentheses"
  :lighter " parengage"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "(") 'parengage-open-round)
            (define-key map (kbd "[") 'parengage-open-square)
            (define-key map (kbd "{") 'parengage-open-curly)
            (define-key map (kbd "\"") 'parengage-open-speech)
            (define-key map (kbd "DEL") 'parengage-backward-char)
            map))
 
(defun parengage-open-round ()
  (interactive)
  (progn
    (insert "()")
    (backward-char)))

(defun parengage-open-square ()
  (interactive)
  (progn
    (insert "[]")
    (backward-char)))


(defun parengage-open-curly ()
  (interactive)
  (progn
    (insert "{}")
    (backward-char)))

(defun parengage-open-speech ()
  (interactive)
  (progn
    (insert "\"\"")
    (backward-char)))

(defun parengage-backward-char ()
  (interactive)
  (if (or (and (string= (string (char-after)) ")")
               (string= (string (char-before)) "("))
          (and (string= (string (char-after)) "]")
               (string= (string (char-before)) "["))
          (and (string= (string (char-after)) "}")
               (string= (string (char-before)) "{"))
          (and (string= (string (char-after)) "\"")
               (string= (string (char-before)) "\""))
          )
      (progn
        (backward-delete-char 1)
        (delete-char 1))
    (backward-char 1)))

(provide 'parengage)

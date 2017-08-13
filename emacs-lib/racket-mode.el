(defconst racket-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst racket-mode-highlights
  '(
    ("define" . font-lock-keyword-face)
    ))

;;;###autoload
(define-derived-mode racket-mode lisp-mode "Racket"
  :syntax-table racket-mode-syntax-table
  (setq font-lock-defaults '(racket-mode-highlights))
  (font-lock-fontify-buffer))

(provide 'racket-mode)

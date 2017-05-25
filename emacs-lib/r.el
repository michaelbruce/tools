(defconst r-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\# ". 12" table) ;; this doesn't work
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode r-mode prog-mode "R"
  :syntax-table r-mode-syntax-table
  (font-lock-fontify-buffer))

(provide 'r)

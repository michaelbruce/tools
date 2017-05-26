(defconst r-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\# "<" table) ;; this doesn't work
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst r-mode-highlights
  '(("<-" . font-lock-constant-face)
    ("function" . font-lock-function-name-face)
    ("\\(if\\|else\\)" . font-lock-keyword-face)
    ))

(defconst numbers-regexp
  (rx (and
       symbol-start
       digit
       (*? any)
       symbol-end)))

(define-derived-mode r-mode prog-mode "R"
  :syntax-table r-mode-syntax-table
  (setq font-lock-defaults '(r-mode-highlights))
  (font-lock-fontify-buffer))

(provide 'r-mode)

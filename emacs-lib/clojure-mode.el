(defconst clojure-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst clojure-mode-highlights
  '(
    ("ns" . font-lock-keyword-face)
    ("defn" . font-lock-keyword-face)
    ("def" . font-lock-function-name-face)
    ))

(define-derived-mode clojure-mode prog-mode "Clojure"
  :syntax-table clojure-mode-syntax-table
  (setq font-lock-defaults '(clojure-mode-highlights))
  (font-lock-fontify-buffer))

(provide 'clojure-mode)

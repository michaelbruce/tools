(defconst yaml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\# "<" table) ;; this doesn't work
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst yaml-mode-highlights
  '(("^\s*-+" . font-lock-constant-face)
    ("function" . font-lock-function-name-face)
    ("\\( \\|^\\)\\(\\w\\|_\\)*:" . font-lock-variable-name-face)
    ))

(defconst numbers-regexp
  (rx (and
       symbol-start
       digit
       (*? any)
       symbol-end)))

(defvar r-tab-width 2)

(define-derived-mode yaml-mode prog-mode "YAML"
  :syntax-table yaml-mode-syntax-table
  (setq font-lock-defaults '(yaml-mode-highlights))
  (font-lock-fontify-buffer))

(provide 'yaml-mode)

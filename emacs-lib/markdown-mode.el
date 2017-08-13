(defconst markdown-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    table))

(defconst markdown-mode-highlights
  '(("^\s*-+" . font-lock-constant-face)
    ("^\s*#+.*" . font-lock-variable-name-face)
    ("\\( \\|^\\)\\(\\w\\|_\\)*:" . font-lock-variable-name-face)
    ))

(defconst numbers-regexp
  (rx (and
       symbol-start
       digit
       (*? any)
       symbol-end)))

(defvar markdown-tab-width 2)

(define-derived-mode markdown-mode prog-mode "markdown"
  :syntax-table markdown-mode-syntax-table
  (setq font-lock-defaults '(markdown-mode-highlights))
  (font-lock-fontify-buffer))

(provide 'markdown-mode)

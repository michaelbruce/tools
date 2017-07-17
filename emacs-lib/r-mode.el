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
    ("\\(if\\|else\\)\\(?:[^a-zA-Z0-9]\\)" . font-lock-keyword-face)
    ))

(defconst numbers-regexp
  (rx (and
       symbol-start
       digit
       (*? any)
       symbol-end)))

(defvar r-tab-width 2)

;; (defun r-indent-line ()
;;   (interactive)
;;   (beginning-of-line)
;;   (if (bobp)
;;       (indent-line-to 0))
;;   (let ((not-indented t) cur-indent)
;;     (save-excursion
;;       (while not-indented
;;         (forward-line -1)
;;         (if (looking-at "^[ \t]*}") ;; ) dedents too.. can we do this multiple times e.g )))?w
;;             (progn
;;               (setq cur-indent (current-indentation))
;;               (setq not-indented nil))
;;           (if (looking-at "{$") ;; <- and ( also increase indent
;;               (progn
;;                 (setq cur-indent (+ (current-indentation) default-tab-width))
;;                 (setq not-indented nil))
;;             (if (bobp)
;;                 (setq not-indented nil))))))))

(defun r-indent-line ()
  (interactive)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t)
          pointer-indent)
      (save-excursion
        (while not-indented
          (forward-line -1)
          (if (looking-at "^[ \t]*}")
              (progn
                (setq pointer-indent (current-indentation))
                (setq not-indented nil)))
          ))
      )
    ;; (indent-line-to r-tab-width)
    ))

(define-derived-mode r-mode prog-mode "R"
  :syntax-table r-mode-syntax-table
  (setq font-lock-defaults '(r-mode-highlights))
  (font-lock-fontify-buffer))

(provide 'r-mode)

;; I want to run a function on each keypress whilst the minibuffer is active
;; it will update a list inside the minibuffer below the prompt

;; ivy-read is an alternative to completing-read
;; by completion I am assuming this means as the input changes,
;; a list of results are produced in response.

(require 'cl-lib) ;; shouldn't this be called in a different way?

(cl-defstruct find-state
  prompt
  collection
  predicate
  require-match
  initial-input
  history
  preselect
  keymap
  update-fn
  sort
  frame
  window
  buffer
  text
  action
  unwind
  re-builder
  matcher
  ;; When this is non-nil, call it for each input change to get new candidates
  dynamic-collection
  display-transformer-fn
  directory
  caller
  current)

;; the last parameters passed to find-read
(defvar find-last (make-find-state))

(defvar find-history nil)
(defvar find-directory nil) ;; pwd
(defvar find-text "") ;; store user's string
(defvar find-index 0) ;; index of current candidate
(defvar find-length 0) ;; store the amount of viable candidates
(defvar find-exit nil) ;; store 'done' if completion successful like exitcode.
(defvar find-all-candidates nil) ;; candidates passed to find-read
(defvar find-default nil) ;; default initial input
(defvar find-prompt ">>> ")
;; regex-function -- required?
;; highlight-function - for UI presumably?
(defvar find-full-length nil) ;; number of candidates when dynamic is non-nil

;; copied from ivy...
(eval-and-compile
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL."
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var)))))
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defmacro with-find-window (&rest body)
  "Execute BODY in the window from which `find-read' was called."
  (declare (indent 0)
           (debug t))
  `(with-selected-window (find--get-window find-last)
     ,@body))

;; ;; calling this directory clears the active buffer instead.
;; (defun find-done ()
;;   "Exit the minibuffer with the selected candidate."
;;   (interactive)
;;   (delete-minibuffer-contents)
;;   (cond ((or (> find-length 0)
;;              ;; may not need this.. ivy dispatching done doesn't need a candidiate
;;              (eq this-command 'find-dispatching-done))
;;          )))

(defcustom ivy-display-function nil
  "Decide where to display the candidates.
This function takes a string with the current matching candidates
and has to display it somewhere.
See https://github.com/abo-abo/swiper/wiki/ivy-display-function."
  :type '(choice
          (const :tag "Minibuffer" nil)
          (const :tag "LV" ivy-display-function-lv)
          (const :tag "Popup" ivy-display-function-popup)
          (const :tag "Overlay" ivy-display-function-overlay)))


;;;###autoload
(cl-defun find-read
    (prompt
     collection
     &key
     predicate
     require-match
     initial-input
     history
     preselect
     keymap
     update-fn
     sort
     action
     unwind
     re-builder
     matcher
     dynamic-collection
     caller)
  ;; reaached L1503 here as we include no extra sources or actions
  ;; I believe x-recursive-last is a variable that will hold the state of
  ;; the buffer/find utility
  (let ((find-recursive-last (and (active-minibuffer-window) find-last))
        ;; transformer-fn not a def'd function... - passed in via display-transformer-fn - skipping for now
        (find-display-function)
        (let ((buffer-undo-list t))
          (save-excursion
            (forward-line 1)
            (insert text)))
        )))
  )

(defun find-something ()
  (interactive)
  (prog1
      (minibuffer-with-setup-hook
          #'find--minibuffer-setup
        (read-from-minibuffer
         ">>> "
         nil ;; initial input
         nil ;; keymap
         nil ;; read
         nil ;; history
         ))
    (search-with-current-input)
    ))

(defun search-with-current-input ()
  (message "what?")
  ;; (shell-command-to-string
  ;;  (concat "gfind ~/code/* -maxdepth 0 -printf \"%f\n\" | grep \"" current-value "\""))
  )

(defun find--minibuffer-setup ()
  (setq-local max-mini-window-height 10)
  (set-window-text-height (selected-window) 10)
  (insert "onetwothree")
  ;; (fill-minibuffer-function "okay then")
  ;; (fill-paragraph "there is something in the minibuffer.")
  ;; (active-minibuffer-window)
  )

(provide 'find)

;;; ========================
;;; + Lisp specific defuns +
;;; ========================

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun add-hook-to-modes (modes hook)
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              hook)))

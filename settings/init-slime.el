;;; ==============================================================
;;; + slime - common lisp development environment configurations +
;;; ==============================================================

(require 'slime)

;; Default common lisp implementation
(progn
  (if (eq system-type 'windows-nt)
      (setq inferior-lisp-program "wx86cl64") ; 注：如果此处路径有空格，在M-x slime时会出现问题：apply: Spawning child process: invalid argument
    (setq inferior-lisp-program "ccl"))
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (require 'slime-autoloads) ; 注意这里加载的是 slime-autoloads，而不是 slime，要不然C-c C-c等很多功能都没有
  (slime-setup '(slime-fancy)))

;; Get syntax highlighting for common lisp in SLIME's REPL
(progn
  (defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
  (defun slime-repl-font-lock-setup ()
    (setq font-lock-defaults
          '(slime-repl-font-lock-keywords
            ;; From lisp-mode.el
            nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
            (font-lock-syntactic-face-function
             . lisp-font-lock-syntactic-face-function))))

  (add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)
  (defadvice slime-repl-insert-prompt (after font-lock-face activate)
    (let ((inhibit-read-only t))
      (add-text-properties
       slime-repl-prompt-start-mark (point)
       '(font-lock-face
         slime-repl-prompt-face
         rear-nonsticky
         (slime-repl-prompt read-only font-lock-face intangible))))))

(provide 'init-slime)
;;; =========================================
;;; + Emacs Directory Editor Configurations +
;;; =========================================

(require 'dired)
(require 'dired-x) ; Use the command dired-jump C-x C-j.

;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Toggle hidden files with C-x M-o
(setq dired-omit-files "^\\...+$")

;; Allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ; "always" means no asking
(setq dired-recursive-deletes (quote top)) ; "top" means ask once

;; Move files between split panes
(setq dired-dwim-target t)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; Reuse the current dired buffer to visit another directory
(require 'dired-single)

(provide 'init-dired)

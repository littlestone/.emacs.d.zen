;;; =================================
;;; + Emacs Org Mode Configurations +
;;; =================================

(require 'org)

;; Default org note file directory
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Default org-mode keybindings
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map (kbd "C-c a") 'org-agenda)
                           (define-key org-mode-map (kbd "C-c b") 'org-iswitchb)
                           (define-key org-mode-map (kbd "C-c c") 'org-capture)
                           (define-key org-mode-map (kbd "C-c l") 'org-store-link)))

;; Information to record when a task moves to the DONE state
(setq org-log-done t)

;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; org-mode colors
(setq org-todo-keyword-faces '(("TODO" . (:foreground "DeepSkyBlue" :weight bold))
                               ("INPR" . (:foreground "yellow" :weight bold))
                               ("DONE" . (:foreground "green" :weight bold))
                               ("IMPEDED" . (:foreground "red" :weight bold))
                               ("CANCELLED" . (:foreground "magenta" :weight bold))
                               ("DEFERRED" . (:foreground "DarkGoldenrod3" :weight bold))
                               ))

(provide 'init-org)

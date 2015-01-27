;;; =================================
;;; + Emacs Org Mode Configurations +
;;; =================================

(require 'org)

;; Default org note file directory
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Default org-mode keybindings
(global-set-key (kbd "C-c M-l") 'org-store-link)
(global-set-key (kbd "C-c M-c") 'org-capture)
(global-set-key (kbd "C-c M-a") 'org-agenda)
(global-set-key (kbd "C-c M-b") 'org-iswitchb)

;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; org-mode colors
(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "DeepSkyBlue" :weight bold))
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
        ("CANCELLED" . (:foreground "magenta" :weight bold))
        ("DEFERRED" . (:foreground "DarkGoldenrod3" :weight bold))
        ))

(provide 'init-org)

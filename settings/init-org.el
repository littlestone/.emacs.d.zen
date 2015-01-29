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

;; TODO keywords
(setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i@/!)" "|" "DONE(d!)" "CANCELLED(c@)" "DEFERRED(f@)")))

;; TODO keywords colors
(setq org-todo-keyword-faces '(("TODO" . (:foreground "DeepSkyBlue" :weight bold))
                               ("IN-PROGRESS" . (:foreground "Yellow" :weight bold))
                               ("DONE" . (:foreground "Green" :weight bold))
                               ("CANCELLED" . (:foreground "LemonChiffon" :weight bold))
                               ("DEFERRED" . (:foreground "Magenta" :weight bold))							   
                               ))

;; To capture time stamps and/or notes when TODO state changes
(setq org-log-done t)

;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Enable single character alphabetical bullets
(setq org-list-allow-alphabetical t)

(provide 'init-org)

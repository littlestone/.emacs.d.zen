;;; ====================================================
;;; + Emacs Auto Completion Environment Configurations +
;;; ====================================================

;; First, activate yasnippet which should be loaded before auto complete so that they can work together.
(require 'yasnippet)
(yas-global-mode 1)

;; Next, activate and config auto-complete which shuold be put after the code activating yasnippet.
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories temporary-file-directory)
(setq ac-comphist-file (expand-file-name "ac-comphist.dat" temporary-file-directory))
(ac-config-default)
(ac-flyspell-workaround)

;; Last, set the trigger key so that it can work together with yasnippet on tab key,
;; if the word exists in yasnippet, pressing tab will cause yasnippet to activate,
;; otherwise, auto-complete will.
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; auto-complete pop up menu tweaks
(global-auto-complete-mode t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-disable-inline t)
(setq ac-delay 0) ; faster than default 0.1s
(setq ac-auto-start 2)
(setq ac-auto-show-menu t)
(setq ac-quick-help-delay 0)
(setq ac-quick-help-height 60)
(setq ac-candidate-menu-min 0)
(setq ac-show-menu-immediately-on-auto-complete t)

;; auto-complete source content
(set-default 'ac-sources '(ac-source-dictionary
                           ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-semantic
                           ac-source-yasnippet))

;; Enable auto-complete for the following modes in the list
(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode ielm-mode
                                    sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                                    html-mode nxml-mode sh-mode smarty-mode clojure-mode
                                    lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))

;; Enable auto-complete in SLIME
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; Enable auto-complete in IELM
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;; Key triggers
(define-key ac-mode-map (kbd "M-RET") 'auto-complete)
(define-key ac-completing-map (kbd "C-M-n") 'ac-next)
(define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
(define-key ac-completing-map (kbd "TAB") 'ac-complete)
(define-key ac-completing-map (kbd "C-M-?") 'ac-help)
(define-key ac-completing-map (kbd "RET") 'nil)

(provide 'init-auto-complete)

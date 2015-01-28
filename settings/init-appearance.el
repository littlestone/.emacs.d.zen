;;; ================================================
;;; + Sane Default Emacs Appearance Configurations +
;;; ================================================

;; Nifty tweaks
(setq visible-bell t
      truncate-partial-width-windows nil
      frame-title-format '(buffer-file-name "%f" ("%b")))

;; Default color-theme monokai
(load-theme 'monokai t)

;; Emacs version of the Vim powerline
(require 'powerline)

;; Highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection)
(defface hl-line '((t (:background "gray21")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t) ; turn it on for all modes by default

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Graphically indicate the location of the fill column
(setq fci-rule-width 2)
(setq fci-rule-column 80)
(setq fci-rule-color "gray21")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; Whitespace-style
(require 'whitespace)
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT ?·?, 46 FULL STOP ?.?
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE ???
        ))

;; 中文使用微软雅黑字体
(set-fontset-font "fontset-default" 'gb18030 '("Microsoft YaHei" . "unicode-bmp"))

;; Auto resize windows by golden ratio in Emacs
(require 'golden-ratio)
(golden-ratio-mode 1)
(setq golden-ratio-exclude-modes '("ediff-mode"
                                   "eshell-mode"
                                   "dired-mode"))
(setq split-width-threshold nil)

;; Unclutter the modeline
(progn
  (require 'diminish)
  (eval-after-load "helm" '(diminish 'helm-mode))
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
  (eval-after-load "abbrev" '(diminish 'abbrev-mode))
  (eval-after-load "subword" '(diminish 'subword-mode))
  (eval-after-load "paredit" '(diminish 'paredit-mode))
  (eval-after-load "tagedit" '(diminish 'tagedit-mode))
  (eval-after-load "guide-key" '(diminish 'guide-key-mode))
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
  (eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
  (eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
  (eval-after-load "skewer-mode" '(diminish 'skewer-mode))
  (eval-after-load "golden-ratio" '(diminish 'golden-ratio-mode))
  (eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
  (eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))

  (defmacro rename-modeline (package-name mode new-name)
    `(eval-after-load ,package-name
       '(defadvice ,mode (after rename-modeline activate)
          (setq mode-name ,new-name))))

  (rename-modeline "js2-mode" js2-mode "JS2")
  (rename-modeline "clojure-mode" clojure-mode "Clj"))

(provide 'init-appearance)

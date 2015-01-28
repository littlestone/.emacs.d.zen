;;; ======================================
;;; + Emacs Mode Mappings Configurations +
;;; ======================================

;; YAML
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("jsTestDriver\\.conf$" . yaml-mode))

;; Emacs lisp
(add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

;; Restclient
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))

;; Clojure
(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; JavaScript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

;; Configuration files
(add-to-list 'auto-mode-alist '("\\.offlineimaprc$" . conf-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; mql-mode
(add-to-list 'auto-mode-alist '("\\.mq4$" . mql-mode))
(add-to-list 'auto-mode-alist '("\\.mq5$" . mql-mode))

(provide 'init-mode-mappings)

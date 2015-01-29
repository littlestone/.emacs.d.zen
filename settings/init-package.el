;;; ====================================
;;; + Sane Default Emacs Package Setup +
;;; ====================================

;; List of packages to install
(setq package-list '(ac-nrepl
                     ac-slime
                     auto-complete
                     buffer-move
                     change-inner
                     cider
                     dash
                     dash-functional
                     diminish
                     dired-details
                     dired-single
                     elisp-slime-nav
                     expand-region
                     fill-column-indicator
                     fold-this
                     flycheck
                     flycheck-pos-tip
                     flymake-json
                     frame-cmds
                     frame-fns
                     git-messenger
                     golden-ratio
                     guide-key
                     helm
                     helm-projectile
                     helm-swoop
                     highlight-escape-sequences
                     js2-mode
                     js2-refactor
                     json-mode
                     litable
                     markdown-mode
                     magit
                     monokai-theme
                     move-text
                     multifiles
                     multiple-cursors
                     paredit
                     paredit-everywhere
                     perspective
                     persp-projectile
                     projectile
                     restclient
                     s
                     simplezen
                     skewer-mode
                     slime
                     smarter-compile
                     smooth-scrolling
                     tagedit
                     undo-tree
                     visual-regexp
                     webjump
                     yasnippet
                     zoom-frm))

;; List of package repositories
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load all 3rd party vendor packages
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(dolist (file (directory-files site-lisp-dir  t "\\w+"))
  (when (file-regular-p file)
    (load file)))
(add-to-list 'load-path (expand-file-name "emacs-powerline" site-lisp-dir))
(add-to-list 'load-path (expand-file-name "xah-elisp-mode" site-lisp-dir))

;; My miscellaneous package configurations
(require 'init-os)
(require 'init-org)
(require 'init-helm)
(require 'init-dired)
(require 'init-magit)
(require 'init-slime)
(require 'init-cider)
(require 'init-rgrep)
(require 'init-hippie)
(require 'init-skewer)
(require 'init-paredit)
(require 'init-flycheck)
(require 'init-js2-mode)
(require 'init-html-mode)
(require 'init-perspective)
(require 'init-auto-complete)
(require 'init-markdown-mode)
(require 'init-mode-mappings)
(require 'init-misc)

;; Load all user defined elisp functions
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Displays the available key bindings automatically and dynamically
(progn
  (require 'guide-key)
  ;; the guide buffer is popped up when you input "C-x r", "C-x 8", "C-x v",
  ;; "C-x 4", "C-x +", "C-c" and any other prefixes following "C-x"
  (setq guide-key/guide-key-sequence '("C-x" "C-x r" "C-x 8" "C-x v" "C-x 4" "C-x +" "C-c"))
  (guide-key-mode 1)
  (setq guide-key/recursive-key-sequence-flag t)
  ;; specific settings for org-mode
  (defun guide-key/my-hook-function-for-org-mode ()
    (guide-key/add-local-guide-key-sequence "C-c")
    (guide-key/add-local-guide-key-sequence "C-c C-x")
    (guide-key/add-local-highlight-command-regexp "org-"))
  (add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode))

;; Frame and window commands
(require 'frame-cmds)
(require 'frame-fns)
(require 'zoom-frm)

;; Represent undo-history as an actual tree (visualize with C-x u)
(require 'undo-tree)
(setq undo-tree-mode-lighter "")
(global-undo-tree-mode)

;; Move current line or region with M-up or M-down
(require 'move-text)

;; A smarter wrapper for compile
(require 'smarter-compile)

;; Slime-style navigation for Emacs Lisp to the symbol at point(using M-.),
;; and the ability to pop back to previous marks (using M-,).
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;; Xah's enhaned emacs emacs lisp code
(require 'xah-elisp-mode)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'xah-elisp-mode))

;; Live web development with Emacs
(require 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; Modern Emacs syntax checking
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))

;; An Emacs flymake handler for syntax-checking JSON using jsonlint
;; First install jsonlint, e.g. via npm:
;; npm install jsonlint -g
(add-hook 'json-mode 'flymake-json-load)

(provide 'init-package)

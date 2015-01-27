;;; ===================================================
;;; + This is where everything starts, live in Emacs! +
;;; ===================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 102 :width normal)))))

;;; ============================
;;; + Sane Default Emacs Setup +
;;; ============================

;; Initialize frame size
(setq initial-frame-alist '((top . 100) (left . 550) (width . 82) (height . 38)))

;; Set transparent window look
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

;; Turn off mouse interface early in start-up to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Run at all power
(setq disabled-command-function nil)

;; Run Emacs in server mode to speed up subsequent startups of Emacs significantly
(load "server")
(unless (server-running-p) (server-start))

;; Set path to dependencies
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; All roads lead to $HOME
(setq default-directory "~/")

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set default coding systems to UTF-8
(progn
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)
  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode +1)
(setq recentf-max-saved-items 50) ; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; 80 chars is a good width
(set-default 'fill-column 80)

;; Highlight matching parentheses when the point is on them
(show-paren-mode 1)

;; Add parts of each file's directory to the buffer name if not unique
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Nic says eval-expression-print-level needs to be set to nil (turned off)
;; so that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Display 'lambda' as 'λ'
(global-prettify-symbols-mode 1)

;; TAB completion
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Offer to create parent directories if they do not exist
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

;; Write temporary files to own directory
(progn
  ;; create temp directory if not exists
  (defvar --temporary-directory (concat user-emacs-directory "temp"))
  (if (not (file-exists-p --temporary-directory))
      (make-directory --temporary-directory))
  ;; move all temporary file to its own directory
  (setq temporary-file-directory (concat user-emacs-directory "temp/")
        eww-bookmarks-directory temporary-file-directory
        save-place-file (expand-file-name "places" temporary-file-directory)
        savehist-file (expand-file-name "history" temporary-file-directory)
        recentf-save-file (expand-file-name "recentf" temporary-file-directory)
        abbrev-file-name (expand-file-name "abbrev_defs" temporary-file-directory)
        tramp-persistency-file-name (expand-file-name "tramp" temporary-file-directory)
        ido-save-directory-list-file (expand-file-name "ido.last" temporary-file-directory)
        auto-save-list-file-prefix "~/.emacs.d/temp/auto-save-list/.saves-"
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

;; Write backup files to own directory
(progn
  (setq backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory "backup")))))
  ;; make backups of files, even when they're in version control
  (setq vc-make-backup-files t)
  ;; stop emacs's backup changing the file's creation date of the original file
  (setq backup-by-copying t))

;; Automatically save and restore sessions
(progn
  (setq desktop-dirname             temporary-file-directory
        desktop-base-file-name      "emacs.desktop"
        desktop-base-lock-name      "emacs.desktop.lock"
        desktop-path                (list desktop-dirname)
        desktop-save                t
        desktop-files-not-to-save   "^$" ;reload tramp paths
        desktop-load-locked-desktop nil)
  ;; desktop-save-mode error fix
  (setq desktop-restore-frames nil
        desktop-restore-in-current-display t
        desktop-restore-forces-onscreen nil)
  (desktop-save-mode 1))

;;; ====================================
;;; + Sane Default Emacs Package Setup +
;;; ====================================

;; List of packages to install
(setq package-list '(ac-nrepl
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
                     flymake-json
                     git-messenger
                     golden-ratio
                     guide-key
                     helm
                     json-mode
                     litable
                     markdown-mode
                     magit
                     monokai-theme
                     multifiles
                     multiple-cursors
                     paredit
                     paredit-everywhere
                     restclient
                     s
                     skewer-mode
                     slime
                     smarter-compile
                     smooth-scrolling
                     tagedit
                     undo-tree
                     webjump
                     whitespace-cleanup-mode
                     yasnippet))

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

;; Load all user defined elisp functions
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; My package initializations
(require 'init-os)
(require 'init-org)
(require 'init-helm)
(require 'init-dired)
(require 'init-magit)
(require 'init-slime)
(require 'init-cider)
(require 'init-hippie)
(require 'init-paredit)
(require 'init-keybinding)
(require 'init-mode-mapping)
(require 'init-auto-complete)
(require 'init-misc)

;; Represent undo-history as an actual tree (visualize with C-x u)
(require 'undo-tree)
(setq undo-tree-mode-lighter "")
(global-undo-tree-mode)

;; A smarter wrapper for compile
(require 'smarter-compile)

;; Slime-style navigation for Emacs Lisp to the symbol at point(using M-.),
;; and the ability to pop back to previous marks (using M-,).
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;; Live web development with Emacs
(require 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; Intelligently call whitespace-cleanup before buffers are saved
(require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode)

;; An Emacs flymake handler for syntax-checking JSON using jsonlint
;; First install jsonlint, e.g. via npm:
;; npm install jsonlint -g
(require 'flymake-json)
(add-hook 'json-mode 'flymake-json-load)

;;; =======================================
;;; + Sane Default Emacs Appearance Setup +
;;; =======================================

;; Nifty tweaks
(setq visible-bell t
      truncate-partial-width-windows nil
      frame-title-format '(buffer-file-name "%f" ("%b")))

;; Default color-theme monokai
(load-theme 'monokai t)

;; Set cursor color to white
(set-cursor-color "#ffffff")

;; Emacs version of the Vim powerline
(require 'powerline)

;; Highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection)
(defface hl-line '((t (:background "gray21")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t) ; turn it on for all modes by default

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

;; Displays the available key bindings automatically and dynamically.
(progn
  (require 'guide-key)
  ;; the guide buffer is popped up when you input "C-x r", "C-x 8"
  ;; and any other prefixes following "C-x"
  (setq guide-key/guide-key-sequence '("C-x"))
  (setq guide-key/recursive-key-sequence-flag t)
  (guide-key-mode 1) ; enable guide-key mode
  ;; Add guide-key spedific settings for org-mode
  (defun guide-key/my-hook-function-for-org-mode ()
    (guide-key/add-local-guide-key-sequence "C-c")
    (guide-key/add-local-guide-key-sequence "C-c C-x")
    (guide-key/add-local-highlight-command-regexp "org-"))
  (add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode))

;; Xah's enhaned emacs emacs lisp code
(require 'xah-elisp-mode)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'xah-elisp-mode))

;; Enable syntax highlighting of dash functions
(eval-after-load "dash" '(dash-enable-font-lock))

;; Unclutter the modeline
(progn
  (require 'diminish)
  (eval-after-load "abbrev" '(diminish 'abbrev-mode))
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "paredit" '(diminish 'paredit-mode))
  (eval-after-load "tagedit" '(diminish 'tagedit-mode))
  (eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
  (eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
  (eval-after-load "skewer-mode" '(diminish 'skewer-mode))
  (eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
  (eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
  (eval-after-load "golden-ratio" '(diminish 'golden-ratio-mode))
  (eval-after-load "guide-key" '(diminish 'guide-key-mode))
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
  (eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
  (eval-after-load "subword" '(diminish 'subword-mode))

  (defmacro rename-modeline (package-name mode new-name)
    `(eval-after-load ,package-name
       '(defadvice ,mode (after rename-modeline activate)
          (setq mode-name ,new-name))))

  (rename-modeline "js2-mode" js2-mode "JS2")
  (rename-modeline "clojure-mode" clojure-mode "Clj"))

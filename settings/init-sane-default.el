;;; =====================================
;;; + Sane Default Emacs Configurations +
;;; =====================================

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

(provide 'init-sane-default)

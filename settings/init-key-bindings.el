;;; =====================================
;;; + Emacs Key Bindings Configurations +
;;; =====================================

;; Make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      w32-apps-modifier 'hyper) ; Menu key

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-=") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)

;; Expand region (increases selected region by semantic units)
(global-set-key (kbd "C-'") 'er/expand-region)

;; Multiple cursors
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/mark-all-dwim)
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-S-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c C-S-a") 'mc/edit-beginnings-of-lines)

;; Symbol and word specific mark-more
(global-set-key (kbd "C-c C-w") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-c C-S-w") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-c s-w") 'mc/mark-all-words-like-this)
(global-set-key (kbd "C-c C-s") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "C-c C-S-s") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "C-c s-s") 'mc/mark-all-symbols-like-this)

;; Extra multiple cursors stuff
(global-set-key (kbd "C-~") 'mc/reverse-regions)
(global-set-key (kbd "M-~") 'mc/sort-regions)
(global-set-key (kbd "H-~") 'mc/insert-numbers)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Replace rectangle-text with inline-string-rectangle
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Navigation bindings
(global-set-key (kbd "<C-prior>") 'beginning-of-buffer)
(global-set-key (kbd "<C-next>") 'end-of-buffer)

;; Move buffer in multiple windows easily
(global-set-key (kbd "<M-up>") 'buf-move-up)
(global-set-key (kbd "<M-down>") 'buf-move-down)
(global-set-key (kbd "<M-left>") 'buf-move-left)
(global-set-key (kbd "<M-right>") 'buf-move-right)

;; Resize window easily
(global-set-key (kbd "<M-S-left>") 'enlarge-window-horizontally)
(global-set-key (kbd "<M-S-right>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-S-up>") 'enlarge-window)
(global-set-key (kbd "<M-S-down>") 'shrink-window)

;; Zoom frame font size
(global-set-key (kbd "<C-S-wheel-up>") 'zoom-in)
(global-set-key (kbd "<C-S-wheel-down>") 'zoom-out)
(global-set-key (kbd "<C-S-mouse-4>") 'zoom-in)
(global-set-key (kbd "<C-S-mouse-5>") 'zoom-out)

;; Window switching
(global-set-key [(control ?,)] (lambda () (interactive) (other-window -1)))
(global-set-key [(control ?.)] (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Visual regexp
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;; Keyboard macro
(global-set-key (kbd "C-c M-i") 'insert-kbd-macro)
(global-set-key (kbd "C-c M-n") 'name-last-kbd-macro)
(global-set-key (kbd "C-c M-m") 'apply-macro-to-region-lines)

;; Help should search more than just commands
(global-set-key (kbd "<help> a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; To test small elisp code changes easily with eval-region
(global-set-key (kbd "C-c C-r") 'eval-region)

;; Eval buffer
(global-set-key (kbd "C-c M-e") 'eval-buffer)

;; Eval and print last s-expression (C-j is used by Paredit for inserting new line and indent)
(global-set-key (kbd "C-c C-j") 'eval-print-last-sexp)

;; Bind "C-c r" for function replace-string
(global-set-key (kbd "C-c r") 'replace-string)

;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)

;; Copy file path to kill ring
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)

;; Add region to *multifile*
(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)

;; Indentation help
(global-set-key (kbd "M-j") (λ (join-line -1)))

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "s-z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))
(global-set-key (kbd "M-Z") (lambda (char) (interactive "cZap to char: ") (zap-to-char 1 char)))
(global-set-key (kbd "s-Z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (λ (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (λ (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (λ (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (λ (replace-region-by 's-upper-camel-case)))

;; Killing text
(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key (kbd "C-k") (bol-with-prefix kill-line))
(global-set-key (kbd "M-k") 'kill-whole-line)
(global-set-key (kbd "C-M-k") 'kill-sentence)
(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") (λ (save-region-or-current-line 1)))

;; Make shell more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Manipulate whitespace
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Copy the whole lines
(global-set-key (kbd "C-c C-c") 'copy-whole-lines)

;; Clever newlines
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-S-o") 'open-line-above)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Line movement
(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; File finding
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c M-r") 'revert-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "C-c C-b") 'quick-switch-buffer)

;; Killing buffer
(global-set-key (kbd "s-k") 'kill-this-buffer)

;; Quickly switch to scratch buffer
(global-set-key (kbd "C-c <tab>") 'goto-scratch)

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Create new frame
(define-key global-map (kbd "C-c n") 'make-frame-command)

;; Browse visualized undo tree
(global-set-key (kbd "C-x u") 'undo-tree-visualize)

;; Yank without indent
(global-set-key (kbd "C-S-y") 'yank-unindented)

;; Perform general cleanup
(global-set-key (kbd "C-c =") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; Fold the active region
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; vim's ci and co commands
(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)
(global-set-key (kbd "s-i") 'copy-inner)
(global-set-key (kbd "s-o") 'copy-outer)

;; Efficiently hopping squeezed lines
(global-set-key (kbd "C-c s") 'helm-swoop)
(global-set-key (kbd "C-c S") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-s") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-s") 'helm-multi-swoop-all)

;; Emulation of the vi % comman
(global-set-key (kbd "%") 'goto-match-paren)

;; Show line number for goto-line
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Emacs package manager
(global-set-key (kbd "C-x M-p") 'package-list-packages)

;; Emacs git interface
(global-set-key (kbd "C-x m") 'magit-status)

;; Emacs web wowser
(global-set-key (kbd "C-x w") 'eww)

;;;
;;;============================================================================
;;;

;; Smarter compile
(global-set-key (kbd "<f5>") 'smarter-compile)

;; Open the current file or dired marked files in external app
(global-set-key (kbd "<f6>") 'ergoemacs-open-in-external-app)

;; Manage system process from within Emacs
(global-set-key (kbd "<C-f6>") 'proced)

;; Toggle linum-mode
(global-set-key (kbd "<f7>") 'linum-mode)

;; Toggle line wrap
(global-set-key (kbd "<f8>") 'toggle-truncate-lines)

;; Toggle whitespace-mode
(global-set-key (kbd "<f9>") 'whitespace-mode)

;; Perform general cleanup
(global-set-key (kbd "<C-f9>") 'cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "<C-f10>") 'menu-bar-mode)

;; Turn on the tool bar for exploring speed buttons
(global-set-key (kbd "<C-S-f10>") 'tool-bar-mode)

(provide 'init-key-bindings)

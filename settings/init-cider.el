;;; =========================================================
;;; + Clojure IDE and REPL for Emacs, built on top of nREPL +
;;; =========================================================

(require 'cider)

(defun live-windows-hide-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(when (eq system-type 'windows-nt)
  (add-hook 'cider-mode-hook 'live-windows-hide-eol)
  (add-hook 'cider-repl-mode-hook 'live-windows-hide-eol))

;; Enable eldoc in Clojure buffers
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (cider-turn-on-eldoc-mode)))

(add-hook 'cider-mode-hook
          (lambda ()
            (cider-turn-on-eldoc-mode)))

;; Stop the error buffer from popping up while working in buffers other than the REPL
(setq cider-popup-stacktraces nil)
(setq cider-popup-stacktraces-in-repl nil)
(add-to-list 'same-window-buffer-names "*cider*")

;; Auto Complete for cider
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; Enabling CamelCase support for editing commands(like forward-word, backward-word, etc)
(add-hook 'cider-repl-mode-hook 'subword-mode)

;; To auto-select the error buffer when it's displayed
(setq cider-auto-select-error-buffer t)

;; Specify the print length to be 100 to stop infinite sequences killing things.
(defun live-nrepl-set-print-length ()
  (nrepl-send-string-sync "(set! *print-length* 100)" "clojure.core"))

(add-hook 'nrepl-connected-hook 'live-nrepl-set-print-length)

;; Use specific nrepl-port number 4555
(setq nrepl-port "4555")

(provide 'init-cider)

;;; ===================================================
;;; + This is where everything starts, live in Emacs! +
;;; ===================================================

;; Keep track of loading time
(defconst emacs-start-time (current-time))

;; Set path to dependencies
(setq settings-dir (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "init-custom.el" settings-dir))
(load custom-file)

;; Initialization
(require 'init-sane-default)
(require 'init-package)
(require 'init-appearance)
(require 'init-keybinding)

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

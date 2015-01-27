;;; ===============================================
;;; + Emacs OS Platform Dependency Configurations +
;;; ===============================================

;; Windows OS specifics
(if (eq system-type 'windows-nt)
    (progn
      ;; Add all git related command path, exec-path is important for Magit, setenv is used by eshell
      (setenv "GIT_ASKPASS" "git-gui--askpass") ; fix magit push hung up issue on windows (require OpenSSH)
      (setq exec-path (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;" (getenv "PATH"))))

  ;; use GNU W32 Utils find and grep for Windows
  (when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
    (setenv "PATH" (concat "C:\\GNU\\bin\\gnuwin32\\bin;" (getenv "PATH")))
    (setq find-program "C:\\GNU\\bin\\gnuwin32\\bin\\find.exe"
          grep-program "C:\\GNU\\bin\\gnuwin32\\bin\\grep.exe"))

  ;; prevent issues with the Windows null device (NUL) when using cygwin find with rgrep.
  (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
    "Use cygwin's /dev/null as the null-device."
    (let ((null-device "/dev/null"))
      ad-do-it))
  (ad-activate 'grep-compute-defaults))

(provide 'init-os)

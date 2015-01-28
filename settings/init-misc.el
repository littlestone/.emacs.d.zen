;;; =====================================
;;; + Miscellaneous Emacs Configurations +
;;; ======================================

(require 'init-os)

;; Seed the random-number generator
(random t)

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Add Urban Dictionary to webjump (C-x g)
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites '(("Urban Dictionary" .
                                  [simple-query
                                   "www.urbandictionary.com"
                                   "http://www.urbandictionary.com/define.php?term="
                                   ""]))))
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites '("百度" .
                                 [simple-query
                                  "www.baidu.com"
                                  "http://www.baidu.com/s?wd="
                                  ""])))
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites '("汉典" .
                                 [simple-query
                                  "www.zdic.net"
                                  "http://www.zdic.net/sousuo/?q="
                                  ""])))
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites '("海词在线" .
                                 [simple-query
                                  "dict.cn"
                                  "http://dict.cn/"
                                  ""])))
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites '("法語助手" .
                                 [simple-query
                                  "www.frdic.com"
                                  "http://www.frdic.com/dicts/fr/"
                                  ""])))

;; Make eww default for most URLs
(if (consp browse-url-browser-function)
    (setcdr (assoc "." browse-url-browser-function) 'eww-browse-url)
  (setq browse-url-browser-function
        `(("^ftp://.*" . browse-ftp-tramp)
          ("video" . ,browse-url-browser-function)
          ("\\.tv" . ,browse-url-browser-function)
          ("youtube" . ,browse-url-browser-function)
          ("." . eww-browse-url))))

;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (lambda (insert "™")))
(global-set-key (kbd "C-x 8 ( c )") (lambda (insert "©")))
(global-set-key (kbd "C-x 8 - >") (lambda (insert "→")))
(global-set-key (kbd "C-x 8 8") (lambda (insert "∞")))
(global-set-key (kbd "C-x 8 ( c )") (lambda (insert "©")))

;; A bit of misc cargo culting in misc.el
(setq xterm-mouse-mode t)

;; Move multiple-cursors history file to temps
(setq mc/list-file (expand-file-name ".mc-lists.el" temporary-file-directory))

;; Every buffer would be cleaned up before it's saved
(add-hook 'before-save-hook 'cleanup-buffer)

(provide 'init-misc)

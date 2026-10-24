;;; global-map
;;------------------------------------------------------------------------------
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x d") 'list-directory)
;; (keymap-global-set "ESC" 'keyboard-escape-quit)
;; (keymap-global-set "C-x k" 'kill-current-buffer)
(when (fboundp 'vterm)
  (global-set-key (kbd "C-x C-t") 'vterm))
(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x C-g") 'magit-status))
;; unset repeat-complex-command
(global-unset-key (kbd "C-x <escape> <escape>"))

;;; dired-mode-map
;;------------------------------------------------------------------------------
;; (define-key dired-mode-map (kbd "<SPC>") 'dired-mark)

(provide 'my/key-binding)

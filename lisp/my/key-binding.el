;; ;;; my/key-binding.el --- My key-binding customization

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; (keymap-global-set "ESC" 'keyboard-escape-quit)
;; (keymap-global-set "C-x k" 'kill-current-buffer)

(global-unset-key (kbd "C-x <escape> <escape>"))


(provide 'my/key-binding)

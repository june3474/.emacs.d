;;; my/funcs-misc.el --- My org mode indent customization

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun save-and-compile ()
  "Save current buffer and compile. Bind key to F9"
  (interactive)
  ;; Don't make a backup file
  (save-buffer 0)
  (if (equal major-mode 'emacs-lisp-mode)
      (byte-compile-file (buffer-file-name))
    (compile "make -k")))
(global-set-key (kbd "<f9>") 'save-and-compile)

(defun revert-buffer-with-euc-kr ()
  "set the encoding of the current buffer to euc-kr. Bind key to F12"
  (interactive)
  (let ((coding-system-for-read 'korean-iso-8bit))
    (revert-buffer t t)))
(global-set-key (kbd "<f12>") 'revert-buffer-with-euc-kr)

(defun enable-babel ()
  "Enabel source block evaluation"
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (error "Not in Org mode")
    ;; Select languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (shell . t)))))


(provide 'my/misc-funcs)

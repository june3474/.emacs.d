;;; my/org-faces.el --- My org mode faces customization

;;; Code:

(defun my-org-faces ()
  "set org-mode font faces"
  (interactive)
  (require 'org-faces)
  (set-face-attribute 'org-level-1 nil :height 1.25 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.15 :foreground "PaleGreen")
  (set-face-attribute 'org-level-4 nil :height 1.1)
  ;; For org-level5~8, use default settings inherited from outline5~8
  (set-face-attribute 'org-link nil :underline t)
  ;; (set-face-attribute 'org-headline-done nil :strike-through t)
  (set-face-attribute 'org-document-title nil :foreground "pale turquoise"
  					  :height 1.2
                      :inverse-video t
					  :weight 'bold)
  ;; Faces which should be fixed-width even under variable-pitch-mode
  (set-face-attribute 'org-block nil :family "Monospace")
  (set-face-attribute 'org-code  nil :family "Monospace")
  (set-face-attribute 'org-document-info-keyword nil :family "Monospace")
  (set-face-attribute 'org-meta-line nil :family "Monospace")
  (set-face-attribute 'org-special-keyword nil :family "Monospace")
  (set-face-attribute 'org-table nil :family "Monospace")
  (set-face-attribute 'org-verbatim nil :family "Monospace")
  (set-face-attribute 'org-property-value nil :family "Monospace")
  (set-face-attribute 'org-tag nil :inherit 'shadow :family "Monospace")
  (set-face-attribute 'org-hide nil :family "Monospace" :height 1.0)
  ;; Add some new faces
  (defface org-doing '((t :inverse-video t :inherit org-todo))
    "Face to use for todo keyword 'DOING'"
    :group 'org-faces))

(provide 'my/org-faces)

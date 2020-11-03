;;; my-org.el --- My org mode customization -*- lexical-binding: t -*-

;;; Code:

(defun my-org-face ()
  "set headline font faces"
  (set-face-attribute 'org-level-1 nil :height 1.35 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.3 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.2)
  (set-face-attribute 'org-level-4 nil :height 1.1)
  ;; For org-level5~8, use default settings inherited from outline5~8
  (set-face-attribute 'org-link nil :underline t)
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (set-face-attribute 'org-document-title nil :foreground "pale turquoise"
					                          :weight 'bold
  										      :height 1.2
										      :inverse-video t)
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

(defun my-org-indent--compute-prefixes ()
  "Override the original 'org-indent--compute-prefixes' function."
  (setq org-indent--heading-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--inlinetask-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (dotimes (n org-indent--deepest-level)
    (let ((indentation (if (<= n 1) 0
                         (* (1- org-indent-indentation-per-level)
                            (1- n)))))
      (let ((heading-prefix (make-string indentation ?*)))
        ;; Headlines line prefixes.
        (aset org-indent--heading-line-prefixes
              n
              (org-add-props heading-prefix nil 'face 'org-indent))
        ;; Inline tasks line prefixes
        (aset org-indent--inlinetask-line-prefixes
              n
              (cond ((<= n 1) "")
                    ((bound-and-true-p org-inlinetask-show-first-star)
                     (concat org-indent-inlinetask-first-star
                             (substring heading-prefix 1)))
                    (t (org-add-props heading-prefix nil 'face 'org-indent)))))
      ;; Text line prefixes.
      (let* ((heading-prefix-width (* (frame-char-width) indentation))
             (stars-width (if (<= n 1) 0
                            (* (1- n) (frame-char-width))))
             ;; Org-mode defines 8 org-level-faces
             (face-attr (and (< n 9)
                             (> n 0)
                             (face-attribute (intern-soft
                                              (concat "org-level-"
                                                      (number-to-string n)))
                                             :height)))
             (level-face-height
              (cond ((not face-attr) 1)
                    ((eq face-attr 'unspecified) 1)
                    (t face-attr)))
             (bullet-space-width
               (if (= n 0) 0
                 (* 2 (ceiling (* (frame-char-width) level-face-height))))))
        (aset org-indent--text-line-prefixes
              n
              (+ heading-prefix-width stars-width bullet-space-width))))))

(defun my-org-indent-set-line-properties (level indentation &optional heading)
  "Override the original 'org-indent-set-line-properties' function."
  (let* (line wrap (t-indent (aref org-indent--text-line-prefixes level)))
    (pcase heading
      ;; Regular text or list.
      ('nil
       (setq line `(space :width (,t-indent)))
       (setq wrap (if (<= indentation 1)
                      line
                    (list 'space :width
                          (list (+ t-indent
                                   (* (frame-char-width) indentation))))))) 

      ;; Heading or inline task.
      (_
       (setq line (aref (pcase heading
                          ('t org-indent--heading-line-prefixes)
                          ('inlinetask org-indent--inlinetask-line-prefixes))
                        level))
       (setq wrap `(space :width (,t-indent)))))
    ;; Add properties down to the next line to indent empty lines.
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
                         `(line-prefix ,line wrap-prefix ,wrap)))
  (forward-line))
      
(advice-add 'org-indent--compute-prefixes
            :override #'my-org-indent--compute-prefixes)
(advice-add 'org-indent-set-line-properties
            :override #'my-org-indent-set-line-properties)

(provide 'my-org)

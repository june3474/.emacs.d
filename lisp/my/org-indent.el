;;; my/org.el --- My org mode indent customization

;;; Code:

;; (require 'org-indent)

(defun my-org-indent--compute-prefixes ()
  "My function to advise the `org-indent--compute-prefixes' function.
This creates an extra list that saves the width of line-prefix in pixel for
text lines, i.e., non-heading lines"

  (defvar org-indent--text-line-prefixes-pixel nil
  "Vector containing line prefixes in pixel for regular text.")
  (setq org-indent--text-line-prefixes-pixel
        (make-vector org-indent--deepest-level nil))
  (dotimes (n org-indent--deepest-level)
    (let ((indentation (if (<= n 1) 0
                         (* (1- org-indent-indentation-per-level)
                            (1- n)))))
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
        (aset org-indent--text-line-prefixes-pixel
              n
              (+ heading-prefix-width stars-width bullet-space-width))))))

;; Notice backquote(`) and comma(,). They have special meanings.
;; https://emacs.stackexchange.com/questions/7602/what-is-the-point-of-quote-with-single-argument-and-comma-quote-arg
;; Also, () after :width means width is in pixels rather than the # of characters.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Pixel-Specification.html
(defun my-org-indent-set-line-properties (level indentation &optional heading)
  "Override the original `org-indent-set-line-properties' function."

  (require 'org-indent)
  (defvar org-indent--text-line-prefixes-pixel)
  (let* ((indent-pixel (aref org-indent--text-line-prefixes-pixel level))
         (wrap (propertize (aref org-indent--text-line-prefixes level)
                           'display `(space :width (,indent-pixel))))
         line)
    (pcase heading
      ;; Regular text or list.
      (`nil (setq line wrap))
      ;; inline task.
      (`inlinetask (setq line (aref org-indent--inlinetask-line-prefixes level)))
      ;; Heading
      (_  (setq line (aref org-indent--heading-line-prefixes level))))
    ;; Add properties down to the next line to indent empty lines.
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
                         `(line-prefix ,line wrap-prefix ,wrap)))
  (forward-line))

;;--------------------------------------------

(advice-add 'org-indent--compute-prefixes
            :after #'my-org-indent--compute-prefixes)
(advice-add 'org-indent-set-line-properties
            :override #'my-org-indent-set-line-properties)

(provide 'my/org-indent)

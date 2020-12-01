;;; my-org-journal.el --- My org-journal mode customization -*- lexical-binding: t -*-

;;; Code:

(defun my-org-journal-handle-old-carryover (old_carryover)
  "My custom function to handle the old carryover entries in the previous day's journal"
  (save-excursion
    (let ((matcher (cdr (org-make-tags-matcher org-journal-carryover-items))))
      (dolist (entry (reverse old_carryover))
        (save-restriction
          (narrow-to-region (car entry) (cadr entry)) 
          (goto-char (point-min))
          (org-scan-tags '(lambda ()
                            (org-todo (concat (org-get-todo-state) "⥱")))
                         matcher org--matcher-tags-todo-only))))))

(defun my-org-journal-insert-template ()
  "Insert template after a new journal is created.

Assume this function will be run as a `org-journal-after-entry-create-hook'"
  (let ((heading-re (concat "^" (regexp-quote org-journal-time-prefix)))
        (templates '("Work" "Personal" "Computer & Programming"))
        insert-point)
    ;; Current position would be at the end of the subtree of the current day's
    ;; heading or (point-max).
    (save-restriction
      (save-match-data
        ;; Go upto date level, point would be at the bol of today's date heading
        (while (org-up-heading-safe))
        (org-narrow-to-subtree)
        (setq case-fold-search nil) ; case-sensitive
        ;; Set initial insert-point
        (setq insert-point
              (if (re-search-forward heading-re nil t)
                  (progn
                    (beginning-of-line)
                    (point))
                (point-max)))
        ;; Insert template
        (when org-journal--newly-created-p
          (dolist (template templates)
            (goto-char (point-min))
            (if (re-search-forward (concat heading-re template) nil t)
                (setq insert-point (progn
                                     (outline-end-of-subtree)
                                     (point)))
              (goto-char insert-point)
              (insert (concat org-journal-time-prefix template "\n"))
              (setq insert-point (point)))))
        ;; Sometimes, :LOGBOOK: drawers don't get folded. dunno why?
        (goto-char (point-min))
        (org-cycle-hide-drawers 'subtree)
        ;; Cursor location, beginning of the first ** level entry
        (re-search-forward heading-re nil t)
        (goto-char (match-end 0))))
    (save-buffer)))

(provide 'my-org-journal)

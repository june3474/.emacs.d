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

This function is supposed to be run as a `org-journal-after-entry-create-hook'"
  ;; The value of `org-journal-time-prefix' is "** " 
  (let ((heading-re (concat "^" (regexp-quote org-journal-time-prefix)))
        (templates '("퇴직 일기" "Tennis" "Guitar" "Programming")))
    ;; Current position would be at the end of the subtree of the current day's
    ;; heading or (point-max).
    (save-restriction
      (save-match-data
        ;; Go upto date level, point would be at the bol of today's date heading
        (while (org-up-heading-safe))
        (org-narrow-to-subtree)
        (setq case-fold-search nil) ; case-sensitive
        (when org-journal--newly-created-p
          ;; Set initial search-starting point
          (if (re-search-forward heading-re nil 'end)
              (beginning-of-line)
            (unless (= 0 (current-column)) (insert "\n")))
          ;; Insert template
          (dolist (template templates)
            (if (re-search-forward (concat heading-re template) nil t)
                ;; `outline-end-of-subtree' does not work well. dunno why?
                ;; instead, go to the bol of the next ** level, which is the same as
                ;; the end of the current ** level.
                (if (re-search-forward heading-re nil 'end)
                    (beginning-of-line))
              (unless (= 0 (current-column)) (insert "\n"))
              (insert (concat org-journal-time-prefix template "\n")))))

        ;; Cursor location, beginning of the first ** level entry
        (goto-char (point-min))
        (re-search-forward heading-re nil t)))))

(defun my-delete-blank-lines ()
  "Leave just one blank line at the end

This function is supposed to be run as a `before-save-hook'"
  (when (eq major-mode 'org-journal-mode)
    (goto-char(point-max))
    (if (= 0 (current-column))
        (delete-blank-lines)
      (insert "\n"))))

(provide 'my-org-journal)

;;; -*- lexical-binding: t; -*-
;;; my/org-journal.el --- My org-journal mode customization

(require 'org-journal (concat user-emacs-directory "lisp/org-journal/org-journal"))

(defun my-org-journal-handle-old-carryover (old_carryover)
  "My custom function to handle the old carryover entries in the previous day's
 journal"
  (save-excursion
    (let ((matcher (cdr (org-make-tags-matcher org-journal-carryover-items))))
      (dolist (entry (reverse old_carryover))
        (save-restriction
          (narrow-to-region (car entry) (cadr entry)) 
          (goto-char (point-min))
          (org-scan-tags '(lambda ()
                            (org-todo (concat (org-get-todo-state) "⥱")))
                         matcher org--matcher-tags-todo-only))))))

(defun my-org-journal-carryover-items (text entries prev-buffer)
  "Carryover items.

Will insert `text', and run `org-journal-handle-old-carryover-fn' function
to process the carryover entries in `prev-buffer'."
  (when entries
    (if (org-journal--is-date-prefix-org-heading-p)
        (progn
          (while (org-up-heading-safe))
          (outline-end-of-subtree))
      (goto-char (point-max)))

    ;; Ensure `view-mode' is not active
    (view-mode -1)

    (unless (eq (current-column) 0) (insert "\n"))

    (insert text)

    (save-excursion
      (if (org-journal--daily-p)
          (goto-char (point-min))
        (while (org-up-heading-safe)))

      (unless (null org-journal-skip-carryover-drawers)
        (org-journal--remove-drawer))

      (save-excursion
        (while (re-search-forward "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( [a-z]+\\)?\\)>" nil t)
          (unless (save-excursion
                    (goto-char (line-beginning-position))
                    (re-search-forward "\\<\\(SCHEDULED\\|DEADLINE\\):" (line-end-position) t))
            (replace-match
             (format-time-string "%Y-%m-%d %a"
                                 (org-journal--calendar-date->time
                                  (save-match-data
                                    (if (org-journal--daily-p)
                                        (org-journal--file-name->calendar-date (buffer-file-name))
                                      (save-excursion
                                        (while (org-up-heading-safe))
                                        (org-journal--entry-date->calendar-date))))))
             nil nil nil 1)))))

    (outline-end-of-subtree)

    ;; Process carryover entries in the previous day's journal
    (with-current-buffer prev-buffer
      (funcall org-journal-handle-old-carryover-fn entries))))

(defun my-org-journal-insert-template ()
  "Insert template after a new journal is created.

This function is supposed to be run as a `org-journal-after-entry-create-hook',
which means being run after carryover. Each template item will be inserted as
level 2 heading(i.e., **), the same as `org-journal-time-prefix'.
After this function finish, cursor would be at (point-max) and
(org-narrow-to-subtree) is still in effect."

  ;; The value of `org-journal-time-prefix' is "** " 
  (let ((heading-re (concat "^" (regexp-quote org-journal-time-prefix)))
        (templates (reverse '("Work" "Tennis" "대구 살이" "Guitar" "Computer & Programming")))
        insert-point)
    ;; Current position would be at (column 0) of the new line after the end of the today's
    ;; subtree or (point-max).
    (save-restriction
      (save-match-data
        ;; case-sensitive search
        (setq case-fold-search nil)
        ;; Go upto date level. Now, point would be at the bol of today's date heading
        (while (org-up-heading-safe))
        ;; Hide all other dates
        (org-narrow-to-subtree)
        (save-excursion
          (setq insert-point (progn
                               (re-search-forward ":END:" nil t)
                               (forward-line)
                               (unless (eq (current-column) 0) (insert "\n"))
                               (point))))
        ;; Insert template
        (dolist (template templates)
          ;; Check if template heading is already there,
          ;; if not, insert the template item
          (unless (re-search-forward (concat heading-re template) nil t)
            (goto-char insert-point)
            (insert org-journal-time-prefix template "\n")))))))

(defun my-org-journal--finalize-view ()
  "my final view.

Basically, 1) show only date-level headings
2) but for today's journal, show upto 2'nd level(time-level) heading."
  (let ((hide-level (if org-journal-hide-entries-p
                        (min (org-journal--time-entry-level) 2)
                      2)))
    ;; show only top-level headings
    (org-overview)
    ;; Now, current position be in the middle of the today's date-heading
    (org-back-to-heading) ;; move to bol of the heading or inlinetask. Necessary
    (org-fold-show-children hide-level)
    (org-fold-hide-drawer-all))

  ;; locate cursor
  (re-search-forward (concat "^" (regexp-quote org-journal-time-prefix)) nil t)
  (end-of-line))

(defun my-org-journal-new-entry (prefix &optional time no-timestamp)
  "Open today's journal file and start a new entry.

With a PREFIX arg, open the today's file, create a heading if it
doesn't exist yet, but do not create a new entry.

If given a TIME, create an entry for the time's day. If no TIME
was given, use the current time (which is interpreted as
belonging to yesterday if smaller than `org-extend-today-until').

Whenever a journal entry is created the `org-journal-after-entry-create-hook'
hook is run."
  (interactive "P")
  (org-journal--sanity-checks)
  (org-journal--create-journal-dir)

  ;; If time is before org-extend-today-until, interpret it as
  ;; part of the previous day:
  (let* ((now (decode-time nil))
         (org-extend-today-until-active-p (and (not time) (< (nth 2 now) org-extend-today-until)))
         (entry-path)
         (should-add-entry-p (not prefix)))
    (when org-extend-today-until-active-p
      (setq time (encode-time (nth 0 now)       ; second
                              (nth 1 now)       ; minute
                              (nth 2 now)       ; hour
                              (1- (nth 3 now))  ; day
                              (nth 4 now)       ; month
                              (nth 5 now)       ; year
                              (nth 8 now))))    ; timezone
    (setq entry-path (org-journal--get-entry-path time))

    ;; Open journal file
    (unless (string= entry-path (buffer-file-name))
      (funcall org-journal-find-file-fn entry-path))

    ;; Ensure `view-mode' is not active
    (view-mode -1)

    (org-journal--insert-header time)
    (org-journal--insert-entry-header time)
    (org-journal--decrypt)

    ;; Move TODOs from previous day to new entry
    (when (and org-journal--new-entry-header-p
               org-journal-carryover-items
               (not (string-blank-p org-journal-carryover-items))
               (string= entry-path (org-journal--get-entry-path (current-time))))
      (org-journal--carryover))

    (if (org-journal--is-date-prefix-org-heading-p)
        (outline-end-of-subtree)
      (goto-char (point-max)))

    (when should-add-entry-p
      (org-journal--insert-entry time org-extend-today-until-active-p no-timestamp))

    (my-org-journal--finalize-view)))

(defun my-delete-blank-lines ()
  "Leave just one blank line at the end

This function is supposed to be run as a `before-save-hook'"
  (when (eq major-mode 'org-journal-mode)
    (goto-char(point-max))
    (if (= 0 (current-column))
        (delete-blank-lines)
      (insert "\n"))))

;; Maybe these settings could go to config.el
(defun my-setup-variables ()
  "Setup todo keywords list and fontlock"
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "WORKING(w!)" "|"
                    "TODO⥱" "WORKING⥱" "DONE(d!)"))
        org-todo-keyword-faces
        '(("TODO" . org-todo) ("WORKING" . org-doing)
          ("TODO⥱" . "LightSteelBlue") ("WORKING⥱" . "LightSteelBlue")
          ("DONE" . org-done))
        org-journal-carryover-items "TODO=\"TODO\"|TODO=\"WORKING\""
        org-journal-handle-old-carryover-fn 'my-org-journal-handle-old-carryover
        org-journal-carryover-headings-only t)
  ;; Give "⥱" a different color
  (font-lock-add-keywords
   'org-journal-mode
   '(("\\<TODO\\(⥱\\)\\>" 1 font-lock-keyword-face prepend)
     ("\\<WORKING\\(⥱\\)\\>" 1 font-lock-keyword-face prepend))
   'append))

(defun new-journal ()
  "Write a new journal

This function is supposed to be autoloaded and bound to a global keymap by
 `use-package'"

  (interactive)
  ;; (let ((current-prefix-arg '(4))) ; Simulates C-u (prefix argument 4)
  ;;     (call-interactively 'org-journal-new-entry))

  ;; Make prefix nil so as to run  `org-journal--insert-entry' and
  ;; `org-journal-after-entry-create-hook'
  (org-journal-new-entry nil nil t))

;;--------------------------------------------

;; add to global hook
(add-hook 'org-journal-after-entry-create-hook
          #'my-org-journal-insert-template)

;; add to buffer local hook
(add-hook 'org-journal-mode-hook
          #'(lambda () (add-hook 'before-save-hook
                                 #'my-delete-blank-lines -1 t)))
(my-setup-variables)

;; replace the original functions with mine
(advice-add 'org-journal-new-entry
            :override #'my-org-journal-new-entry)

(provide 'my/org-journal)

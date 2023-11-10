;;; my/config-daemon.el --- Daemon mode customization

(defun not-windowed-on-other-frame ()
  "Return t if the current buffer is NOT being displayed on other frames,
nil otherwise.

This function is intended to be used as with `kill-buffer-query-functions',
so that if the `current-buffer' is being displayed on other frame,
the buffer will not be killed. 
"
  (catch 'found
    ;; The first element of `get-buffer-window-list' is `selected-window'
    (dolist (window (get-buffer-window-list (current-buffer) 'nominibuf 0) t)
      (unless (equal (selected-frame) (window-frame window))
        (throw 'found nil)))))

(defun my-server-save-buffers-kill-terminal (arg)
  ;; Called from save-buffers-kill-terminal in files.el.
  "Modified `server-save-buffers-kill-terminal' function to kill appropriate 
buffers and to handle the frame & the process.

`server-save-buffers-kill-terminal' works as expected with the so called
`process-buffers' which started with emacsclient, i.e., files offered as
arguments of emacsclient.
But the files(buffers) opened afterwards with C-x C-f are not killed by C-x C-c
and ramain buried in the server.
This function determines buffers to kill based on the number of frames open.
"
  (let* ((proc (frame-parameter nil 'client))
         (proc-buffers (if (processp proc) (process-get proc 'buffers) nil))
         (first-buffer (current-buffer))
         (buffers-to-handle (delete-dups (append (list first-buffer) proc-buffers))))
    ;; First, handle buffers
    (if (<= (length (frame-list)) 2)  ; this is the only frame excluding server
        (progn
          ;; Ask & save all the file-visiting buffers.
          (save-some-buffers arg nil)
          ;; Kill all the file-visiting buffers.
          (dolist (buf (buffer-list))
            (when (and (buffer-file-name buf) (buffer-live-p buf))
              (kill-buffer buf))))
      ;; With more than 2 frames open, i.e., (> 2 (length (frame-list)))
      (save-some-buffers arg (lambda () (memq (current-buffer) buffers-to-handle)))
      (dolist (buf buffers-to-handle)
        (when (and (buffer-file-name buf)
                   (buffer-live-p buf))
          ;; For any process-buffer currently not showing, ask to kill
          ;; except for the `first-buffer' which must be showing now.
          (if (or (not proc-buffers)
                  (equal buf first-buffer)
                  ;; If this buffer is being displayed on other frame, `kill-buffer'
                  ;; does nothing as `not-windowed-on-other-frame' is to be added to
                  ;; `kill-buffer-query-functions'.
                  ;; So, what this condition actually does is, kinda `unless' condition
                  ;; for the `y-or-n-p' on the next line.
                  (not (with-current-buffer buf (not-windowed-on-other-frame)))
                  ;; A process-buffer neither the `first-buffer' nor currently being
                  ;; displayed on other frames.
                  (and (switch-to-buffer buf)
                       (y-or-n-p (format "Kill the buffer %s? " buf))))
              (kill-buffer buf)
            ;; If we choose not to kill this buffer, remove buf from process-buffers
            ;; so that it can survive from `server-delete-client' below.
            (process-put proc 'buffers (remove buf proc-buffers))))))
    ;; Now, handle frame and process
    (cond ((eq proc 'nowait) ; emacsclient started with --nowait option
	       (if (cdr (frame-list))
		       (delete-frame (selected-frame) t)
	         ;; If we're the last frame standing, kill Emacs.
	         (save-buffers-kill-emacs arg)))
	      ((processp proc) ; without --nowait option
	       (server-delete-client proc))
          (t (error "Invalid client frame")))))

(defun my-handle-delete-frame-1 (event)
  "Handle delete-frame events from the X server.

Currently, NOT USED!, Leave just for posssible future reference.
"
  (interactive "e")
  (let* ((frame (posn-window (event-start event))))
    (select-frame frame)
    (if (catch 'other-frame
          (dolist (frame-1 (frame-list))
            ;; A valid "other" frame is visible, has its `delete-before'
            ;; parameter unset and is not a child frame.
            (when (and (not (eq frame-1 frame))
                       (frame-visible-p frame-1)
                       (not (frame-parent frame-1))
                       (not (frame-parameter frame-1 'delete-before)))
              (throw 'other-frame t))))
	    (my-server-save-buffers-kill-terminal nil)
      ;; Gildea@x.org says it is ok to ask questions before terminating.
      (save-buffers-kill-emacs))))

(defun my-handle-delete-frame (event)
  "Handle delete-frame events from the X server.

The function is called when you click the delete-frame button(upper-right corner
[X] on Windows). You can advise(defadvice) that command or you can replace it
with a function that handles that click.
"
  (interactive "e")
  (let* ((frame (posn-window (event-start event))))
    (select-frame frame)
	(save-buffers-kill-terminal)))

;;--------------------------------------------

(when (daemonp)
  ;; Customize the closing behavior
  (add-hook 'kill-buffer-query-functions #'not-windowed-on-other-frame)
  (advice-add 'server-save-buffers-kill-terminal
              :override
              #'my-server-save-buffers-kill-terminal)
  (advice-add 'handle-delete-frame
              :override
              #'my-handle-delete-frame)
  ;; Bring the newly created frame to front
  (add-hook 'server-after-make-frame-hook
            #'raise-frame)
  ;; Make cursor blink
  (add-hook 'server-after-make-frame-hook
            #'blink-cursor-mode))


(provide 'my/config-daemon)

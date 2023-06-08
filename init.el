(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-blinks 0)
 '(column-number-mode t)
 '(compilation-window-height 8)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   '("cf861f5603b7d22cb3545a7c63b2ee424c34d8ed3b3aa52d13abfea4765cffe7" "5185a285365a768a30ac274bdbc4437e7fd2fbe3107a1b0f2b60e900181905e0" default))
 '(default-major-mode 'text-mode t)
 '(delete-selection-mode t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages
   '(markdown-mode vscode-dark-plus-theme jetbrains-darcula-theme web-beautify org-bullets ivy))
 '(scroll-bar-mode 'right)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(text-mode-hook '(text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; GENERAL
;;---------------------------------------------------------------------
(setq-default frame-title-format
              '(:eval (format "Emacs-%s: %s"
                              emacs-version
                              (or buffer-file-name
                                  dired-directory
                                  (buffer-name)))))

;; In the text-console, i.e., not in a graphic mode like X, and not on WSL
;; adjust the BACKSPACE key's behavior.
(or (display-graphic-p)
    (getenv "WSL_DISTRO_NAME")
    (normal-erase-is-backspace-mode 0))


;; BUILD MY `load-path' & `custom-theme-directory'
;;---------------------------------------------------------------------
(let ((lisp-dir (concat user-emacs-directory (file-name-as-directory "lisp")))
      (theme-dir (concat user-emacs-directory (file-name-as-directory "theme"))))
  (when (file-directory-p lisp-dir)
	(add-to-list 'load-path lisp-dir)
	; add subdirectories of ~/emacs.d/lisp/ to the load-path
	(let ((default-directory lisp-dir))
	  (normal-top-level-add-subdirs-to-load-path)))
  (when (file-directory-p theme-dir)
    (add-to-list 'custom-theme-load-path theme-dir)))


;; CODING SYSTEM & FONT
;;--------------------------------------------------------------------
 ;; Set default language and input method to Korean.
 ;; This enables to directly input Hangul with Hangul IME in -nw emacs.
(set-language-environment "Korean")
 ;; Now, we can toggle Hangul input mode with with C-\.
(setq default-input-method "korean-hangul")
 ;; But, we prefer utf-8.
 ;; Settings below will force buffers saved in utf-8 regardless of language-env.
 ;; This also sets other encoding settings below to utf-8:
 ;; default-coding-system for new files, keyboard input, 
 ;; terminal output, sub-process I/O. default-file-name-coding-system.
(setq buffer-file-coding-system 'utf-8)
 ;; give the first priority to utf-8 followed by euc-kr
(prefer-coding-system 'utf-8)

(defun my-set-hangul-font ()
  (set-fontset-font "fontset-default" '(#x1100 . #xffdc) "D2Coding"); hangul range
  (set-fontset-font "fontset-default" '(#xe0bc . #xf66e) "D2Coding")); user range
  
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my-set-hangul-font)
  (my-set-hangul-font))


;; PACKAGE SETTINGS
;;--------------------------------------------------------------------
;; package repository
(require 'package)
(add-to-list 'package-archives
			 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-on-del-error-function #'ignore)

;; MODE CUSTOMIZATIONS
;;--------------------------------------------------------------------

;; daemon mode
(defun not-windowed-on-other-frame ()
  "Return t if the current buffer is NOT being displayed on other frames,
nil otherwise.

This function is intended to be used as with 'kill-buffer-query-functions',
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

`server-save-buffers-kill-terminal' works as expected with the so called 'process-buffers'
 which started with emacsclient, i.e., files offered as arguments of emacsclient.
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

The function is called when you click the delete-frame button(upper-right corner
[X] on Windows). You can advise(defadvice) that command or you can replace it
with a function that handles that click.
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

(when (daemonp)
  ;; Load `org' beforehand, which takes time to load
  (require 'org)
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

;; lisp-interaction mode, i.e., *scratch* buffer
(add-hook 'lisp-interaction-mode-hook
          #'(lambda ()
             ;; (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-print-last-sexp)
             ;; Don't truncate outputs with the ellipsis(...)
             (setq eval-expression-print-length nil
                   eval-expression-print-level nil)))

;; text mode
(add-hook 'text-mode-hook
          #'(lambda ()
             (visual-line-mode 1)
             (setq line-spacing 0.15)))

;; C & C++ mode
(add-hook 'c-mode-common-hook
		  #'(lambda ()
             ;; classic Kernighan and Ritchie style instead gnu. 
			 (setq c-default-style '((java-mode . "java")
									 (awk-mode . "awk")
									 (other . "k&r")))
			 ;; Delete a contiguous block of whitespace with a single key.
			 (c-toggle-hungry-state t)
			 ;; (c-toggle-auto-newline t)
			 (local-set-key (kbd "RET") 'newline-and-indent)
             ;; (semantic-mode t)
			 (linum-mode t)))

;; python mode
(add-hook 'python-mode-hook
		  #'(lambda ()
			  (setq tab-width 4)
			  (setq python-indent 4)))

;; nxml/sgml/html mode
(defun tag-folding ()
  "add html/xml tags as hideshow's block delimiter for tag-folding."
  (add-to-list 'hs-special-modes-alist
			   (list major-mode
					 "<!--\\|<[^/>]*[^/]>"
					 "-->\\|</[^/>]*[^/]>"
					 "<!--"
					 (if (eq major-mode 'nxml-mode)
						 'nxml-forward-element
					   'sgml-skip-tag-forward)
					 nil))
  ;; Bind key to 'C-c h' and middle mouse button
  (local-set-key (kbd "C-c h") 'hs-toggle-hiding)
  (local-set-key [mouse-2] 'hs-toggle-hiding))

(dolist (hook '(nxml-mode-hook sgml-mode-hook html-mode-hook))
  (add-hook hook 'hs-minor-mode)
  (add-hook hook 'tag-folding))

;; org mode
(defun my-org-list-bullet ()
  "display the list bullet with '▸'."
  (font-lock-add-keywords
   nil
   ;; list lines start with initial spaces, followed by a dash"
   '(("^[[:space:]]*\\(-\\) "
	  (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▸")))))))

 ;; `eval-after-load' runs once after an elisp library be loaded.
 ;; whereas `mode-hook' runs on every buffer where the mode is enabled.
 ;; `eval-after-load' runs first and `mode-hook' later. 
(with-eval-after-load 'org-bullets
  (setq org-bullets-bullet-list '("▌" "□" "○" "−" "•")))

 ;; `org-todo-keywords' have to be set before org-mode starts.
 ;;  Otherwise, buffer-local variable `org-todo-keywords-1' will be messed up.
(with-eval-after-load 'org
  (require 'my-org)
  ;; use old style easy-template, i.e., <trigger TAB
  (require 'org-tempo)
  (setq org-todo-keywords '((sequence "TODO(t!)" "WORKING(w!)" "|"
                                      "CANCELED(c!)" "DONE(d!)")))
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook
                #'(lambda () (setq org-startup-folded 'content)))
    (setq org-startup-folded 'content)))

(add-hook 'org-mode-hook
           ;; Applied to derived modes too.
		  #'(lambda ()
			 (setq line-spacing 0.16
			       org-hide-emphasis-markers t
                   org-log-into-drawer t)
             (my-org-face)
             (my-org-list-bullet)
			 (org-bullets-mode t)
             (org-indent-mode t)))

;; org-journal mode
(with-eval-after-load 'org-journal
  (require 'my-org-journal)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "WORKING(w!)" "|"
                    "TODO⥱" "WORKING⥱" "DONE(d!)"))
        org-todo-keyword-faces
        '(("TODO" . org-todo) ("WORKING" . org-doing)
          ("TODO⥱" . "LightSteelBlue") ("WORKING⥱" . "LightSteelBlue")
          ("DONE" . org-done))
        org-journal-carryover-items "TODO=\"TODO\"|TODO=\"WORKING\""
        org-journal-handle-old-carryover 'my-org-journal-handle-old-carryover
        org-journal-carryover-headings-only t)
  
  ;; Give "⥱" a different color
  (font-lock-add-keywords
   'org-journal-mode
   '(("\\<TODO\\(⥱\\)\\>" 1 font-lock-keyword-face prepend)
     ("\\<WORKING\\(⥱\\)\\>" 1 font-lock-keyword-face prepend))
   'append))

;; add to global hook
(add-hook 'org-journal-after-entry-create-hook
          #'my-org-journal-insert-template)
;; add to buffer local hook
(add-hook 'org-journal-mode-hook
          #'(lambda () (add-hook 'before-save-hook
                                 #'my-delete-blank-lines -1 t)))


;; MY FUNCTIONS & KEY BINDING
;;--------------------------------------------------------------------
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun my-save-and-compile ()
   "Save current buffer and compile. Bind key to F9"
   (interactive)
   ;; Don't make a backup file
   (save-buffer 0)
   (compile "make -k"))
(global-set-key [(f9)] 'my-save-and-compile)

(defun revert-buffer-with-euc-kr ()
  "set the encoding of the current buffer to euc-kr. Bind key to F12"
  (interactive)
  (let ((coding-system-for-read 'korean-iso-8bit))
    (revert-buffer t t)))
(global-set-key [(f12)] 'revert-buffer-with-euc-kr)

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

(defun my-journal ()
  "Open/create today's journal. Bind key to C-c C-j.

This key-binding is to open/create a journal when `org-journal' is not loaded yet.
Once `org-journal' is loaded, C-c C-j is redefined to the `org-journal-new-entry' 
function. Thus, variable settings--i.e., setq part--do not happen repeatedly"
  ;; If you want `my-journal' to take C-u prefix and pass it to `org-journal-new-entry',
  ;; use (interactive "P") and remove `current-prefix-arg' setting below.
  ;; (interactive "P")
  (interactive)
  (require 'org-journal)
  (setq org-journal-dir "~/Documents/journal/"
        ;; org-journal-dir "/ssh:dks@jupiter:/home/dks/Documents/journal/"
        org-journal-file-type 'monthly
        org-journal-file-format "%Y%m"
        org-journal-date-format "%Y-%m-%d (%A)"
        org-journal-search-result-date-format org-journal-date-format
        org-journal-time-format ""
        org-journal-find-file 'find-file)

  ;; With C-u prefix, create a new entry automatically at the end
  ;; (current-prefix-arg '(4))
  (call-interactively 'org-journal-new-entry))

(global-set-key (kbd "C-c C-j") 'my-journal)

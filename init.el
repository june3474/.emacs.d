(custom-set-variables
 ; custom-set-variables was added by Custom.
 ; If you edit it by hand, you could mess it up, so be careful.
 ; Your init file should contain only one such instance.
 ; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-blinks 0)
 '(column-number-mode t)
 '(compilation-window-height 8)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   '("cf861f5603b7d22cb3545a7c63b2ee424c34d8ed3b3aa52d13abfea4765cffe7" "5185a285365a768a30ac274bdbc4437e7fd2fbe3107a1b0f2b60e900181905e0" default))
 '(default-major-mode 'text-mode t)
 '(delete-selection-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages
   '(vscode-dark-plus-theme jetbrains-darcula-theme web-beautify org-bullets ivy))
 '(scroll-bar-mode 'right)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(text-mode-hook '(text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify)))

;; Show the full path of the file visiting in the frame title
(setq-default frame-title-format
              '(:eval
                (format "Emacs-%s: %s"
                        emacs-version
                        (or
                         buffer-file-name
                         dired-directory
                         (buffer-name)))))

;; In the text-console, i.e., not in a graphic mode like X, and not on WSL
;; adjust the BACKSPACE key's behavior.
(and (not (display-graphic-p))
     (not (getenv "WSL_DISTRO_NAME"))
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
 ; Set default language and input method to Korean.
 ; This enables to directly input Hangul with Hangul IME in -nw emacs.
(set-language-environment "Korean")
 ; Now, we can toggle Hangul input mode with with C-\.
(setq default-input-method "korean-hangul")
 ; But, we prefer utf-8.
 ; Settings below will force buffers saved in utf-8 regardless of language-env.
 ; This also sets other encoding settings below to utf-8:
 ; default-coding-system for new files, keyboard input, 
 ; terminal output, sub-process I/O. default-file-name-coding-system.
(setq buffer-file-coding-system 'utf-8)
 ; give the first priority to utf-8 followed by euc-kr
(prefer-coding-system 'utf-8)

(defun my-set-hangul-font (frame)
  (set-fontset-font "fontset-default" '(#x1100 . #xffdc) "D2Coding"); hangul range
  (set-fontset-font "fontset-default" '(#xe0bc . #xf66e) "D2Coding"); user range
  ; When run in daemon mode, 'my-set-hangul-font runs only once after the first
  ; frame being created. If 'my-set-hangul-font is not in the hook, nothing is done. 
  (remove-hook 'after-make-frame-functions #'my-set-hangul-font))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my-set-hangul-font)
  (my-set-hangul-font (selected-frame)))


;; PACKAGE SETTINGS
;;--------------------------------------------------------------------
;; package repository
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-on-del-error-function #'ignore)


;; MODE CUSTOMIZATIONS
;;--------------------------------------------------------------------
;; daemon mode
(when (daemonp)
  ; Bring the newly created frame to front
  (add-hook 'server-after-make-frame-hook
            #'raise-frame))

;; lisp-interaction mode, i.e., *scratch* buffer
(add-hook 'lisp-interaction-mode-hook
          '(lambda ()
             ; (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)
             ; Don't truncate outputs with the ellipsis(...)
             (setq eval-expression-print-length nil
                   eval-expression-print-level nil)))

;; text mode
(add-hook 'text-mode-hook
          '(lambda ()
             (visual-line-mode 1)
             (setq line-spacing 0.15)))

;; C & C++ mode
(add-hook 'c-mode-common-hook
		  '(lambda ()
             ; classic Kernighan and Ritchie style instead gnu. 
			 (setq c-default-style '((java-mode . "java")
									 (awk-mode . "awk")
									 (other . "k&r")))
			 ; Delete a contiguous block of whitespace with a single key.
			 (c-toggle-hungry-state t)
			 ; (c-toggle-auto-newline t)
			 (local-set-key (kbd "RET") 'newline-and-indent)
             ; (semantic-mode t)
			 (linum-mode t)))

;; python mode
(add-hook 'python-mode-hook
		  '(lambda ()
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
  ; Bind key to 'C-c h'
  (local-set-key (kbd "C-c h") 'hs-toggle-hiding))

(dolist (hook '(nxml-mode-hook sgml-mode-hook html-mode-hook))
  (add-hook hook 'hs-minor-mode)
  (add-hook hook 'tag-folding))

;; org mode
(defun my-org-list-bullet ()
  "display the list bullet with '▸'."
  (font-lock-add-keywords
   nil
   ; list lines start with initial spaces, followed by a dash"
   '(("^[[:space:]]*\\(-\\) "
	  (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▸")))))))

(with-eval-after-load 'org-bullets
  (setq org-bullets-bullet-list '("▌" "□" "○" "−" "•")))

 ; `org-todo-keywords' have to be set before org-mode starts.
 ;  Otherwise, buffer-local variable `org-todo-keywords-1' will be messed up.
(with-eval-after-load 'org
  (require 'my-org)
  ; use old style easy-template, i.e., <trigger TAB
  (require 'org-tempo)
  (setq org-todo-keywords '((sequence "TODO(t!)" "WORKING(w!)" "|"
                                      "CANCELED(c!)" "DONE(d!)")))
  (my-org-face))

(add-hook 'org-mode-hook
		  '(lambda ()
			 (setq line-spacing 0.16
			       org-hide-emphasis-markers t
                   org-log-into-drawer t)
             ; Apply to derived modes too
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
        org-journal-carryover-items "TODO=\"TODO\"|TODO=\"WORKING\"")
  ; Give "⥱" a different color
  (font-lock-add-keywords
   'org-journal-mode
   '(("\\<TODO\\(⥱\\)\\>" 1 font-lock-keyword-face prepend)
     ("\\<WORKING\\(⥱\\)\\>" 1 font-lock-keyword-face prepend))
   'append)
  (setq org-journal-handle-old-carryover
        'my-org-journal-handle-old-carryover))

; add to global hook
(add-hook 'org-journal-after-entry-create-hook
          #'my-org-journal-insert-template)
; add to buffer local hook
(add-hook 'org-journal-mode-hook
          (lambda () (add-hook 'before-save-hook
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
   ; Don't make a backup file
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
    ; Select languages
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
  ; If you want `my-journal' to take C-u prefix and pass it to `org-journal-new-entry',
  ; use (interactive "P") and remove `current-prefix-arg' setting below.
  ; (interactive "P")
  (interactive)
  (require 'org-journal)
  (setq org-journal-dir "~/Documents/journal/"
        ; org-journal-dir "/ssh:dks@jupiter:/home/dks/Documents/journal/"
        org-journal-file-type 'monthly
        org-journal-file-format "%Y%m"
        org-journal-date-format "%Y-%m-%d (%A)"
        org-journal-search-result-date-format org-journal-date-format
        org-journal-time-format ""
        org-journal-hide-entries-p nil
        org-journal-find-file 'find-file)
  (let (; (current-prefix-arg nil)
        ; With C-u prefix, avoid the automatic creation of a new entry
        (current-prefix-arg '(4)))
    (call-interactively 'org-journal-new-entry))
  ; Fold :LOGBOOK: drawers
  (org-hide-drawer-all))
(global-set-key (kbd "C-c C-j") 'my-journal)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

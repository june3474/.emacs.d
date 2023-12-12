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
 '(delete-selection-mode t)
 '(dired-listing-switches "-ahlv --group-directories-first")
 '(ediff-split-window-function 'split-window-horizontally)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages
   '(buffer-move org-appear all-the-icons-ivy all-the-icons centaur-tabs diminish counsel expand-region web-mode org-indent my-org-faces elisp-mode prog-mode text-mode vscode-dark-plus-theme jetbrains-darcula-theme org-bullets ivy))
 '(scroll-bar-mode 'right)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(text-mode-hook '(text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(use-short-answers t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; BASIC SETUP
;;------------------------------------------------------------------------------
;; Set window(frame) title
(setq-default frame-title-format
              '(:eval (format "%s-%s: %s"
                              (if (daemonp)
                                  "Emacsclient"
                                "Emacs")
                              emacs-version
                              (or buffer-file-name
                                  dired-directory
                                  (buffer-name)))))

;; In the text-console, i.e., not in a graphic mode like X, and not on WSL
;; adjust the BACKSPACE key's behavior.
(or (display-graphic-p)
    (getenv "WSL_DISTRO_NAME")
    (normal-erase-is-backspace-mode 0))

;; A message you may want to show in *scratch* buffer
(setq-default initial-scratch-message
              ";; \"... 그러나 이건 정말일까?\" by 미미여사\n\n")

;; Disable beep
(setq ring-bell-function
      #'(lambda()
          (unless (memq this-command
                        '(isearch-abort abort-recursive-edit exit-minibuffer
                          keyboard-quit mwheel-scroll down up next-line
                          previous-line backward-char forward-char
                          cua-scroll-up cua-scroll-down))
            (ding))))


;;; `load-path' & `custom-theme-directory'
;;------------------------------------------------------------------------------
(let ((lisp-dir (concat user-emacs-directory (file-name-as-directory "lisp")))
      (theme-dir (concat user-emacs-directory (file-name-as-directory "theme"))))
  (when (file-directory-p lisp-dir)
	(add-to-list 'load-path lisp-dir)
	; add subdirectories of ~/emacs.d/lisp/ to the load-path
	(let ((default-directory lisp-dir))
	  (normal-top-level-add-subdirs-to-load-path)))
  (when (file-directory-p theme-dir)
    (add-to-list 'custom-theme-load-path theme-dir)))


;;; LANGUAGE & FONT
;;------------------------------------------------------------------------------
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

;; Underline at descent position, not baseline position
(setq x-underline-at-descent-line t)


;;; BASE PACKAGES
;;------------------------------------------------------------------------------
;; package
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))
;; allow upgrading built-in packages
(setq package-install-upgrade-built-in t)
;; export load-path and autoloads of installed packages,b
;; but does not load packages.
(package-initialize)

;; use-package
(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))


;;; CONFIGURATION
;;------------------------------------------------------------------------------

(require 'my/misc-funcs)
(require 'my/config)
(when (daemonp)
  (require 'my/config-daemon))
(require 'my/key-binding)

;;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;; my/config.el --- My package customization

;;; recentf-mode
(use-package recentf
  :bind
  ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode 1))

;;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; ivy, load now!
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function #'ignore)
  (ivy-mode 1))

;;; visual-line mode
(use-package simple
  :hook
  (text-mode . visual-line-mode)
  (org-mode . visual-line-mode)
  :config
  (setq word-wrap-by-category t)
  ;; add ",-./" and ";" to 'line-breakable characters(?|)'
  ;; refer to `describe-categories' func
  (modify-category-entry '(?, . ?/) ?|)
  (modify-category-entry ?\; ?|))

;;; text-mode
(use-package text-mode
  :hook
  ;; (setq line-spacing 0.15) doesn't work inside :config
  ;; text-mode is the grand parent mode of org-mode
  (text-mode . (lambda () (setq line-spacing 0.15))))

;;; display-linenumbers-mode
(use-package display-line-numbers
  :hook
  (prog-mode emacs-lisp-mode))

;;; emacs-lisp-mode, e.g., *scratch* buffer
(use-package elisp-mode
  :init
  (unbind-key "<C-return>" cua-global-keymap)
  :bind  
  (:map lisp-interaction-mode-map ("<C-return>" . eval-print-last-sexp))
  :config
  ;; Don't truncate outputs with the ellipsis(...)
  (setq eval-expression-print-length nil
        eval-expression-print-level nil))

;;; C & C++ mode
(use-package cc-vars
  :defer t
  :config
  ;; classic Kernighan and Ritchie style instead gnu.
  (add-to-list 'c-default-style '(c-mode . "k&r")))
  
(use-package cc-mode
  :defer t
  :config
  (setq c-basic-offset 4
        indent-tabs-mode nil)
  ;; delete a contiguous block of whitespace with a single key.
  (c-toggle-hungry-state t))

;;; python-mode
(use-package python
  :defer t
  :config
  (setq python-indent 4))

;;; web-mode
;; This mode doesn't seem to help much with normal html files.
;; Shame on that emacs doesn't have a good html formatter yet.
(use-package web-mode
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.blade\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.hbs\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  ("\\.[x]?html?\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-indentation t)
  ;; override fontface
  (face-spec-reset-face 'web-mode-html-tag-bracket-face nil)
  (set-face-attribute 'web-mode-html-attr-name-face nil
                      :foreground 'unspecified
                      :inherit font-lock-variable-name-face)
  (set-face-attribute 'web-mode-html-tag-face nil
                      :foreground 'unspecified
                      :inherit font-lock-function-name-face))

;;; org-mode

(use-package org-faces
  :defer t
  :config
  (require 'my/org-faces))

(use-package org-indent
  :defer t
  :config
  (require 'my/org-indent))

(use-package org-bullets
  :defer t
  :config
  (setq org-bullets-bullet-list '("▌" "□" "○" "−" "•"))
  ;; display the list bullet with '▸'."
  (font-lock-add-keywords
   nil
   ;; lines starting with spaces, followed by a dash"
   '(("^[[:space:]]*\\(-\\) "
	  (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▸")))))))

(use-package org-appear  
  :defer t
  :config
  (setq org-appear-autolinks t
        org-appear-autoentities t
        org-appear-autokeywords t))

;; Separate the configurations in order to apply the same configurations,
;; regardless that `org' is loaded early in the daemon mode--as shown in
;; the below-- or loaded later in non-daemon mode.
;;
;; `eval-after-load' runs once after an elisp library be loaded.
;; whereas `mode-hook' runs on every buffer where the mode is enabled.
;; `eval-after-load' runs first and `mode-hook' later.
(with-eval-after-load 'org
  (require 'org-faces)
  (require 'org-indent)
  (require 'org-bullets)
  (require 'org-appear)
  ;; use old style easy-template, i.e., <trigger TAB
  ;; `org-tempo' has no autoload function nor variable
  (require 'org-tempo)
  ;; mode settings would be better to go on the buffer-basis with a hook
  (add-hook 'org-mode-hook
            #'(lambda ()
                (org-indent-mode 1)
                (org-bullets-mode 1)
                (org-appear-mode 1)))
  (setq org-startup-folded 'content
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-log-into-drawer t))

;; load `org', an old-school way rather than use-package 
(when (daemonp)
  (require 'org))

;;; org-journal-mode
(use-package my/org-journal
  :bind
  ("C-c C-j" . new-journal)
  ;; load immediatly when daemon mode
  :config
  (setq org-journal-dir "~/Documents/journal/"
        ;; org-journal-dir "/ssh:dks@jupiter:/home/dks/Documents/journal/"
        org-journal-file-type 'monthly
        org-journal-file-format "%Y%m"
        org-journal-date-format "%Y-%m-%d (%A)"
        org-journal-search-result-date-format org-journal-date-format
        org-journal-time-format ""
        org-journal-find-file 'find-file))


(provide 'my/config)

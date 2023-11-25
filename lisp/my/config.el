;;; my/config.el --- My package customization

;;; recentf
(use-package recentf
  :bind
  ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode 1))

;;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; diminish
(use-package diminish)

;;; ivy & counsel
(use-package ivy
  :demand t  ;; load Now!
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function #'ignore)
  (ivy-mode 1)
  (use-package counsel
    :demand t
    :bind (("M-x" . counsel-M-x)
           ("C-x b" . counsel-ibuffer)
           ("C-x C-f" . counsel-find-file)
           ;; ("C-M-j" . counsel-switch-buffer)
           ("C-M-l" . counsel-imenu)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history))
    :custom
    (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
    :config
    ;; Don't start searches with ^   
    (setq ivy-initial-inputs-alist nil)
    (counsel-mode 1)))

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

;;; elisp mode, e.g., *scratch* buffer
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
(use-package cc-mode
  :defer t
  :init
  (use-package cc-vars
    :defer t
    :config
    ;; classic Kernighan and Ritchie style instead gnu.
    (add-to-list 'c-default-style '(c-mode . "k&r"))
    (setq c-basic-offset 4))
  (use-package cc-cmds
    :defer t
    :config
    ;; delete a contiguous block of whitespace with a single key.
    (c-toggle-hungry-state 1))
  :config
  (setq indent-tabs-mode nil))

;;; python mode
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

;;; org mode
(use-package org-faces
  :defer t
  :config
  (require 'my/org-faces))

(use-package org-indent
  :hook org-mode  ;; imply defer
  :config
  (require 'my/org-indent))

(use-package org-bullets
  :hook org-mode
  :init
  (add-hook 'org-mode-hook
            ;; display the list bullet with '▸'."
            #'(lambda ()
                (font-lock-add-keywords
                 nil
                 ;; lines starting with spaces, followed by a dash"
                 '(("^[[:space:]]*\\(-\\) "
	                (0 (prog1 () (compose-region (match-beginning 1)
                                                 (match-end 1) "▸"))))))))  
  :config
  (setq org-bullets-bullet-list '("▌" "□" "○" "−" "•")))

(use-package org-appear  
  :hook org-mode
  :config
  (setq  org-appear-trigger 'on-change
         org-appear-autolinks t
         org-appear-autoentities t
         org-appear-autokeywords t))

(use-package org
  :defer t
  :config
  ;; use old style easy-template, i.e., <trigger TAB
  ;; `org-tempo' has no autoload function nor variable
  (require 'org-tempo)
  (setq org-startup-folded 'content
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-log-into-drawer t))  

;;; org-journal mode
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

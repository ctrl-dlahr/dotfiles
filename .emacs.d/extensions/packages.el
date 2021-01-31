;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package smex
  :ensure t
  :config
  (smex-initialize)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands))


(use-package ido-completing-read+
  :ensure t
  :config
 (ido-mode 1)
 (ido-everywhere 1))

(use-package try
  :ensure t
  :commands try)

(use-package yasnippet
  :ensure t)

(use-package company-lsp
  :ensure t)

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'")

(use-package log4j-mode
  :ensure t
  :mode "\\.log\\'")

;; prettify orgmode
(use-package org
  :ensure t
  :config
  (setq org-agenda-files (list "~/org"))
  (progn (custom-set-faces ;Get rid of the different font sizes on headers
                  '(org-document-title ((t (:inherit outline-1 :height 1.0 :underline nil))))
                  '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
                  '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
                  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
                  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
                  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))))
  :init (org-babel-do-load-languages
         'org-babel-load-languages
         '((awk . t)
           (C . t)
           (calc . t)
           (clojure . t)
           (css . t)
           (haskell . t)
           (java . t)
           (js . t)
           (latex . t)
           (lisp . t)
           (makefile . t)
           (perl . t)
           (python . t)
           (ruby . t)
           (screen . t)
           (sql . t)
           (sqlite . t))))

;; #+SEQ_TODO: ❢ ☯ ⧖ | ☺ ✔ ⌚ ✘
(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-todo-keywords '((sequence "❢ TODO(t)")
                            (sequence "✔ DONE(w)")
                            (sequence "☯ IN PROGRESS(p)")
                            (sequence "☺ REPORT(r)")
                            (sequence "⌚ LATER(l)")
                            (sequence "⧖ WAITING(w)")
                            (sequence "✘ CANCELED(c)"))))

(use-package magit
  :ensure t
  :bind
  ("M-m g g" . magit-status)
  ("M-m g m" . magit-blame)
  :config (magit-add-section-hook 'magit-status-sections-hook
                                  'magit-insert-unpushed-to-upstream
                                  'magit-insert-unpushed-to-upstream-or-recent
                                  'replace))

;; visual replace
(use-package visual-regexp
  :ensure t
  :config
  (define-key global-map (kbd "M-m r r") 'vr/replace)
  (define-key global-map (kbd "M-m r q") 'vr/query-replace)
  ;; if you use multiple-cursors, this is for you:
  (define-key global-map (kbd "M-m r m") 'vr/mc-mark))

;; see undo
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'redo))

;; move text up and down
(use-package drag-stuff
  :ensure t
  :config
  (global-set-key (kbd "M-<up>")   #'drag-stuff-up)
  (global-set-key (kbd "M-<down>") #'drag-stuff-down))

;; colors parens by depth
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; switch between windows with shift + direction
(use-package windmove
  :config
  (windmove-default-keybindings 'shift))

;; show the commands
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode))

(use-package vlf
  :ensure t)


;; TODO: Script to install PDF program

;; (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")

;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install)
;;   (setq-default pdf-view-display-size 'fit-page))


(use-package all-the-icons
  :ensure t
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfM-mode)
  :mode (("README\\.md\\'" . gfM-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; (setq markdown-command "/usr/local/bin/pandoc")

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'redo))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "M-m c e")  'mc/edit-lines)
  (global-set-key (kbd "C->")  'mc/mark-next-like-this)
  (global-set-key (kbd "C-<")  'mc/mark-previous-like-this)
  (global-set-key (kbd "M-m c =")  'mc/mark-all-like-this)
  (global-set-key (kbd "M-m c \"") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "M-m c :")  'mc/skip-to-previous-like-this))

(provide 'packages)

;;; packages.el ends here

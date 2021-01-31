;;; package --- Summary
;;; Commentary:
;;; Code:

(setq gc-cons-threshold 100000000)

(defun my-lower-gc-cons-threshold ()
  (setq gc-cons-threshold 800000)
  (remove-hook 'focus-out-hook #'my-lower-gc-cons-threshold))

(add-hook 'after-init-hook
          (lambda ()
            (run-with-idle-timer
             60
             nil
             #'my-lower-gc-cons-threshold)
            (add-hook 'focus-out-hook #'my-lower-gc-cons-threshold)))

(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(dolist (package '(use-package))
   (unless (package-installed-p package)
     (package-install package)))

(eval-and-compile
  (add-to-list 'load-path (concat user-emacs-directory "extensions/")))

;; set for more ergonomic-like key chords
(dolist (key '("\M-m"))
  (global-unset-key key))

;; use-packages that are not language specific
(require 'packages)
(require 'dired+)
(require 'my-functions)
(require 'appearance)
(require 'lang-ruby)
(require 'lang-web)
(require 'lang-java)

(global-set-key (kbd "<escape>") 'keyboard-quit)

;; -------~-------~--~------------------~------
;; EMACS SETTINGS
;; -------~-------~--~------------------~------

;; Core settings
;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Change  size
(global-set-key (kbd "M-m <left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-m <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-m <down>") 'shrink-window)
(global-set-key (kbd "M-m <up>") 'enlarge-window)

(setq-default line-spacing 2)
(setq-default indent-tabs-mode nil)

;; open in same frame
(setq ns-pop-up-frames nil)

(global-auto-revert-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(delete-selection-mode t)

;; able to use y or n instead of yes and n
(fset 'yes-or-no-p 'y-or-n-p)

;; no start up screen
(setq inhibit-startup-message t)

;; no backup files
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #auto-save# files
(setq ns-confirm-quit 1)

;; show matching parens
(show-paren-mode 1)
;; set tabbing to TAB
;; additional tabbing in web-mode
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;; used in OS X. Disable sRGB before setting up spaceline.
(when (memq window-system '(mac ns))
  (defvar ns-use-srgb-colorspace nil))

(cua-mode t)

;; -------~-------~--~------------------~------
;; KEY BINDINGS
;; -------~-------~--~------------------~------

(define-key global-map (kbd "M-m l n") 'global-display-line-numbers-mode)
(define-key global-map (kbd "M-m v") 'select-current-line)
(define-key global-map (kbd "C-`") 'toggle-flycheck-error-buffer)
(define-key prog-mode-map (kbd "C-c w") 'better-whitespace)
(global-set-key (kbd "C-c p") 'find-file-at-point)
(global-set-key (kbd "C-c i m") 'imenu)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "4641f2added941818ca5a618aa38206d6dd6c2fa553137e2d0e1c82073b8674a" "72d880c2fd3fbf02abd467608fd2bd33635e7a5b8d0fc3eea2194fcc7e69fa72" default))
 '(package-selected-packages
   '(overcast-theme dap-java helm-lsp yasnippet yaml-mode whitespace-cleanup-mode which-key web-mode-edit-element vlf visual-regexp use-package undo-tree try sunburn-theme smex rvm robe rainbow-mode rainbow-delimiters projectile-rails phpunit php-mode org-bullets multiple-cursors modus-themes magit lsp-ui lsp-java log4j-mode inkpot-theme indent-guide ido-completing-read+ iceberg-theme haml-mode green-is-the-new-black-theme flycheck enh-ruby-mode emmet-mode dumb-jump drag-stuff dockerfile-mode cycle-themes csv-mode company-lsp color-theme-sanityinc-tomorrow all-the-icons-dired ahungry-theme))
 '(whitespace-style
   '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit outline-1 :height 1.0 :underline nil))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

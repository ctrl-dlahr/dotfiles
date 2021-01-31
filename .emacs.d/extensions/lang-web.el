;;; package --- Summary
;;; Commentary:
;; web mode configuration
;;

;;; Code:
(use-package web-mode
  :ensure t
  :bind (("C-c ]" . emmet-next-edit-point)
         ("C-c [" . emmet-prev-edit-point)
         ("C-c o b" . browse-url-of-file))
  :mode
  (("\\.js\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.phtml?\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.jsx$" . web-mode)
   ("\\.blade.php\\'" . web-mode)))

:config
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)
(setq web-mode-engines-alist
  '(("php"    . "\\.phtml\\'")
    ("blade"  . "\\.blade\\.")))
  
(add-hook 'web-mode-hook 'jsx-flycheck)

(use-package php-mode
  :ensure t)

(use-package phpunit
  :ensure t)

;; highlight enclosing tags of the element under cursor
(setq web-mode-enable-current-element-highlight t)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; editing enhancements for web-mode
;; https://github.com/jtkDvlp/web-mode-edit-element
(use-package web-mode-edit-element
  :ensure t
  :config (add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode))

                                        ; snippets for HTML
;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :ensure t
  :init (setq emmet-move-cursor-between-quotes t) ;; default nil
  :diminish (emmet-mode . " e"))
(add-hook 'web-mode-hook 'emmet-mode)

(defun my-web-mode-hook ()
  "Hook for `web-mode' config for company-backends."
  (set (make-local-variable 'company-backends)
       '((company-tern company-css company-web-html company-files))))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Enable JavaScript completion between <script>...</script> etc.
(defadvice company-tern (before web-mode-set-up-ac-sources activate)
  "Set `tern-mode' based on current language before running company-tern."
  (message "advice")
  (if (equal major-mode 'web-mode)
      (let ((web-mode-cur-language
             (web-mode-language-at-pos)))
        (if (or (string= web-mode-cur-language "javascript")
                (string= web-mode-cur-language "jsx"))
            (unless tern-mode (tern-mode))
          (if tern-mode (tern-mode -1))))))
(add-hook 'web-mode-hook 'company-mode)

;; configure CSS mode company backends
(use-package css-mode
  :ensure t
  :config
  (defun my-css-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-css company-dabbrev-code company-files))))
  (add-hook 'css-mode-hook 'my-css-mode-hook)
  (add-hook 'css-mode-hook 'company-mode))

(use-package indent-guide
  :ensure t
  :init (add-hook 'yaml-mode-hook 'indent-guide-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.sls\\'")
  :init
  (add-hook 'yaml-mode-hook 'turn-off-auto-fill))

;; show colors in css, etc.
(use-package rainbow-mode
  :ensure t)

(provide 'lang-web)
;;; lang-web.el ends here

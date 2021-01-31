;; ruby shell
(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

(use-package enh-ruby-mode
  :ensure t
  :mode
  (("\\.rb\\'" . ruby-mode)))

(use-package robe
  :ensure t
  :config
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (eval-after-load 'company
      '(push 'company-robe company-backends))))

;; projectile-rails config
(use-package projectile-rails
  :ensure t
  :config
  (projectile-rails-global-mode))

(use-package haml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.haml\\'" . yaml-mode)))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(provide 'lang-ruby)

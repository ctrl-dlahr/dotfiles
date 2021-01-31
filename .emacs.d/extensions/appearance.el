;;; package --- Summary
;;; Commentary:
;;; Code:

(setq ns-use-srgb-colorspace nil)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(load-theme 'desert t)

;; turn off highlight current line
(global-hl-line-mode 0)

(whitespace-mode 1)
(custom-set-variables
 '(whitespace-style
   '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark)))

(set-face-attribute 'whitespace-space nil :background nil :foreground "gray25")

(set-frame-font "SF Mono-14")

(set-face-attribute 'hl-line nil :inherit nil :background "gray10")


;; dired
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")


(provide 'appearance)

;;; appearance.el ends here

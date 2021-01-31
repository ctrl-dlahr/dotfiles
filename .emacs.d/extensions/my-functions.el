;;; package --- Summary
;;; Commentary:
;;; Code:

(defun toggle-flycheck-error-buffer ()
  "Toggle a flycheck error buffer."
  (interactive)
  (if (string-match-p "Flycheck errors" (format "%s" (window-list)))
      (dolist (w (window-list))
        (when (string-match-p "*Flycheck errors*" (buffer-name (window-buffer w)))
          (delete-window w)))
    (flycheck-list-errors)))

(defun text-mode-hook ()
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (flymake-mode-off))
(add-hook 'text-mode-hook 'text-mode-hook)

(defun purge-buffers ()
  "Delete all buffers, except for *scratch*."
  (interactive)
  (mapc #'(lambda (b) (unless (sting= (buffer-name b) "*scratch*") (kill-buffer b))) (buffer-list)))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (vlf-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(defun better-whitespace ()
  (interactive)
  (whitespace-mode -1)
  (let ((ws-small '(face lines-tail))
        (ws-big '(face tabs spaces trailing lines-tail space-before-tab
                       newline indentation empty space-after-tab space-mark
                       tab-mark newline-mark)))
    (if (eq whitespace-style ws-small)
        (setq whitespace-style ws-big)
      (setq whitespace-style ws-small)))
  (whitespace-mode 1))

(provide 'my-functions)

;;; my-functions.el ends here

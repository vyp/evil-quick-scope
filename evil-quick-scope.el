(defface evil-qs-hl
  '((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 16) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 16) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'basic-faces)

(ov (+ (point-at-bol) 5) (+ (point-at-bol) 6) 'face 'evil-qs-hl 'evil-qs-hl-1 t)
(ov (+ (point-at-bol) 7) (+ (point-at-bol) 8) 'face 'evil-qs-hl 'evil-qs-hl-2 t)

(defvar evil-qs-last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'evil-qs-last-post-command-position)

(defun evil-qs-refresh-if-moved-post-command ()
  (unless (equal (point) evil-qs-last-post-command-position)
    (ov-clear 'evil-qs-hl-1))
  (setq evil-qs-last-post-command-position (point)))

(add-to-list 'post-command-hook #'evil-qs-refresh-if-moved-post-command)


;; (ov (+ (point-at-bol) 5) (+ (point-at-bol) 6) 'face '(:background "#00ff00") 'ov1 t)

;; (ov 5 17 'face 'warning)
;; (ov-clear)
;; (message (thing-at-point 'line))
;; (ov (point-min) (point) 'face '(:background "#00ff00"))

;; (message "%d" (+ (point-at-bol) 4))

;; (defun evil-qs-refresh ()
;;   (ov-clear 'ov1))

;; (add-hook 'evil-motion-state-exit-hook 'evil-qs-refresh)
;; (ov-clear 'ov1)
;; (ov-clear)

;; (defadvice evil-backward-char (after evil-qs-refresh)
;;   (ov-clear))

;; (advice-add 'evil-backward-char :after #'ov-clear)

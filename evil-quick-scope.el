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
  "Highlighting face used by evil-quick-scope."
  :group 'basic-faces)

(defun evil-qs-highlight-current-line ()
  (let ((accepted-chars '(("a" . 0)
                          ("b" . 0)
                          ("c" . 0)
                          ("d" . 0)
                          ("e" . 0)
                          ("f" . 0)
                          ("g" . 0)
                          ("h" . 0)
                          ("i" . 0)
                          ("j" . 0)
                          ("k" . 0)
                          ("l" . 0)
                          ("m" . 0)
                          ("n" . 0)
                          ("o" . 0)
                          ("p" . 0)
                          ("q" . 0)
                          ("r" . 0)
                          ("s" . 0)
                          ("t" . 0)
                          ("u" . 0)
                          ("v" . 0)
                          ("w" . 0)
                          ("x" . 0)
                          ("y" . 0)
                          ("z" . 0)
                          ("A" . 0)
                          ("B" . 0)
                          ("C" . 0)
                          ("D" . 0)
                          ("E" . 0)
                          ("F" . 0)
                          ("G" . 0)
                          ("H" . 0)
                          ("I" . 0)
                          ("J" . 0)
                          ("K" . 0)
                          ("L" . 0)
                          ("M" . 0)
                          ("N" . 0)
                          ("O" . 0)
                          ("P" . 0)
                          ("Q" . 0)
                          ("R" . 0)
                          ("S" . 0)
                          ("T" . 0)
                          ("U" . 0)
                          ("V" . 0)
                          ("W" . 0)
                          ("X" . 0)
                          ("Y" . 0)
                          ("0" . 0)
                          ("1" . 0)
                          ("2" . 0)
                          ("3" . 0)
                          ("4" . 0)
                          ("5" . 0)
                          ("6" . 0)
                          ("7" . 0)
                          ("8" . 0)
                          ("9" . 0)))

        ;; (current-line (split-string (thing-at-point 'line) "[-_ \f\t\n\r\v]"))
        (current-line (thing-at-point 'line))
        (start (point-at-bol))
        (end (point-at-eol))
        (cursor 0)
        (before-cursor "")
        (after-cursor ""))

    (setq cursor (- (point) start)
          before-cursor (substring current-line 0 cursor)
          after-cursor (substring current-line cursor (- end start)))
        ;; (words-in-line (length current-line))
        ;; (cursor-word 0))


    ;; (setq cursor-word (dotimes (n words-in-line cursor-word)

    ;;     (cursor-word (catch 'break
    ;;                    (while (< n words-in-line)
    ;;                      (if (equal )

    ;; (while (< n words-in-line)




    after-cursor))

(message "%s" (evil-qs-highlight)) ;; omg lol

(split-string "  hello--world foo_bar" "[-_ \f\t\n\r\v]")
(message "%d" (point))

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

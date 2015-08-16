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
  (unless (< (length (split-string (thing-at-point 'line) "[-_ \f\t\n\r\v]+")) 4)
    (let ((accepted-chars '(("a" . 0) ("n" . 0) ("A" . 0) ("N" . 0) ("0" . 0)
                            ("b" . 0) ("o" . 0) ("B" . 0) ("O" . 0) ("1" . 0)
                            ("c" . 0) ("p" . 0) ("C" . 0) ("P" . 0) ("2" . 0)
                            ("d" . 0) ("q" . 0) ("D" . 0) ("Q" . 0) ("3" . 0)
                            ("e" . 0) ("r" . 0) ("E" . 0) ("R" . 0) ("4" . 0)
                            ("f" . 0) ("s" . 0) ("F" . 0) ("S" . 0) ("5" . 0)
                            ("g" . 0) ("t" . 0) ("G" . 0) ("T" . 0) ("6" . 0)
                            ("h" . 0) ("u" . 0) ("H" . 0) ("U" . 0) ("7" . 0)
                            ("i" . 0) ("v" . 0) ("I" . 0) ("V" . 0) ("8" . 0)
                            ("j" . 0) ("w" . 0) ("J" . 0) ("W" . 0) ("9" . 0)
                            ("k" . 0) ("x" . 0) ("K" . 0) ("X" . 0)
                            ("l" . 0) ("y" . 0) ("L" . 0) ("Y" . 0)
                            ("m" . 0) ("z" . 0) ("M" . 0) ("Z" . 0)))

          ;; TOOD: Abort highlight if line is not long enough.
          (current-line (thing-at-point 'line))
          (beg (point-at-bol))
          (end (point-at-eol))
          (cursor)

          (before-cursor-string "")
          (after-cursor-string "")
          (after-cursor)
          (before-cursor)

          (word-seps-reg "[-_ \f\t\n\r\v]")

          (pri-chars-to-hl '())
          (sec-chars-to-hl '())
          (pri-to-hl)
          (sec-to-hl))

      (setq cursor (- (point) beg)

            before-cursor-string (substring current-line 0 cursor)
            after-cursor-string (substring current-line cursor (- end beg))

            before-cursor (split-string before-cursor-string word-seps-reg)
            after-cursor (split-string after-cursor-string word-seps-reg))


      (let ((pos cursor)
            (accepted)
            (first-word t)
            (found-pri-char nil)
            (found-sec-char nil))

        (dolist (word (reverse before-cursor))
          (setq found-pri-char nil
                found-sec-char nil)

          (dolist (char (reverse (append word nil)))
            (setq pos (- pos 1)
                  char (string char)
                  accepted (cdr (assoc char accepted-chars)))

            (unless (eq accepted nil)
              (add-to-list 'accepted-chars `(,char . ,(+ 1 accepted)))

              (if (eq 0 accepted)
                  (progn
                    (setq pri-to-hl (+ beg pos)
                          found-pri-char t))

                (if (eq 1 accepted)
                    (progn
                      (setq sec-to-hl (+ beg pos)
                            found-sec-char t))))))

          (if (eq found-pri-char t)
              (setq pri-chars-to-hl (append pri-chars-to-hl (list pri-to-hl))))

          (if (eq found-sec-char t)
              (setq sec-chars-to-hl (append sec-chars-to-hl (list sec-to-hl))))

          (setq pos (- pos 1))))

      ;; (dolist (pos pri-chars-to-hl)

      pri-chars-to-hl)))

(defun evil-qs-show-line ()
  (let ((current-line (thing-at-point 'line))
        (current-line-list))

    (setq current-line-list (split-string current-line "[-_ \f\t\n\r\v]+"))
    (dolist (word current-line-list)
      (message "%s" word))
    (message "%d" (length current-line-list))
    nil))

(message "%s" (evil-qs-highlight-current-line))
(append '(1 3) (list 5))
(string 101)

(setq my-assoc-list '(("a" . 1)
                      ("b" . 2))
      my-key "a")

(add-to-list 'my-assoc-list `(,my-key . 7))
(cdr (assoc my-key my-assoc-list))

(split-string "  hello--world foo_bar" "[-_ \f\t\n\r\v]")
(message "%d" (point))

(ov (+ (point-at-bol) 5) (+ (point-at-bol) 6) 'face 'evil-qs-hl 'evil-qs-hl-1 t)
(ov (+ (point-at-bol) 7) (+ (point-at-bol) 8) 'face 'evil-qs-hl 'evil-qs-hl-1 t)
(ov (+ (point-at-bol) 7) (+ (point-at-bol) 8) 'face 'evil-qs-hl 'evil-qs-hl-2 t)

(defvar evil-qs-last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'evil-qs-last-post-command-position)

;; TODO: Refresh only if cursor moved to a new word.
(defun evil-qs-refresh-if-moved-post-command ()
  (unless (equal (point) evil-qs-last-post-command-position)
    (ov-clear 'evil-qs-hl-1))
  (setq evil-qs-last-post-command-position (point)))

(add-hook 'post-command-hook #'evil-qs-refresh-if-moved-post-command nil t)
(remove-hook 'post-command-hook #'evil-qs-refresh-if-moved-post-command t)


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

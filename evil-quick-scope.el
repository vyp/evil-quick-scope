;; TODO: Abort highlighting if before-cursor/after-cursor is too short.
;;
;; TODO: Make global minor mode so that it can be turned off and on.
;;       - Remember, should only be on in evil mode's normal and visual states.
;;
;; TODO: Comment header (with the "Package-Requires:" dependencies line).
;;
;; TODO: Readme: What, Installation, Screenshots?, Roadmap.
;;
;;       - Roadmap:
;;
;;         - Highlight all visible lines if `evil-cross-lines` is true.
;;
;;         - Option to only turn on highlighting after pressing `fFtT;,`.
;;
;;         - Option to disable highlighting long lines.
;;
;;         - Option to have a different face for the current line in case of
;;           using hl-line mode.
;;
;;       - Also remember to mention the current version of ov being used.
;;
;; TODO: Instead of just checking for the number of words, check for the number
;; of words *with accepted characters in them*, to determine when to start
;; highlighting on the line.

(require 'ov)

(defvar evil-qs-last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'evil-qs-last-post-command-position)

(defface evil-qs-forward-primary
  '((t (:foreground "#c3744a")))
  "Highlights primary matches after the cursor.")

(defface evil-qs-forward-secondary
  '((t (:foreground "#a96540")))
  "Highlights secondary matches after the cursor.")

(defface evil-qs-backward-primary
  '((t (:foreground "#c3744a")))
  "Highlights primary matches before the cursor.")

(defface evil-qs-backward-secondary
  '((t (:foreground "#a96540")))
  "Highlights secondary matches before the cursor.")

(defun evil-qs-highlight-forward-primary (positions)
  (dolist (pos positions)
    (ov pos (+ 1 pos ) 'face 'evil-qs-forward-primary 'evil-qs-fwd-pri t)))

(defun evil-qs-highlight-forward-secondary (positions)
  (dolist (pos positions)
    (ov pos (+ 1 pos ) 'face 'evil-qs-forward-secondary 'evil-qs-fwd-sec t)))

(defun evil-qs-highlight-backward-primary (positions)
  (dolist (pos positions)
    (ov pos (+ 1 pos ) 'face 'evil-qs-backward-primary 'evil-qs-bwd-pri t)))

(defun evil-qs-highlight-backward-secondary (positions)
  (dolist (pos positions)
    (ov pos (+ 1 pos ) 'face 'evil-qs-backward-secondary 'evil-qs-bwd-sec t)))

(defun evil-qs-highlight ()
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

          (occurrences)

          (current-line (thing-at-point 'line))
          (beg (point-at-bol))
          (end (point-at-eol))
          (cursor)

          (before-cursor-string "")
          (after-cursor-string "")
          (after-cursor)
          (before-cursor)

          (word-seps-reg "[-_ \f\t\n\r\v]"))
          ;; (word-seps-reg-greedy "[-_ \f\t\n\r\v]+"))

      (setq occurrences accepted-chars
            cursor (- (point) beg)

            before-cursor-string (substring current-line 0 cursor)
            after-cursor-string (substring current-line cursor (- end beg)))

      (let ((pos cursor)
            (accepted)

            (first-word t)
            (first-char t)

            (found-pri-char nil)
            (found-sec-char nil)

            (pri-chars-to-hl '())
            (sec-chars-to-hl '())
            (pri-to-hl)
            (sec-to-hl)

            (after-cursor (split-string after-cursor-string word-seps-reg)))

        (dolist (word after-cursor)
          (setq found-pri-char nil
                found-sec-char nil)

          (dolist (char (append word nil))
            (unless (eq first-char t)
              (setq char (string char)
                    accepted (cdr (assoc char occurrences)))

              (unless (eq accepted nil)
                (add-to-list 'occurrences `(,char . ,(+ 1 accepted)))

                (unless (or (eq found-pri-char t) (eq found-sec-char t))
                  (unless (eq first-word t)
                    (if (eq 0 accepted)
                        (progn
                          (setq pri-to-hl (+ beg pos)
                                found-pri-char t))

                      (if (eq 1 accepted)
                          (progn
                            (setq sec-to-hl (+ beg pos)
                                  found-sec-char t))))))))

            (setq first-char nil)
            (setq pos (+ pos 1)))

          (if (eq found-pri-char t)
              (setq pri-chars-to-hl (append pri-chars-to-hl (list pri-to-hl)))

            (if (eq found-sec-char t)
                (setq sec-chars-to-hl (append sec-chars-to-hl (list sec-to-hl)))))

          (if (eq first-word t)
              (setq first-word nil))

          (setq pos (+ pos 1)))

        (evil-qs-highlight-forward-primary pri-chars-to-hl)
        (evil-qs-highlight-forward-secondary sec-chars-to-hl))

      ;; Reset occurrences because now we are counting in a different direction.
      (setq occurrences accepted-chars)

      (let ((pos cursor)
            (accepted)

            (first-word t)
            (found-pri-char nil)
            (found-sec-char nil)

            (pri-chars-to-hl '())
            (sec-chars-to-hl '())
            (pri-to-hl)
            (sec-to-hl)

            (before-cursor (split-string before-cursor-string word-seps-reg)))

        (dolist (word (reverse before-cursor))
          (setq found-pri-char nil
                found-sec-char nil)

          (dolist (char (reverse (append word nil)))
            (setq pos (- pos 1)
                  char (string char)
                  accepted (cdr (assoc char occurrences)))

            (unless (eq accepted nil)
              (add-to-list 'occurrences `(,char . ,(+ 1 accepted)))

              (unless (eq first-word t)
                (if (eq 0 accepted)
                    (progn
                      (setq pri-to-hl (+ beg pos)
                            found-pri-char t))

                  (if (eq 1 accepted)
                      (progn
                        (setq sec-to-hl (+ beg pos)
                              found-sec-char t)))))))

          (if (eq found-pri-char t)
              (setq pri-chars-to-hl (append pri-chars-to-hl (list pri-to-hl)))

            (if (eq found-sec-char t)
                (setq sec-chars-to-hl (append sec-chars-to-hl (list sec-to-hl)))))

          (if (eq first-word t)
              (setq first-word nil))

          (setq pos (- pos 1)))

        (evil-qs-highlight-backward-primary pri-chars-to-hl)
        (evil-qs-highlight-backward-secondary sec-chars-to-hl)))))

(defun evil-qs-clear ()
  (ov-clear 'evil-qs-fwd-pri)
  (ov-clear 'evil-qs-fwd-sec)
  (ov-clear 'evil-qs-bwd-pri)
  (ov-clear 'evil-qs-bwd-sec))

(defun evil-qs-refresh ()
  (evil-qs-clear)
  (evil-qs-highlight))

(defun evil-qs-refresh-if-moved-post-command ()
  (unless (equal (point) evil-qs-last-post-command-position)
    (evil-qs-refresh))

  (setq evil-qs-last-post-command-position (point)))

(defun evil-qs-start ()
  (add-hook 'post-command-hook #'evil-qs-refresh-if-moved-post-command nil t))

(defun evil-qs-stop ()
  (evil-qs-clear)
  (remove-hook 'post-command-hook #'evil-qs-refresh-if-moved-post-command t))

(evil-qs-start)
(add-hook 'evil-insert-state-entry-hook #'evil-qs-stop nil t)
(add-hook 'evil-insert-state-exit-hook #'evil-qs-start nil t)

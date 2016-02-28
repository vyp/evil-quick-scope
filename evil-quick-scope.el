;;; evil-quick-scope.el --- Lightning fast f/F/t/T movement for Evil mode.

;; Copyright (C) 2016 by xd1le

;; Author: xd1le
;; Maintainer: xd1le
;; Version: 0.1.0
;; Created: 15th August 2015
;; Keywords: highlight, character, evil, convenience
;; URL: https://github.com/vyp/evil-quick-scope
;; Package-Requires: ((evil "1.2.2") (ov "1.0.6"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program as a file named `copying` in the root of this repository. If
;; not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode that attempts to emulate Brian Le's [1] quick-scope vim plugin [2]
;; for Evil mode [3].
;;
;; [1]: https://plus.google.com/102336503306134343850
;; [2]: https://github.com/unblevable/quick-scope
;; [3]: https://bitbucket.org/lyro/evil

;;; Introduction:

;; Find yourself repeatedly but cautiously pressing `;`/`,` often to get to
;; where you want? Quick scope for Evil mode highlights the characters you can
;; reach via `f`/`F` with two or three keystrokes, at most once per word.
;;
;; It does not provide any commands or keybindings, it simply augments the only
;; drawback of the `f`/`F`/`t`/`T` motions: it is difficult and slow to
;; consistently choose the best characters to target.
;;
;; Quick scope does the counting for you and presents you with the optimal
;; choices, resulting in faster and *more accurate* "character motions", as
;; Brian likes to call them.
;;
;; To avoid too much visual clutter, quick scope ignores "special" characters
;; (i.e. punctuation and similar) that tend to only appear once or twice per
;; line, and are therefore usually easy to get to.
;;
;; Quick scope attempts to highlight closest to the beginning of each word if
;; possible.
;;
;; A "word" separator is defined as any of the following characters:
;;
;; - Whitespace.
;; - Hyphen ("-").
;; - Underscore ("_").
;; - Forward slash ("/").
;; - Period (".").

;;; Configuration:

;; Enabling Minor Mode At Startup:
;; ===============================
;;
;; Because quick scope is a minor mode, you can just run the
;; `evil-quick-scope-mode` command to toggle it on or off for the current
;; buffer. Pass `t` to unconditionally turn on, or pass `-1` to unconditionally
;; turn off.
;;
;; Therefore, for example, to turn on quick scope for all text and programming
;; modes, simply add the appropriate hooks (in your `.emacs` or `init.el`):
;;
;;     (require 'evil-quick-scope)
;;
;;     (add-hook 'text-mode-hook 'evil-quick-scope-mode)
;;     (add-hook 'prog-mode-hook 'evil-quick-scope-mode)
;;
;; But now say you do not want it enabled in a particular text mode, say LaTeX
;; mode, you can disable it specifically for LaTeX mode by passing `-1` as said
;; above:
;;
;;     (add-hook 'latex-mode-hook (lambda () (evil-quick-scope-mode -1)))
;;
;; If you find yourself wanting it enabled nearly everywhere except for a few
;; modes, it may be easier to take a 'blacklisting' approach by defining and
;; enabling a globalized minor mode:
;;
;;     (defun turn-on-evil-quick-scope-mode ()
;;       "Unconditionally turn on evil-quick-scope-mode."
;;       (evil-quick-scope-mode t))
;;
;;     (define-globalized-minor-mode my-global-evil-quick-scope-mode
;;       evil-quick-scope-mode turn-on-evil-quick-scope-mode)
;;
;;     (my-global-evil-quick-scope-mode t)
;;
;; Then blacklist with each mode's hook that you want disabled using the example
;; shown above with LaTeX mode.
;;
;; Only Highlight After Keypress:
;; ==============================
;;
;; If you find keeping the highlighting always on too annoying/intrusive, it may
;; be a better option to only enable highlighting after pressing the
;; `f`/`F`/`t`/`T` keys:
;;
;;     (setq-default evil-qs-highlight-on-keypress '("f" "F" "t" "T"))
;;
;; By default this option is `nil`, hence why highlighting is always on by
;; default.
;;
;; It takes a list of four key sequences [4]. The list items correspond to the
;; following functionality respectively:
;;
;; 1. `evil-find-char`
;; 2. `evil-find-char-backward`
;; 3. `evil-find-char-to`
;; 4. `evil-find-char-to-backward`
;;
;; These key sequences are then used to make mappings for the normal, visual,
;; operator and motion evil states.
;;
;; [4]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Sequences.html
;;
;; Appearance:
;; ===========
;;
;; There are four faces that can be customised to determine the appearance of
;; the highlighting:
;;
;; 1. `evil-qs-forward-primary`
;; 2. `evil-qs-forward-secondary`
;; 3. `evil-qs-backward-primary`
;; 4. `evil-qs-backward-secondary`
;;
;; Should be self-explanatory with the following:
;;
;; - "**Primary**" refers to characters that can be reached in **two**
;;   keystrokes just by pressing `f`/`F` and then the character.
;;
;;     - By default these faces are given the "green" foreground color.
;;
;; - "**Secondary**" refers to characters that require additionally pressing a
;;   semicolon (`;`) to be reached, hence requiring **three** keystrokes.
;;
;;     - By default these faces are given the "blue" foreground color.
;;
;; - "Forward" refers to characters highlighted *after* the cursor,
;;   i.e. navigated via `f`.
;;
;; - "Backward" refers to characters highlighted *before* the cursor, i.e.
;;   navigated via `F`.

;;; Code:

;; TODO: Abort highlighting if before-cursor/after-cursor is too short.
;; TODO: Instead of just checking for the number of words, check for the number
;; of words *with accepted characters in them*, to determine when to start
;; highlighting on the line.

(require 'ov)
(require 'evil)

(defvar evil-qs-last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'evil-qs-last-post-command-position)

(defgroup evil-quick-scope nil
  "Customise evil-quick-scope."
  :prefix "evil-qs"
  :group 'Text)

(defcustom evil-qs-highlight-on-keypress nil
  "If not-nil, defines a list of key sequences to active highglighting.
First item is for `evil-find-char`.
Second item if for `evil-find-char-backward`.
Third item if for `evil-find-char-to`.
Fourth item if for `evil-find-char-to-backward`."
  :group 'evil-quick-scope)

(make-variable-buffer-local 'evil-qs-highlight-on-keypress)

(defgroup evil-quick-scope-faces nil
  "Customise evil-quick-scope faces."
  :prefix "evil-qs"
  :group 'evil-quick-scope)

(defface evil-qs-forward-primary
  '((t (:foreground "green")))
  "Highlights primary matches after the cursor."
  :group 'evil-quick-scope-faces)

(defface evil-qs-forward-secondary
  '((t (:foreground "blue")))
  "Highlights secondary matches after the cursor."
  :group 'evil-quick-scope-faces)

(defface evil-qs-backward-primary
  '((t (:foreground "green")))
  "Highlights primary matches before the cursor."
  :group 'evil-quick-scope-faces)

(defface evil-qs-backward-secondary
  '((t (:foreground "blue")))
  "Highlights secondary matches before the cursor."
  :group 'evil-quick-scope-faces)

(defun evil-qs-highlight-forward-primary (positions)
  (dolist (pos positions)
    (ov pos (+ 1 pos) 'face 'evil-qs-forward-primary 'evil-qs-fwd-pri t)))

(defun evil-qs-highlight-forward-secondary (positions)
  (dolist (pos positions)
    (ov pos (+ 1 pos) 'face 'evil-qs-forward-secondary 'evil-qs-fwd-sec t)))

(defun evil-qs-highlight-backward-primary (positions)
  (dolist (pos positions)
    (ov pos (+ 1 pos) 'face 'evil-qs-backward-primary 'evil-qs-bwd-pri t)))

(defun evil-qs-highlight-backward-secondary (positions)
  (dolist (pos positions)
    (ov pos (+ 1 pos) 'face 'evil-qs-backward-secondary 'evil-qs-bwd-sec t)))

(defun evil-qs-highlight (&optional direction)
  (unless (< (length (split-string (thing-at-point 'line) "[-_\/. \f\t\n\r\v]+")) 3)
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
          (current-char (following-char))
          (current-char-accepted-p t)

          (current-line (thing-at-point 'line))
          (beg (point-at-bol))
          (end (point-at-eol))
          (cursor)

          (before-cursor-string "")
          (after-cursor-string "")
          (after-cursor)
          (before-cursor)

          (word-seps-reg "[-_\/. \f\t\n\r\v]"))
          ;; (word-seps-reg-greedy "[-_\/. \f\t\n\r\v]+"))

      (setq occurrences accepted-chars
            cursor (- (point) beg)

            before-cursor-string (substring current-line 0 cursor)
            after-cursor-string (substring current-line cursor (- end beg)))

      (unless (eq current-char 0)
        (setq current-char-accepted-p
              (cdr (assoc (string current-char) occurrences))))

      (unless (equal direction "backward")
        (let ((pos cursor)
              (accepted)

              (first-word t)
              (first-char (if current-char-accepted-p t nil))

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

              (if (eq first-char t)
                  (setq first-char nil))

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

        ;; Reset occurrences in case we need to highlight backwards too.
        (setq occurrences accepted-chars))

      (unless (equal direction "forward")
        (let ((pos cursor)
              (accepted)

              (first-word (if current-char-accepted-p t nil))
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
          (evil-qs-highlight-backward-secondary sec-chars-to-hl))))))

(defun evil-qs-clear ()
  (ov-clear 'evil-qs-fwd-pri)
  (ov-clear 'evil-qs-fwd-sec)
  (ov-clear 'evil-qs-bwd-pri)
  (ov-clear 'evil-qs-bwd-sec))

(defun evil-qs-refresh ()
  (evil-qs-clear)
  (evil-qs-highlight))

(defun evil-qs-refresh-if-moved-post-command ()
  (ignore-errors
    (unless (eq (point) evil-qs-last-post-command-position)
      (evil-qs-refresh))

    (setq evil-qs-last-post-command-position (point))))

(defun evil-qs-start ()
  (add-hook 'post-command-hook #'evil-qs-refresh-if-moved-post-command nil t))

(defun evil-qs-stop ()
  (evil-qs-clear)
  (remove-hook 'post-command-hook #'evil-qs-refresh-if-moved-post-command t))

(evil-define-motion evil-qs-find-char (count)
  :type inclusive
  (evil-qs-highlight "forward")
  (unwind-protect
      (call-interactively 'evil-find-char)
    (evil-qs-clear)))

(evil-define-motion evil-qs-find-char-backward (count)
  :type inclusive
  (evil-qs-highlight "backward")
  (unwind-protect
      (call-interactively 'evil-find-char-backward)
    (evil-qs-clear)))

(evil-define-motion evil-qs-find-char-to (count)
  :type inclusive
  (evil-qs-highlight "forward")
  (unwind-protect
      (call-interactively 'evil-find-char-to)
    (evil-qs-clear)))

(evil-define-motion evil-qs-find-char-to-backward (count)
  :type inclusive
  (evil-qs-highlight "backward")
  (unwind-protect
      (call-interactively 'evil-find-char-to-backward)
    (evil-qs-clear)))

;;;###autoload
(define-minor-mode evil-quick-scope-mode
  "Toggle evil-quick-scope-mode on or off."

  nil " qsc" (make-sparse-keymap)

  (if evil-quick-scope-mode
      (progn
        (if (not (eq evil-qs-highlight-on-keypress nil))
            (progn
              (let ((evil-qs-hok-fwd    (nth 0 evil-qs-highlight-on-keypress))
                    (evil-qs-hok-bwd    (nth 1 evil-qs-highlight-on-keypress))
                    (evil-qs-hok-fwd-to (nth 2 evil-qs-highlight-on-keypress))
                    (evil-qs-hok-bwd-to (nth 3 evil-qs-highlight-on-keypress)))

                (evil-define-key 'normal evil-quick-scope-mode-map
                  (kbd evil-qs-hok-fwd)    'evil-qs-find-char
                  (kbd evil-qs-hok-bwd)    'evil-qs-find-char-backward
                  (kbd evil-qs-hok-fwd-to) 'evil-qs-find-char-to
                  (kbd evil-qs-hok-bwd-to) 'evil-qs-find-char-to-backward)

                (evil-define-key 'visual evil-quick-scope-mode-map
                  (kbd evil-qs-hok-fwd)    'evil-qs-find-char
                  (kbd evil-qs-hok-bwd)    'evil-qs-find-char-backward
                  (kbd evil-qs-hok-fwd-to) 'evil-qs-find-char-to
                  (kbd evil-qs-hok-bwd-to) 'evil-qs-find-char-to-backward)

                (evil-define-key 'operator evil-quick-scope-mode-map
                  (kbd evil-qs-hok-fwd)    'evil-qs-find-char
                  (kbd evil-qs-hok-bwd)    'evil-qs-find-char-backward
                  (kbd evil-qs-hok-fwd-to) 'evil-qs-find-char-to
                  (kbd evil-qs-hok-bwd-to) 'evil-qs-find-char-to-backward)

                (evil-define-key 'motion evil-quick-scope-mode-map
                  (kbd evil-qs-hok-fwd)    'evil-qs-find-char
                  (kbd evil-qs-hok-bwd)    'evil-qs-find-char-backward
                  (kbd evil-qs-hok-fwd-to) 'evil-qs-find-char-to
                  (kbd evil-qs-hok-bwd-to) 'evil-qs-find-char-to-backward)))

          (let ((orig-state evil-state))
            (unless (or (eq orig-state 'insert)
                        (eq orig-state 'replace)
                        (eq orig-state 'emacs))

              (evil-qs-start)))

          (add-hook 'evil-insert-state-entry-hook #'evil-qs-stop nil t)
          ;; In case cursor does not actually change position.
          ;;
          ;; For example, if user enters insert state at the beginning of a
          ;; line, and exits it at the same place, the cursor does not move one
          ;; position back as usual (even if `evil-cross-lines` is set to true).
          (add-hook 'evil-insert-state-exit-hook #'evil-qs-highlight nil t)
          (add-hook 'evil-insert-state-exit-hook #'evil-qs-start nil t)

          (add-hook 'evil-replace-state-entry-hook #'evil-qs-stop nil t)
          (add-hook 'evil-replace-state-exit-hook #'evil-qs-highlight nil t)
          (add-hook 'evil-replace-state-exit-hook #'evil-qs-start nil t)

          (add-hook 'evil-emacs-state-entry-hook #'evil-qs-stop nil t)
          (add-hook 'evil-emacs-state-exit-hook #'evil-qs-highlight nil t)
          (add-hook 'evil-emacs-state-exit-hook #'evil-qs-start nil t)

          (add-hook 'minibuffer-setup-hook (lambda () (evil-quick-scope-mode -1)))))

    (remove-hook 'evil-insert-state-entry-hook #'evil-qs-stop t)
    (remove-hook 'evil-insert-state-exit-hook #'evil-qs-highlight t)
    (remove-hook 'evil-insert-state-exit-hook #'evil-qs-start t)

    (remove-hook 'evil-replace-state-entry-hook #'evil-qs-stop t)
    (remove-hook 'evil-replace-state-exit-hook #'evil-qs-highlight t)
    (remove-hook 'evil-replace-state-exit-hook #'evil-qs-start t)

    (remove-hook 'evil-emacs-state-entry-hook #'evil-qs-stop t)
    (remove-hook 'evil-emacs-state-exit-hook #'evil-qs-highlight t)
    (remove-hook 'evil-emacs-state-exit-hook #'evil-qs-start t)

    (remove-hook 'minibuffer-setup-hook (lambda () (evil-quick-scope-mode -1)))

    (evil-qs-stop)))

(provide 'evil-quick-scope)

;;; evil-quick-scope.el ends here.

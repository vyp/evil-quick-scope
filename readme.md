# Evil Quick Scope

Emacs minor mode that attempts to emulate [Brian Le's] [1] [quick-scope] [2] vim
plugin for [Evil mode] [3]. All credit for this idea goes to him.

## Introduction

Find yourself repeatedly but cautiously pressing `;`/`,` often to get to where
you want? Quick scope for Evil mode highlights the characters you can reach via
`f`/`F` with two or three keystrokes, at most once per word.

It does not provide any commands or keybindings, it simply augments the only
drawback of the `f`/`F`/`t`/`T` motions: it is difficult and slow to
consistently choose the best characters to target.

Quick scope does the counting for you and presents you with the optimal choices,
resulting in faster and *more accurate* "character motions", as Brian likes to
call them.

---

- To avoid too much visual clutter, quick scope ignores "special" characters
  (i.e. punctuation and similar) that tend to only appear once or twice per
  line, and are therefore usually easy to get to.

- Quick scope attempts to highlight closest to the beginning of each word if
  possible.

- A "word" separator is defined as any of the following characters:

  - Whitespace.
  - Hyphen ("-").
  - Underscore ("_").
  - Forward slash ("/").
  - Period (".").

## Installation

### [Quelpa] [4] (recommended)

``` elisp
(quelpa
 '(evil-quick-scope
   :fetcher github
   :repo "vyp/evil-quick-scope"))
```

### [El-Get] [5]

``` elisp
(el-get-bundle vyp/evil-quick-scope)
```

### Manually

Make sure that the dependencies evil and [ov] [6] are installed, and then in
`.emacs` or `init.el`:

``` elisp
(add-to-list 'load-path "~/path/to/evil-quick-scope/")
(load "evil-quick-scope")
```

## Configuration

### Enabling Minor Mode At Startup

Because quick scope is a minor mode, you can just run the
`evil-quick-scope-mode` command to toggle it on or off for the current
buffer. Pass `t` to unconditionally turn on, or pass `nil` to unconditionally
turn off.

Therefore, for example, to turn on quick scope for all text and programming
modes, simply add the appropriate hooks (in your `.emacs` or `init.el`):

``` elisp
(require 'evil-quick-scope)

(add-hook 'text-mode-hook 'evil-quick-scope-mode)
(add-hook 'prog-mode-hook 'evil-quick-scope-mode)
```

But now say you do not want it enabled in a particular text mode, say LaTeX
mode, you can disable it specifically for LaTeX mode by passing `nil` as said
above:

``` elisp
(add-hook 'latex-mode-hook (lambda () (evil-quick-scope-mode nil)))
```

---

If you find yourself wanting it enabled nearly everywhere except for a few
modes, it may be easier to take a 'blacklisting' approach by defining and
enabling a globalized minor mode:

``` elisp
(defun turn-on-evil-quick-scope-mode ()
  "Unconditionally turn on evil-quick-scope-mode."
  (evil-quick-scope-mode t))

(define-globalized-minor-mode my-global-evil-quick-scope-mode
  evil-quick-scope-mode turn-on-evil-quick-scope-mode)

(my-global-evil-quick-scope-mode t)
```

Then blacklist with each mode's hook that you want disabled using the example
shown above with LaTeX mode.

### Appearance

There are four faces that can be customised to determine the appearance of the
highlighting:

1. `evil-qs-forward-primary`
2. `evil-qs-forward-secondary`
3. `evil-qs-backward-primary`
4. `evil-qs-backward-secondary`

Should be self-explanatory with the following:

- "**Primary**" refers to characters that can be reached in **two** keystrokes
  just by pressing `f`/`F` and then the character.

  - By default these faces are given the "green" foreground color.

- "**Secondary**" refers to characters that require additionally pressing a
  semicolon (`;`) to be reached, hence requiring **three** keystrokes.

  - By default these faces are given the "blue" foreground color.

- "Forward" refers to characters highlighted *after* the cursor, i.e. navigated
  via `f`.

- "Backward" refers to characters highlighted *before* the cursor, i.e.
  navigated via `F`.

## Roadmap

### Planned

- Option to only turn on highlighting after pressing `f`/`F`/`t`/`T`/`;`/`,`
  (and to turn off again with the next non-`f`/`F`/`t`/`T`/`;`/`,` movement).

  - If this option is selected, pressing `;`/`,` should only highlight the
    character that is currently being used by these commands.

- Option to specify an alist to highlight faces at highlighting points according
  to the particular face at that point (instead of using the
  `evil-qs-*dir*-*order*` faces).

- Highlight all visible lines if `evil-cross-lines` option is true.

### Maybe

- Option to disable highlighting long lines.

- Option to select word separators?

- Turn off in inactive buffers if possible?

## License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program in a file named `license` at the root directory. If not, see
<http://www.gnu.org/licenses/>.

[1]: https://plus.google.com/102336503306134343850
[2]: https://github.com/unblevable/quick-scope
[3]: https://bitbucket.org/lyro/evil
[4]: https://github.com/quelpa/quelpa
[5]: https://github.com/dimitri/el-get
[6]: https://github.com/ShingoFukuyama/ov.el

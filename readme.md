I've just found [another quick scope package] [1] for evil mode. It was created
before this package, and it's much better written, and also has an extensive
test suite, so you should definitely try that one out first.

I wrote this because I wasn't aware of any others at the time I wrote it. It's
possible blorbx's package was not public yet when I was searching if one already
existed.

For the time being, I will continue using and working on this package because
there's a few features that are not in blorbx's version that I want. However,
when/if these things come to blorbx's quick scope, I see no reason to continue
this package and will deprecate it. So be cautious if you use this package, and
keep checking back periodically, in case it has been deprecated.

With regards to the function prefixes, blorbx's code seems to use
`evil-quickscope` as a prefix throughout, whereas I use `evil-qs`, except I also
call the mode `evil-quick-scope-mode` and the group for the faces
`evil-quick-scope`. So there shouldn't be conflicts, but you should only be
using one or the other anyway.

---

# Evil Quick Scope

Emacs minor mode that attempts to emulate [Brian Le's] [2] [quick-scope] [3] vim
plugin for [Evil mode] [4]. All credit for this idea goes to him.

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

Using [Quelpa] [5]:

``` elisp
(quelpa
 '(evil-quick-scope
   :fetcher gitlab
   :repo "fi/evil-quick-scope"))
```

## Configuration

### Enabling Minor Mode At Startup

Because quick scope is a minor mode, you can just run the
`evil-quick-scope-mode` command to toggle it on or off for the current
buffer. Pass `t` to unconditionally turn on, or pass `-1` to unconditionally
turn off.

Therefore, for example, to turn on quick scope for all text and programming
modes, simply add the appropriate hooks (in your `.emacs` or `init.el`):

``` elisp
(require 'evil-quick-scope)

(add-hook 'text-mode-hook 'evil-quick-scope-mode)
(add-hook 'prog-mode-hook 'evil-quick-scope-mode)
```

But now say you do not want it enabled in a particular text mode, say LaTeX
mode, you can disable it specifically for LaTeX mode by passing `-1` as said
above:

``` elisp
(add-hook 'latex-mode-hook (lambda () (evil-quick-scope-mode -1)))
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

### Only Highlight After Keypress

If you find keeping the highlighting always on too annoying/intrusive, it may be
a better option to only enable highlighting after pressing the `f`/`F`/`t`/`T`
keys:

``` elisp
(setq-default evil-qs-highlight-on-keypress '("f" "F" "t" "T"))
```

By default this option is `nil`, hence why highlighting is always on by
default.

It takes a list of four [key sequences] [6]. The list items correspond to the
following functionality respectively:

1. `evil-find-char`
2. `evil-find-char-backward`
3. `evil-find-char-to`
4. `evil-find-char-to-backward`

These key sequences are then used to make mappings for the normal, visual,
operator and motion evil states.

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
this program as a file named `license` in the root of this repository. If not,
see <http://www.gnu.org/licenses/>.

[1]: https://github.com/blorbx/evil-quickscope
[2]: https://plus.google.com/102336503306134343850
[3]: https://github.com/unblevable/quick-scope
[4]: https://bitbucket.org/lyro/evil
[5]: https://github.com/quelpa/quelpa
[6]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Sequences.html

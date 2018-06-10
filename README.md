# latex-unicode-math-mode

[![Build Status](https://travis-ci.org/Christoph-D/latex-unicode-math-mode.svg?branch=master)](https://travis-ci.org/Christoph-D/latex-unicode-math-mode)
[![MELPA](https://melpa.org/packages/latex-unicode-math-mode-badge.svg)](https://melpa.org/\#/latex-unicode-math-mode)
[![MELPA Stable](https://stable.melpa.org/packages/latex-unicode-math-mode-badge.svg)](https://stable.melpa.org/#/latex-unicode-math-mode)

An Emacs minor mode for entering Unicode math symbols in LaTeX-mode,
with a sty file to make pdflatex Unicode-aware.  This minor mode
automatically replaces inputs like `\phi` with `ϕ` and `\in` with `∈`.
These replacements happen inside of math environments or everywhere,
depending on the configuration.

If you use pdflatex, you can add `\usepackage{unicode-math-mode}` to
your tex file in order to make pdflatex aware of the Unicode
characters.  Use `M-x latex-unicode-save-sty-file` to put this file
somewhere where pdflatex will find it.

If you update this package, you may need to do this again to get the
latest sty file.

You can customize the keybindings and symbols with `M-x
customize-group latex-unicode-math`.

## Installation

### From MELPA

The recommended way of installing is from [MELPA](http://melpa.org/).
Call `M-x list-packages` and look for `latex-unicode-math-mode`.
After installing, add this to your `~/.emacs` file to enable the mode
automatically for all LaTeX files:

```elisp
(require 'latex-unicode-math-mode)
;; Enable latex-unicode-math-mode automatically for all LaTeX files.
;; This converts LaTeX to Unicode inside math environments.
(add-hook 'LaTeX-mode-hook 'latex-unicode-math-mode)

;; Enable latex-unicode-mode automatically for all LaTeX files.
;; This converts LaTeX to Unicode everwhere, not only in math
;; environments.
;;(add-hook 'LaTeX-mode-hook 'latex-unicode-mode)
```

Note that `latex-unicode-math-mode` and `latex-unicode-mode` are
mutually exclusive.  Enabling one will automatically disable the
other.

While `latex-unicode-math-mode` really makes sense only with AUCTeX,
`latex-unicode-mode` also works fine in other major modes, for example
in `org-mode`.

Tested with pdflatex from TeX Live 2016 and GNU Emacs 25.1.1.

## Example

Say you type this:

```latex
\pi(v_1,\ldots,v_n) \in A \cup B
```

As you type, `latex-unicode-math-mode` turns it into this:

```latex
π(v_1,…,v_n) ∈ A ∪ B
```

There will be no change in the compiled pdf (don't forget to add
`\usepackage{unicode-math-mode}` in the preamble).

If you later decide that you do do not like Unicode symbols after all,
you can easily revert everything back to LaTeX macros, see the next
section.

## Additional Features

`M-x latex-unicode-convert-region` converts all LaTeX macros in the
active region to their Unicode equivalents.

`M-x latex-unicode-convert-buffer` is the same, but applies to the
whole buffer.

`M-x latex-unicode-invert-region` and `M-x
latex-unicode-invert-buffer` revert all Unicode symbols in the current
region/buffer back to their LaTeX equivalents.

## Comparison to the `TeX` input method

Emacs comes with an input method called `TeX`, enabled via `M-x
set-input-method TeX` (see
https://github.com/emacs-mirror/emacs/blob/master/lisp/leim/quail/latin-ltx.el).
This input method is very similar to `latex-unicode-math-mode`.  Some
pros and contras are as follows.

`latex-unicode-math-mode`:

- Pro:
  - Only active in math environments.
  - Has a .sty file to make pdflatex understand the Unicode symbols.
  - Has shorthand notation for common symbols such as `!=` for `\neq`
    and `==​>` for `\Longrightarrow`.
  - Can convert/invert regions and buffers.
  - Customizable (customization group `latex-unicode-math`).
- Contra:
  - Very new and likely to change.  Expect rough edges and bugs.

The `TeX` input method:

- Pro:
  - Covers subscripts and superscripts.
  - Has auto-completion (thanks to quail).
- Contra:
  - All or nothing: Does not automatically deactivate itself outside
    of math environments.
  - Does not work with pdflatex: I do not know how to handle Unicode
    superscripts in pdflatex combined with the `'` (prime) character,
    which is active in math mode.

## Bugs

Due to the implementation, this minor mode will probably break your
input method.  If you write in English or in another European
language, chances are you are not using one.

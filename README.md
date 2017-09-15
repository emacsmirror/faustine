# Faustine
 *Edit, visualize, build and run Faust code*
___
[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html) [![Codeship Status for yassinphilip/faustine](https://app.codeship.com/projects/c2385cd0-5dc6-0135-04b2-0a800465306c/status?branch=master)](https://app.codeship.com/projects/238325) [![MELPA](https://melpa.org/packages/faustine-badge.svg)](https://melpa.org/#/faustine) [![MELPA](https://stable.melpa.org/packages/faustine-badge.svg)](https://stable.melpa.org/#/faustine) [![Gratipay](http://img.shields.io/gratipay/yassinphilip.svg)](https://www.gratipay.com/yassinphilip/)


Faustine allows the edition of Faust (http://faust.grame.fr) code. 

## Features

- Faust code syntax hightlighting and indentation
- Project-based (inter-linked Faust files)
- Build/compile with output window
- Graphic diagrams generation and vizualisation in the (default) browser
- Browse generated C++ code inside Emacs
- Inter-linked files/buffers :
    - From "component" to Faust file
    - From "include" to Faust library file
- From error to file:line number
- From function name to online documentation
- Fully configurable (build type/target/architecture/toolkit, keyboard shortcuts, etc.)
- Automatic keyword completion (if [Auto-Complete](https://github.com/auto-complete/auto-complete) is installed)
- Automatic objets (functions, operators, etc.) template insertion with default sensible values (if [Yasnippet](https://github.com/joaotavora/yasnippet) is installed)
- Modeline indicator of the state of the code

## Installation

### Easy

- Install it from [MELPA](https://melpa.org).

### Hard

- Copy/clone this repository in `load-path`
- Copy/clone [Faust-mode](https://github.com/magnetophon/emacs-faust-mode) in `load-path`
- Add
```elisp
(require 'faust-mode)
(require 'faustine)
```
to your init file

### Faust
Oh, and of course install [the latest Faust](http://faust.grame.fr/download/) and ensure it's in the PATH.

### Recommended packages

Those package are not required, but Faustine makes good use of them, and they will make your life better anyway ; They are all available in MELPA, snapshot and stable.

- [Projectile](https://github.com/bbatsov/projectile)
- [AutoComplete](https://github.com/auto-complete/auto-complete)
- [Yasnippet](https://github.com/joaotavora/yasnippet)

## Usage

### Enter the mode

Use `faustine-mode` ; Optionally, add something like this to your init file:
```elisp
(add-to-list 'auto-mode-alist
             '("\\.dsp\\'" . faustine-mode))
```
to put any new Faust file in the mode.

### Commands
Every interactive command is documented in [the README](https://bitbucket.org/yassinphilip/faustine/src/master/README.md) file.

## Major modes

Faustine introduces 2 major modes, detailed below.

### faustine-mode
A mode to allow the edition of Faust code.

Syntax highlighting of all the Faust commands and operators, as
well as indentation rules, using [faust-mode](https://melpa.org/#/faust-mode).

Every referenced ("component") file is linked, and can be
opened by clicking on it or by pressing `RET` over it ; Imported
library files are linked too.

The code is checked at each save ; The state of the last check is
displayed in the modeline as a green bug icon when it compiles
without error or warning, and a red bug when it doesn't. This
icon is also the main Faustine menu.

An "output buffer" is provided to display information about the
Faust command output, you can toggle its visibility with
`faustine-toggle-output-buffer` ; see `faustine-output-mode`
documentation for details about interaction in said buffer.

Several commands allow the editing of Faust code, they are all
available in the menu or as a key binding, and described below.

#### faustine-mode Keys

Key  | Binding 
------------- | ------------- 
`C-c C-b` | [faustine-build](#markdown-header-faustine-build)
`C-c C-c` | [faustine-syntax-check](#markdown-header-faustine-syntax-check)
`C-c C-d` | [faustine-diagram](#markdown-header-faustine-diagram)
`C-c C-h` | [faustine-online-doc](#markdown-header-faustine-online-doc)
`C-c RET` | [faustine-mdoc](#markdown-header-faustine-mdoc)
`C-c C-o` | [faustine-toggle-output-buffer](#markdown-header-faustine-toggle-output-buffer)
`C-c C-s` | [faustine-source-code](#markdown-header-faustine-source-code)
`C-c r` | [faustine-run](#markdown-header-faustine-run)
`C-c C-S-b` | [faustine-build-all](#markdown-header-faustine-build-all)
`C-c C-S-d` | [faustine-diagram-all](#markdown-header-faustine-diagram-all)
`C-M-q` | [prog-indent-sexp](#markdown-header-prog-indent-sexp)

#### faustine-mode Commands

##### faustine-build

```elisp
(faustine-build &optional BUILD-ALL)
```

Build the current buffer/file executable(s).
If BUILD-ALL is set, build all Faust files referenced by this one.

##### faustine-syntax-check

```elisp
(faustine-syntax-check)
```

Check if Faust code buffer compiles.
Run at load and save time.

##### faustine-diagram

```elisp
(faustine-diagram &optional BUILD-ALL)
```

Generate the Faust diagram of the current file.
If BUILD-ALL is set, build all Faust files referenced by this one.

##### faustine-online-doc

```elisp
(faustine-online-doc START END)
```

View online documentation for the selected (or under point)
string on faust.grame.fr.
Build a button/link from START to END.

##### faustine-mdoc

```elisp
(faustine-mdoc &optional BUILD-ALL)
```

Generate Mdoc of the current file, display it in a buffer.
If BUILD-ALL is set, build all Faust files referenced by this one.

##### faustine-toggle-output-buffer

```elisp
(faustine-toggle-output-buffer)
```

Show/hide the Faust output buffer.

##### faustine-source-code

```elisp
(faustine-source-code)
```

Generate the Faust C++ code of the current faust file and
display it in a buffer.

##### faustine-run

```elisp
(faustine-run &optional BUTTON)
```

Run the executable generated by the current Faust code buffer
or passed by from the output buffer BUTTON click.

##### faustine-build-all

```elisp
(faustine-build-all)
```

Build executable of all the files referenced by his one
using the `faustine-build-backend`.

##### faustine-diagram-all

```elisp
(faustine-diagram-all)
```

Build diagram of all the files referenced by his one.
Construct a minimal HTML page and display it in the default browser.

##### prog-indent-sexp
> Defined in `prog-mode.el`


```elisp
(prog-indent-sexp &optional DEFUN)
```

Indent the expression after point.
When interactively called with prefix, indent the enclosing defun
instead.


___
### faustine-output-mode
The Faust output buffer mode. 
The output buffer displays the result of the commands with their time stamps and status. 

- A click on an error opens the buffer at the error line
- A click on an executable name runs it.

#### faustine-output-mode Keys

Key  | Binding 
------------- | ------------- 
`q` | [delete-window](#markdown-header-delete-window)

#### faustine-output-mode Commands

##### delete-window
> Defined in `window.el`


It is bound to C-x 0.

```elisp
(delete-window &optional WINDOW)
```

Delete WINDOW.
WINDOW must be a valid window and defaults to the selected one.
Return nil.

If the variable `ignore-window-parameters` is non-nil or the
`delete-window` parameter of WINDOW equals t, do not process any
parameters of WINDOW.  Otherwise, if the `delete-window`
parameter of WINDOW specifies a function, call that function with
WINDOW as its sole argument and return the value returned by that
function.

Otherwise, if WINDOW is part of an atomic window, call
`delete-window` with the root of the atomic window as its
argument.  Signal an error if WINDOW is either the only window on
its frame, the last non-side window, or part of an atomic window
that is its frame's root window.


___
*README.md made on 2017-09-08 at 14:57:54 with [doc-a-mode](https://bitbucket.org/yassinphilip/doc-a-mode)*
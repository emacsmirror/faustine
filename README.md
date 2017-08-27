# Faustine
 *Edit, visualize, build and run Faust code*
___
[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html) [![Codeship Status for yassinphilip/faustine](https://app.codeship.com/projects/c2385cd0-5dc6-0135-04b2-0a800465306c/status?branch=master)](https://app.codeship.com/projects/238325)


Faustine allows the edition of Faust (http://faust.grame.fr) code. 

## Features

- Project-based (inter-linked Faust files)
- Faust code syntax hightlighting, indentation and keyword completion
- Build/compile with configurable output window
- Graphic diagrams generation and vizualisation in the browser
- Browse generated C++ code inside Emacs
- Inter-linked files/buffers :
    - From "component" to Faust file
    - From "include" to library file
- From error to file, direct to line number
- From function name to online documentation
- Fully configurable (build type/target/architecture/toolkit, keyboard shortcuts, etc.)
- Automatic keyword completion
- Modeline indicator of the state of the code

## faust-mode
A mode to allow the edition of Faust (http://faust.grame.fr) code.

Use `faustine-configure` (M-x faustine-configure) to set it up.
Available commands while editing Faust files:


### faust-mode Keys

Key binding  | Commmand 
------------- | ------------- 
`C-c C-b` | faustine-build
`C-c C-c` | faustine-syntax-check
`C-c C-d` | faustine-diagram
`C-c C-h` | faustine-online-doc
`C-c RET` | faustine-mdoc
`C-c C-o` | faustine-toggle-output-buffer
`C-c C-p` | faustine-configure
`C-c C-s` | faustine-source-code
`C-c r` | faustine-run
`C-c C-S-b` | faustine-build-all
`C-c C-S-d` | faustine-diagram-all
`C-M-q` | prog-indent-sexp

### faust-mode Commands
#### faustine-build

(faustine-build &optional BUILD-ALL)

Build the current buffer/file executable(s).
If BUILD-ALL is set, build all Faust files referenced by this one.

#### faustine-syntax-check

(faustine-syntax-check)

Check if Faust code buffer compiles.
Run at load and save time.

#### faustine-diagram

(faustine-diagram &optional BUILD-ALL)

Generate the Faust diagram of the current file.
If BUILD-ALL is set, build all Faust files referenced by this one.

#### faustine-online-doc

(faustine-online-doc START END)

View online documentation for the selected (or under point)
string on faust.grame.fr.
Build a button/link from START to END.

#### faustine-mdoc

(faustine-mdoc &optional BUILD-ALL)

Generate Mdoc of the current file, display it in a buffer.
If BUILD-ALL is set, build all Faust files referenced by this one.

#### faustine-toggle-output-buffer

(faustine-toggle-output-buffer)

Show/hide the Faust output buffer.

#### faustine-configure

(faustine-configure)

Set up Faustine preferences using `cutomize-group`.

#### faustine-source-code

(faustine-source-code)

Generate the Faust C++ code of the current faust file and
display it in a buffer.

#### faustine-run

(faustine-run &optional BUTTON)

Run the executable generated by the current Faust code buffer
or passed by from the output buffer BUTTON click.

#### faustine-build-all

(faustine-build-all)

Build executable of all the files referenced by his one
using the `faustine-build-backend`.

#### faustine-diagram-all

(faustine-diagram-all)

Build diagram of all the files referenced by his one.
Construct a minimal HTML page and display it in the default browser.

#### prog-indent-sexp (from prog-mode.el)

(prog-indent-sexp &optional DEFUN)

Indent the expression after point.
When interactively called with prefix, indent the enclosing defun
instead.


## faustine-output-mode
The Faust output buffer mode. 
The output buffer displays the result of the commands with their time stamps and status. 

- A click on an error opens the buffer at the error line
- A click on an executable name runs it.

This mode runs the hook `faustine-output-mode-hook`, as the final step
during initialization.



___
*README.md Generated at 04:54:12 using [doc-a-mode](https://bitbucket.org/yassinphilip/doc-a-mode)*
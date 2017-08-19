# Faustine

Faustine allows the edition of Faust (http://faust.grame.fr) code.
Edit, visualize, build and run Faust code files.

Use `faustine-configure' (M-x faustine-configure) to set it up.
Available commands while editing Faust files:

---

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

## Keys

Key binding  | Command 
------------- | ------------- 
C-c C-c |  faustine-syntax-check
C-c C-d |  faustine-diagram
C-c C-h |  faustine-online-doc
C-c RET |  faustine-mdoc
C-c C-o |  faustine-toggle-output-buffer
C-c C-p |  faustine-configure
C-c C-s |  faustine-source-code
C-c r |  faustine-run
C-c C-S-b |  faustine-build-all
C-c C-S-d |  faustine-diagram-all


## Recommended packages

Those packages are totally optional, but if you use them, and you
should, Faustine will take advantage of it, and your experience
will be better/faster/stronger.

- [Auto-Complete](https://github.com/auto-complete/auto-complete)
- [Emacs-helm](https://github.com/emacs-helm/helm)
- [YASnippet](https://github.com/joaotavora/yasnippet) (includes [Faust snippets](https://github.com/AndreaCrotti/yasnippet-snippets/tree/885050d34737e2fb36a3e7759d60c09347bd4ce0/faust-mode))


## Interactive functions

### faustine-build 
Build the current buffer/file executable(s).
If BUILD-ALL is set, build all Faust files referenced by this one.

### faustine-syntax-check 
Check if Faust code buffer compiles.
Run at load and save time.

### faustine-diagram 
Generate the Faust diagram of the current file.
If BUILD-ALL is set, build all Faust files referenced by this one.

### faustine-online-doc 
View online documentation for the selected (or under point)
string on faust.grame.fr.
Build a button/link from START to END.

### faustine-mdoc 
Generate Mdoc of the current file, display it in a buffer.
If BUILD-ALL is set, build all Faust files referenced by this one.

### faustine-toggle-output-buffer 
Show/hide the Faust output buffer.

### faustine-configure 
Set up Faustine preferences using `cutomize-group'.

### faustine-source-code 
Generate the Faust C++ code of the current faust file and
display it in a buffer.

### faustine-run 
Run the executable generated by the current Faust code buffer
or passed by from the output buffer BUTTON click.

### faustine-build-all 
Build executable of all the files referenced by his one
using the `faustine-build-backend'.

### faustine-diagram-all 
Build diagram of all the files referenced by his one.
Construct a minimal HTML page and display it in the default browser.---
_Icon by [Mattahan](https://mattahan.deviantart.com/art/Buuf-37966044)_ 

##### Doc auto-made on 05:11:04
---

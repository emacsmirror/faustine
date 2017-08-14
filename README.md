# Faustine

![Logo](https://bytebucket.org/yassinphilip/faustine/raw/master/faustine.png)

Faustine allows the edition of Faust (http://faust.grame.fr/) code.
Edit, visualize, build and run Faust code files.

Use `faustine-configure' (M-x faustine-configure) to set it up.
Available commands while editing Faust (*.dsp) files:

---
[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html) [ ![Codeship Status for yassinphilip/faustine](https://app.codeship.com/projects/c2385cd0-5dc6-0135-04b2-0a800465306c/status?branch=master)](https://app.codeship.com/projects/238325)

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
- [YASnippet](https://github.com/joaotavora/yasnippet)


## Interactive functions

### faustine-build 

Build the current buffer/file executable(s).
If BUILD-ALL is set, build all Faust files referenced by this one.

### faustine-syntax-check 
Check if Faust code buffer compiles.
Run at load and save time.

### faustine-diagram 

Generate Faust diagram(s).
If BUILD-ALL is set, build all `faustine-faust-extension` files referenced by this one.

### faustine-online-doc 

Websearch selected (or under point) string on faust.grame.fr.
Build a button from START to END.

### faustine-mdoc 

Generate mdoc of the current file, display it in a buffer.
If BUILD-ALL is set, build all linked files.

### faustine-toggle-output-buffer 
Show/hide Faust output buffer.

### faustine-configure 

Use `cutomize-group' to set up Faustine preferences.

### faustine-source-code 

Generate Faust C++ code of the current faust file, display it in `faustine-c++-buffer-name'.

### faustine-run 

Run the executable generated by the current Faust code buffer or passed by BUTTON.

### faustine-build-all 

Build all linked executables.

### faustine-diagram-all 

Build all linked diagrams.

##### Doc auto-made on 04:38:41
---

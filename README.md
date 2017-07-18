# Emacs Faust IDE

A Faust code editor for emacs

[FAUST](http://faust.grame.fr) (Functional Audio Stream) is a functional programming language specifically designed for real-time signal processing and synthesis. FAUST targets high-performance signal processing applications and audio plug-ins for a variety of platforms and standards.

## Features
- Faust code syntax hightlighting plus indentation
- Build with resizable output window
- Graphic diagrams generation and vizualisation in the browser

## Installation

- Clone this repo in your ~/.emacs.d/lisp dir
- Ensure this directory is in the PATH :

```lisp
(let ((default-directory "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))
```
- Require the file in your `~/.emacs`:
`(require 'emacs-faust-ide nil 'noerror)`
- Open a .dsp file

### Can I help?
There are several ways you can help (by order of magnificence) :
Subscribe to [my YouTube channel](https://www.youtube.com/c/YassinPhilip-ManyRecords)
Buy my music on [Bandcamp](https://yassinphilip.bandcamp.com) (I'm told I'm on iTunes, Spotify, Google Music and shit but I never seem to sell on song)
Help me on [Patreon](http://www.patreon.com/yassinphilip)

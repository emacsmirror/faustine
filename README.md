# Faustine
## Edit, visualise, build and run Faust code

A Faust code editor for Emacs ; Not ready for production yet.

[FAUST](http://faust.grame.fr) (Functional Audio Stream) is a functional programming language specifically designed for real-time signal processing and synthesis. FAUST targets high-performance signal processing applications and audio plug-ins for a variety of platforms and standards.

## What does it do?
- Faust code syntax hightlighting plus indentation
- Build/compile with resizable output window
- Graphic diagrams generation and vizualisation in the browser
- From function name to online documentation
- Browse generated C++ code inside Emacs
- Fully configurable (build type/target, toolkit, preferences, etc.) via `customize-group emacs-faust-ide RET`
- Automatic keyword completion
- Modeline indicator of the state of the code

## How can I install it?
- Clone this repo in your ~/.emacs.d/lisp dir
- Ensure this directory is in the PATH :

```lisp
(let ((default-directory "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))
```
- Require the file in your `~/.emacs`:
`(require 'emacs-faust-ide nil 'noerror)`
- Open a .dsp file
- Use `C-h m` for help and commands

### Can I thank you?
There are several ways you can help (by order of magnificence) :
- Subscribe to [my YouTube channel](https://www.youtube.com/c/YassinPhilip-ManyRecords)
- Buy my music on [Bandcamp](https://yassinphilip.bandcamp.com) (I'm told I'm on iTunes, Spotify, Google Music and stuff but I never seem to sell a single song)
- Make a [donation](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=yassinphil%40gmail%2ecom&lc=BM&item_name=Yassin%20Philip&no_note=0&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donateCC_LG%2egif%3aNonHostedGuest)
- Help me on [Patreon](http://www.patreon.com/yassinphilip)

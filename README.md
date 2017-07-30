# Faustine
## Edit, visualise, build and run Faust code

Faustine is a [Faust](http://faust.grame.fr) major mode for [GNU Emacs](https://www.gnu.org/software/emacs/).

[Faust](http://faust.grame.fr) (Functional Audio Stream) is a functional programming language specifically designed for real-time signal processing and synthesis. FAUST targets high-performance signal processing applications and audio plug-ins for a variety of platforms and standards.

## What does it do?
- Project-based (inter-linked Faust files)
- Faust code syntax hightlighting and indentation
- Build/compile with configurable output window
- Graphic diagrams generation and vizualisation in the browser
- From function name to online documentation
- Inter-linked library files
- Browse generated C++ code inside Emacs
- Fully configurable (build type/target/architecture/toolkit, keyboard shortcuts, etc.)
- Automatic keyword completion
- Modeline indicator of the state of the code

## How can I install it?
- Clone this repo in your ~/.emacs.d/lisp dir
- Ensure this directory is in the PATH :

```lisp
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
```

- Require the file in your init file:
`(require 'faustine)`

## How do I use it?
- Open a .dsp file (or use `faustine-configure` (`C-c C-p`) to set your own Faust file extension)
- Look at you modeline, in the bottom of the buffer: The bug is green when your Faust code compiles without errors, and red otherwise. This icon is also a menu where you can access Faustine's main functions and commands.
- Use `faustine-diagram` (`C-c C-d`) to view the diagram of the current Faust file
    - Use `faustine-diagram-all` (`C-c C-S-d`) to view the diagrams of the linked (component) Faust files
- Use `faustine-build` (`C-c C-b`) to build the executable of the current Faust file
    - Use `faustine-build-all` (`C-c C-S-d`) to build the executables of the linked (component) Faust files
- Use `faustine-build` (`C-c C-b`) to build the executable of the current Faust file
- Use `faustine-source-code` (`C-c C-s`) to view the C++ code of the current Faust file
- Use `faustine-mdoc` (`C-c C-m`) to view the Mdoc of the current Faust file
- Use `faustine-configure` (`C-c C-p`) to set your options/preferences
- Use `C-h m` for help and commands

### Can I thank you?

There are several ways you can help (by order of magnificence) :

- Subscribe to [my YouTube channel](https://www.youtube.com/c/YassinPhilip-ManyRecords)
- Buy my music on [Bandcamp](https://yassinphilip.bandcamp.com) (I'm told I'm on iTunes, Spotify, Google Music and stuff but I never seem to sell a single song)
- Make a [donation](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=yassinphil%40gmail%2ecom&lc=BM&item_name=Yassin%20Philip&no_note=0&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donateCC_LG%2egif%3aNonHostedGuest)
- Help me on [Patreon](http://www.patreon.com/yassinphilip)

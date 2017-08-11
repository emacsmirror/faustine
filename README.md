# Faustine
## Edit, visualize, build and run Faust code

Faustine is a [Faust](http://faust.grame.fr) major mode/IDE for [GNU Emacs](https://www.gnu.org/software/emacs/).

[Faust](http://faust.grame.fr) (Functional Audio Stream) is a functional programming language specifically designed for real-time signal processing and synthesis. FAUST targets high-performance signal processing applications and audio plug-ins for a variety of platforms and standards.

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

## Usage
- Just open a .dsp file/project (use `faustine-configure` to set your own Faust file extension)
- Look at the modeline, in the bottom of the buffer: The bug is green when your Faust code compiles without errors, and red otherwise. This icon is also a menu where you can access Faustine's main functions and commands.
- Use `faustine-toggle-output-buffer` to view the Faust build output
- Use `faustine-diagram` to view the diagram of the current Faust file
    - Use `faustine-diagram-all` to view the diagrams of the linked (component) Faust files
- Use `faustine-build` to build the executable of the current Faust file
    - Use `faustine-build-all` to build the executables of the linked (component) Faust files
- Use `faustine-source-code` to view the C++ code of the current Faust file
- Use `faustine-mdoc` to view the Mdoc (PDF) of the current Faust file
- Select a Faust function, and use `C-c C-h` to view its definition/docstring
- Use `faustine-configure` to set your options/preferences
- Use `C-h m` for help and commands

## Installation
- Clone this repo in your PATH
- Require the file in your init file:
`(require 'faustine)`

### Credits
There are several ways you can help (by order of magnificence) :

- Report any bugs and submit feature requests
- Subscribe to [my YouTube channel](https://www.youtube.com/c/YassinPhilip-ManyRecords)
- Buy my music on [Bandcamp](https://yassinphilip.bandcamp.com) (I'm told I'm on iTunes, Spotify, Google Music and stuff but I never seem to sell a single song)
- Make a [donation](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=yassinphil%40gmail%2ecom&lc=BM&item_name=Yassin%20Philip&no_note=0&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donateCC_LG%2egif%3aNonHostedGuest)
- Help me on [Patreon](http://www.patreon.com/yassinphilip)

# Emacs Faust IDE

A Faust code editor for Emacs

[FAUST](http://faust.grame.fr) (Functional Audio Stream) is a functional programming language specifically designed for real-time signal processing and synthesis. FAUST targets high-performance signal processing applications and audio plug-ins for a variety of platforms and standards.

## What does it do?
- Faust code syntax hightlighting plus indentation
- Build/compile with resizable output window
- Graphic diagrams generation and vizualisation in the browser
- From function name to online documentation
- Browse generated C++ code inside Emacs
- Fully configurable (build type/target, toolkit, preferences, etc.) via `customize-group emacs-faust-ide RET`
- Automatic keyword completion

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
- Help me on [Patreon](http://www.patreon.com/yassinphilip)

<form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_top">
<input type="hidden" name="cmd" value="_donations">
<input type="hidden" name="business" value="yassinphil@gmail.com">
<input type="hidden" name="lc" value="FR">
<input type="hidden" name="item_name" value="Yassin Philip">
<input type="hidden" name="no_note" value="0">
<input type="hidden" name="currency_code" value="USD">
<input type="hidden" name="bn" value="PP-DonationsBF:btn_donate_SM.gif:NonHostedGuest">
<input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif" border="0" name="submit" alt="PayPal - The safer, easier way to pay online!">
<img alt="" border="0" src="https://www.paypalobjects.com/en_US/i/scr/pixel.gif" width="1" height="1">
</form>

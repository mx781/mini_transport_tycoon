Mini Transport Tycoon
=====================

Installation: 3110 VM
----------------------

Mini Transport Tycoon (MTT) depends on GTK+ and various image libraries 
associated with it, as well as the camlgraphics, ocamlgraph, camlimages,
lablgtk and piqi libraries for OCaml.

A setup script is shipped with the game to automatically install the
prerequisites, compile the game, and run it. On an initialized CS3110 VM, run 
`bash setup.sh` in the root game directory. Enter your root password when
prompted.

In case an error occurs while running the script, try running it
step-by-step by issuing the following commands:

1. `sudo apt-get install gtk2.0` This will install gtk+.
2. `sudo apt-get install libpng12-dev libjpeg-dev libtiff-dev libxpm-dev
libfreetype6-dev libgif-dev` This will install the required image libraries.
3. `opam install graphics ocamlgraph camlimages lablgtk piqi` This will install
the required opam packages for OCaml.
4. Finally, run `make` to compile and launch the game.

In some marginal cases, the initial run of `make` generates an error saying
"Package piqirun.ext not found". This can be resolved by running `make clean`, 
and then re-running `make`.

Since MTT has a lot of dependencies and updates to a fresh VM might cause
compatibility issues, additional errors might occur. In such an event, the
development team will gladly assist you in resolving them. Please send an email
to mk2228@cornell.edu, jtg98@cornell.edu, dl556@cornell.edu, prw55@cornell.edu, 
and we will get back to you ASAP.

Installation: macOS
-------------------

If you want to run MTT natively on macOS, you may additionally need to install
X11/XQuartz for Graphics support. If you installed ocaml with homebrew, it can
be done by running

```
brew install Caskroom/cask/xquartz
brew reinstall ocaml --with-x11
```

Then, map opam to use the system installation instead of the currently bound
one: `opam switch sys`. Then run ``eval `opam config env` ``  as instructed. You
should then be able to compile by running `make`.

Gameplay
--------

Once you have successfully compiled the game, it should launch automatically. If
running natively on macOS, it might take a few seconds to power up XQuartz. The
game can be manually launched by running `./main.byte`. 

Please make sure that both the Terminal and the main game Window is visible on 
your screen to see instructions and updates as you play.

**Resolution warning**: Depending on the host machine's native resolution and
pixel ratio, as well as any changes made to the VM's resolution, the gameplay
screen might appear too small to play. The game has been tested on the following
resolutions:

- host @ 1366x768, VM @ 1024x768 
- host @ 3840x2160, VM @ 1920x955
- host @ 2560x1440, VM @ 2560x1240
- native on macOS @ 2880x1800

A quickstart for how to play is displayed on the Terminal if you want to jump 
right in. Detailed instructions and gameplay strategies can be found in 
*game_instructions.pdf*.
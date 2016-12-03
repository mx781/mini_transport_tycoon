# CS3110_Final

Mini Transport Tycoon
=====================

Instructions: 3110 VM
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

Instructions: macOS
------------------------------

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
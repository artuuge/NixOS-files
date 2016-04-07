## NixOS-files

This repository is meant to contain a collection of files which I have found useful in configuring my NixOS installation.

### epson-escpr

This directory describes how to install an Epson driver `epson-inkjet-printer-escpr` for a family of inkjet printers listed [here](http://www.openprinting.org/driver/epson-escpr/). The driver itself can be found [here](http://download.ebz.epson.net/dsc/search/01/search/?OSC=LX). The current patch fixes the original Debian oriented `configure` script to match NixOS.

### bluetooth-firmware

This directory shows how to flash a firmware file into a Bluetooth chip during a boot process on NixOS. Additional comments on why this might be needed can be found [here](https://wiki.archlinux.org/index.php/bluetooth).

### nix-monad

This directory explains how to install XMonad on NixOS. The default functionality of XMonad relies on other packages like [dmenu](https://wiki.archlinux.org/index.php/dmenu) which are implicitly assumed to be available in a login session. It can be convenient to include them all in a single wrapper.

### xmobar-volume

This section contains instructions on how to set-up volume control for [XMobar](https://github.com/jaor/xmobar) running [XMonad](https://github.com/xmonad/xmonad) on [NixOS](http://nixos.org). Since XMobar comes without a default volume control plug-in, one may consider different solutions to this problem. Perhaps, the most natural  approach is suggested [here](https://github.com/bchurchill/xmonad-pulsevolume), where one binds together `xmonad`, `xmobar` and `pulseaudio`, with a pair of short scripts written in python and bash. The present directory provides an implementation of the volume control utility with a similar functionality. The source code is written in Haskell as a single program, and the result is attached to XMonad, XMobar, and PulseAudio, via several nix wrappers.

### emacs-packages

There exists a large amount of packages for Emacs with most of them being available on-line from such repositories as [MELPA](https://melpa.org/). It is nonetheless of interest to be able, at least in principle, to use some Lisp code stemming from a local machine in a way compatible with the NixOS purely functional philosophy. In the present directory, I show how to do it creating a simple `helloworld.el` example. It is natural to wrap this file in a separate package, and to set it up via `emacsWithPackages`.

### smalltalk-mode

If you wish to experiment with [GNU Smalltalk](http://smalltalk.gnu.org/) using Emacs, you are, probably, going to need the `smalltalk-mode` feature. The corresponding Lisp code, as well as an implementation of the `gst` REPL, are, in principle, included in the `gnu-smalltalk` package for NixOS, but some additional set-up is still needed, since Emacs should to be told where to find the files in `/nix/store`. It is natural to implement a separate package describing the necessary auto-loads at start-up, so that one can reference it in `emacs-with-packages`.

### ghcjs-wrapper

This directory shows how to get an access to the [ghcjs](https://github.com/ghcjs/ghcjs) compiler on NixOS using the standard nix wrapper `ghcWithPackages`. As an example, one may compile and run in a browser a simple Haskell program making use of the [Reflex](https://github.com/reflex-frp/reflex-platform) platform for functional reactive programming.

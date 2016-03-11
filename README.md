## NixOS-files

This repository is meant to contain a collection of files which I have found useful in configuring my NixOS installation.

### epson-escpr

This directory describes how to install an Epson driver `epson-inkjet-printer-escpr` for a family of inkjet printers listed [here](http://www.openprinting.org/driver/epson-escpr/). The driver itself can be found [here](http://download.ebz.epson.net/dsc/search/01/search/?OSC=LX). The current patch fixes the original Debian oriented `configure` script to match NixOS.

### bluetooth-firmware

This directory shows how to flash a firmware file into a Bluetooth chip during a boot process on NixOS. Additional comments on why this might be needed can be found [here](https://wiki.archlinux.org/index.php/bluetooth).

### nix-monad

This directory explains how to install XMonad on NixOS. The default functionality of XMonad relies on other packages like [dmenu](https://wiki.archlinux.org/index.php/dmenu) which are implicitly assumed to be available in a login session. It can be convenient to include them all in a single wrapper.

### xmobar-volume

This section contains instructions on how to set-up volume control for [XMobar](https://github.com/jaor/xmobar) running [XMonad](https://github.com/xmonad/xmonad) on [NixOS](http://nixos.org). Since XMobar comes without a default volume control plug-in, one may consider different solutions to this problem. Perhaps, the most natural  approach is suggested [here](https://github.com/bchurchill/xmonad-pulsevolume),   where one binds together `xmonad`, `xmobar` and `pulseaudio`, with a pair of short scripts written in python and bash. The present directory provides an implementation of the volume control utility with a similar functionality. The source code is written in Haskell as a single program, and the result is attached to XMonad, XMobar, and PulseAudio, via several nix wrappers.

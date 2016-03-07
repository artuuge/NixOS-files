## NixOS-files

This repository is meant to contain a collection of files which I have found useful in configuring my NixOS installation.

### epson-escpr

This directory describes how to install an Epson driver `epson-inkjet-printer-escpr` for a family of inkjet printers listed [here](http://www.openprinting.org/driver/epson-escpr/). The driver itself can be found [here](http://download.ebz.epson.net/dsc/search/01/search/?OSC=LX). The current patch fixes the original Debian oriented `configure` script to match NixOS.

### bluetooth-firmware

This directory shows how to flash a firmware file into a Bluetooth chip during a boot process on NixOS. Additional comments on why this might be needed can be found [here](https://wiki.archlinux.org/index.php/bluetooth).

### nix-monad

This directory explains how to install XMonad on NixOS. The default functionality of XMonad relies on other packages like [dmenu](https://wiki.archlinux.org/index.php/dmenu) which are implicitly assumed to be available in a login session. It can be convenient to include them all in a single wrapper.

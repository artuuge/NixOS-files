## NixOS-files

This repository is meant to contain a collection of files which I have found useful in configuring my NixOS installation.

### epson-escpr

This directory describes how to install an Epson driver
`epson-inkjet-printer-escpr-1.6.3-1lsb3.2.tar.gz`
for a family of inkjet printers listed [here](http://www.openprinting.org/driver/epson-escpr/).
The driver itself can be found [here](http://download.ebz.epson.net/dsc/search/01/search/?OSC=LX).
The current patch fixes the original Debian oriented `configure` script to match NixOS.

## NixOS-files

This repository is meant to contain a collection of files which I have found useful in configuring my NixOS installation.

## epson-escpr

Having installed the NixOS distribution 15.09 I have faced a problem to setup an Epson inkjet printer WorkForce-3620: the corresponding package for NixOS was absent.

Epson provides a [driver](http://www.openprinting.org/driver/epson-escpr/) which can be downloaded as a file `epson-inkjet-printer-escpr-1.6.3-1lsb3.2.tar.gz` from [here](https://download3.ebz.epson.net/dsc/f/03/00/04/33/53/0177a44361d3dfeacf7f15ff4a347cef373688da/epson-inkjet-printer-escpr-1.6.3-1lsb3.2.tar.gz).

The problem is that the configuration script is tuned for a Debian-like GNU/Linux, so some changes are required to fit NixOS. Assume that you have cloned the `nixpkgs` repository as follows:
```
cd /path/to/
git clone https://github.com/NixOS/nixpkgs.git
```
and that the Epson driver mentioned is not present in your favourite branch. Create a directory:
```
cd  /path/to/nixpkgs/pkgs/misc/drivers/
mkdir epson-escpr
```
and copy the supplied files `cups-filter-ppd-dirs.patch` and `default.nix` into it. Open the file `/path/to/nixpkgs/pkgs/top-level/all-packages.nix` in a text editor and register a package:
```nix
{
  epson-escpr = callPackage ../misc/drivers/epson-escpr { };
}
```
Test that the package appears as an output of the following command:
```
nix-env -f /path/to/nixpkgs -qaA pkgs.epson-escpr
```
Enable printing in `/etc/nixos/configuration.nix`:
```nix
{
  services.printing = {
    enable = true;
    drivers = [ pkgs.epson-escpr ];
  };
}
```
and rebuild your system:
```
nixos-rebuild dry-run -I nixpkgs=/path/to/nixpkgs
sudo -i nixos-rebuild switch -I nixpkgs=/path/to/nixpkgs
```
The rest of the printer setup is done in a browser on the CUPS page which would typically be at [http://localhost:631](http://localhost:631). Follow a standard procedure of adding a printer. WorkForce-3620 as well as many other models should appear in the list of Epson devices in the step where one is asked to supply a PPD file.

#### The patch

The suggested patch corresponds to the pull request [#12875](https://github.com/NixOS/nixpkgs/pull/12875).
Suppose you have downloaded the file `epson-inkjet-printer-escpr-1.6.3-1lsb3.2.tar.gz` and unpacked it:
```
tar xf epson-inkjet-printer-escpr-1.6.3-1lsb3.2.tar.gz
cd epson-inkjet-printer-escpr-1.6.3
```
To test whether the package might get installed without a patch, create a `default.nix` file:
```nix
{ cups, stdenv }:
stdenv.mkDerivation {
  name = "myEpson";
  src = ./.;
  buildInputs = [ cups ];
}
```
and a `release.nix` file:
```nix
let
  pkgs   = import /path/to/nixpkgs {};
  myEpson = pkgs.callPackage (import ./default.nix) {};
in
  myEpson
```
Drop yourself into a nix-shell, build the driver, and look at the result:
```
nix-shell -p cups tree
nix-build release.nix
tree -L 6 -F result
```
This should yield:
```
result
├── lib/
│   ├── libescpr.la*
│   ├── libescpr.so -> libescpr.so.1.0.0*
│   ├── libescpr.so.1 -> libescpr.so.1.0.0*
│   └── libescpr.so.1.0.0*
└── store/
    └── abcdefghijklmnopqrstuvwxyz123456-cups-2.0.4/
        ├── lib/
        │   └── cups/
        │       └── filter/
        │           ├── epson-escpr*
        │           └── epson-escpr-wrapper*
        └── share/
            └── cups/
                └── model/
                    └── epson-inkjet-printer-escpr/
```
If you run just `tree result` without a cut-off, you are going to observe a long list of PPD files in the `epson-inkjet-printer-escpr` subdirectory.

The supplied patch basically gets rid of the subdirectory with the nix hash prefix and puts the cups files in a correct place. The origin of the problem is the pair of commands
```bash
cups-config --serverbin
cups-config --datadir
```
which are used in the original `configure` script to compute the values of the environment variables `CUPS_FILTER_DIR` and `CUPS_PPD_DIR`. In short, the patch can be perceived as a shortcut:
```bash
CUPS_FILTER_DIR="${prefix}/lib/cups/filter"
CUPS_PPD_DIR="${prefix}/share/cups/model"
```
Copy the supplied `cups-filter-ppd-dirs.patch` in your working directory, and add a line in `default.nix`:
```nix
{ cups, stdenv }:
stdenv.mkDerivation {
  name = "myEpson";
  src = ./.;
  buildInputs = [ cups ];
  patches = [ ./cups-filter-ppd-dirs.patch ];
}
```
Executing
```bash
nix-build release.nix
tree -L 4 -F result
```
yields:
```
result
├── lib/
│   ├── cups/
│   │   └── filter/
│   │       ├── epson-escpr*
│   │       └── epson-escpr-wrapper*
│   ├── libescpr.la*
│   ├── libescpr.so -> libescpr.so.1.0.0*
│   ├── libescpr.so.1 -> libescpr.so.1.0.0*
│   └── libescpr.so.1.0.0*
└── share/
    └── cups/
        └── model/
            └── epson-inkjet-printer-escpr/
```
The nix hash prefix is gone. It remains to notice that the file `/path/to/nixpkgs/nixos/modules/services/printing/cupsd.nix` contains the following pieces of code,
```nix
{
  cfg = config.services.printing;

  bindir = pkgs.buildEnv {
    name = "cups-progs";
    paths = cfg.drivers;
    pathsToLink = [ "/lib/cups" "/share/cups" "/bin" "/etc/cups" ];
    postBuild = cfg.bindirCmds;
    ignoreCollisions = true;
  };

  config = {   
    services.printing.cupsFilesConf =
      ''
        ...
        ServerBin ${bindir}/lib/cups
        DataDir ${bindir}/share/cups
        ...
      '';

    services.printing.cupsdConf =
      ''
        ...
        SetEnv PATH ${bindir}/lib/cups/filter:${bindir}/bin:${bindir}/sbin
        ...
      '';
  };
}
```
so the obtained directory structure for the `epson-escpr` driver turns out to be compatible with what is required by `cupsd`.

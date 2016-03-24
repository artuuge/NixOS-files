## Smalltalk Mode

The NixOS package `gnu-smalltalk`, if you look at the `default.nix` file in `<nixpkgs>/pkgs/development/compilers/gnu-smalltalk`, takes an argument `emacsSupport`, which has a default value set to `false`. To have GNU Smalltalk running in Emacs, one needs to build `gnu-smalltalk` with this parameter set to `true`.

Clone the directory `gnu-smalltalk-mode` as a sub-directory of `/path/to` on your local machine. Edit your local configuration `~/.nixpkgs/config.nix`:
```nix
{
  emacsSupport = true;
  packageOverrides = super: let self = super.pkgs; in with self; {

    emacsPackagesNg = super.emacsPackagesNg.override (su: se: with se; {
      gnu-smalltalk-mode = (callPackage (import /path/to/gnu-smalltalk-mode/default.nix) {});
    });

    myEmacs = emacsPackagesNg.emacsWithPackages (es: with es; [
      gnu-smalltalk-mode
    ]);

  };
}
```
In terms of the global configuration `/etc/nixos/configuration.nix`, the `emacsSupport` option enabled corresponds to:
```nix
{
  nixpkgs.config.emacsSupport = true;
}
```
while an installation of Emacs corresponds to:
```nix
{
  environment.systemPackages = [ pkgs.myEmacs ];
}
```
where `myEmacs` should be described by the `packageOverrides` attribute of `nixpkgs.config` in this file.

The directory `/path/to/gnu-smalltalk-mode` contains three files: `gnu-smalltalk-mode.el` describing the auto-loads which need to happen in Emacs upon the editor start-up, `default.nix` installation script, and `release.nix` script meant for testing if things get built as expected. Switch to this directory, and run:
```bash
nix-build release.nix
tree result
```
The corresponding folder structure representation is going to show what the package build yields relative to an installation prefix. One may there observe a pair of symbolic links `gst-mode.elc` and `smalltalk-mode.elc` referencing the corresponding files in the `gnu-smalltalk` realization. If the option `emacsSupport` were not enabled, these files would not had been created by the installation script of `gnu-smalltalk`.

Install Emacs in your session via the `emacs-with-packages` wrapper:
```bash
nix-env -f /path/to/nixpkgs -iA pkgs.myEmacs
```
where it is assumed that `<nixpkgs>` directory is `/path/to`. This may be different on your machine. One should normally be able to invoke the GNU Smalltalk mode with `M-x smalltalk-mode`, and to invoke a REPL with `M-x gst`. If you open a file with an extension `.st`, the `smalltalk-mode` starts automatically, yielding the corresponding syntax highlighting.  

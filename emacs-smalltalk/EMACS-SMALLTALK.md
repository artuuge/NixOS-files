## GNU Smalltalk mode for Emacs

The collection of Emacs packages available on-line is quite large, but it is sometimes needed to install an Emacs package which is not present in the standard repositories, or, perhaps, to create a new one of your own. Let us work out an example with `gnu-smalltalk` for NixOS:
```bash
nix-env -f /path/to/nixpkgs -iA pkgs.gnu-smalltalk
```
where `/path/to` is the directory containing `<nixpkgs>`. The source code of this package comes with a pair of Lisp files, `smalltalk-mode.el` and `gst-mode.el`, which need to be patched. It makes sense to separate them from the main bundle and to create a standalone package for Emacs. The result can then be used in `emacsWithPackages` wrapper following the comments included in the file `/path/to/nixpkgs/pkgs/build-support/emacs/wrapper.nix`.

The overrides for Emacs packages are implemented in a similar way to the overrides of Haskell packages. The counterpart of `ghcWithPackages` is `emacsWithPackages`, and the counterpart of `haskellPackages` is `emacsPackagesNg`. One should keep in mind that the analogy is not perfect though. Clone the directory `gnu_smalltalk-mode` to `/path/to/gnu_smalltalk-mode` on your local machine. Switch to this directory, and look at `default.nix`:
```nix
{ melpaBuild }:
melpaBuild {
  pname = "gnu_smalltalk-mode";
  version = "0.1.0.0";
  src = ./.;
}
```
and `release.nix`:
```nix
let
  pkgs = import <nixpkgs> {};
  newpkgs = pkgs.overridePackages (self: super: with self; {

    emacsPackagesNg = super.emacsPackagesNg.override (su: se: with se; {
      gnu_smalltalk-mode = (callPackage (import ./default.nix) {});
    });

  });
in
  newpkgs.emacsPackagesNg.gnu_smalltalk-mode
```
The other two files present in the directory, `smalltalk-mode.el` and `gst-mode.el`, are the patched versions of the files from the `gnu-smalltalk` source. To test if everything gets built properly, run:
```bash
nix-build release.nix
```
and explore the outcome executing `tree result`:
```
result
├── nix-support
│   └── setup-hook
└── share
    └── emacs
        └── site-lisp
            └── elpa
                └── gnu_smalltalk-mode-0.1.0.0
                    ├── gst-mode.el
                    ├── gst-mode.elc
                    ├── gnu_smalltalk-mode-autoloads.el
                    ├── gnu_smalltalk-mode-autoloads.el~
                    ├── gnu_smalltalk-mode-pkg.el
                    ├── smalltalk-mode.el
                    └── smalltalk-mode.elc
```
Notice that there appears a file `gnu_smalltalk-mode-autoloads.el` which implements the auto-loads described in the end of the files `smalltalk-mode.el` and `gst-mode.el`. The corresponding instructions are marked by
```elisp
;;;###autoload
```
They are not present in the original files, but it was necessary to add them to fit in NixOS. The patches contain additional changes to get rid of the warnings of the Emacs Lisp compiler during the build process.

The current example is meant more as an illustration of how to install an Emacs package on NixOS from a local source. If one wishes to develop in Smalltalk in Emacs, one should consider the [shampoo](https://github.com/dmatveev/shampoo-homepage) package. There is also a generic Smalltalk environment [Pharo](http://pharo.org/) under NixOS:
```bash
nix-env -f /path/to/nixpkgs -iA pkgs.pharo-launcher
```
To install `gnu_smalltalk-mode` package on your system, you should naturally modify the contents of `release.nix` and embed the result in the configuration file. To have Emacs with smalltalk mode installed globally, add the following to `/etc/nixos/configuration.nix`:
```nix
{
  environment.systemPackages = [ pkgs.myEmacs ];

  nixpkgs.config = {
    packageOverrides = super: let self = super.pkgs; in with self; {

      emacsPackagesNg = super.emacsPackagesNg.override (su: se: with se; {
        gnu_smalltalk-mode = (callPackage (import /path/to/smalltalk-mode/default.nix) {});
      });

      myEmacs = emacsPackagesNg.emacsWithPackages (es: with es;
        [ haskell-mode nix-mode gnu_smalltalk-mode ]);
    };
  };
}
```
Note that the path needs to be absolute. The other possibility is to have an installation per session. Put the `packageOverrides` mentioned in your `~/.nixpkgs/config.nix`, and run:
```bash
nix-env -f /path/to/nixpkgs -iA pkgs.myEmacs
```
Include other packages in `myEmacs` if necessary. Notice that there is no need to edit the `~/.emacs` session configuration file to have the auto-loads.

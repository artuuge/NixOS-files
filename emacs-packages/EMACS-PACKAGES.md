## Emacs Packages

The collection of Emacs packages available on-line is quite large, but it is sometimes needed to install an Emacs package which is not present in the standard repositories, or, perhaps, to create a new one of your own. Let us work out a simple example implementing a `helloworld` library.

Denote `/path/to` an absolute path to the directory containing `<nixpkgs>`. We are going to use it to hold the `helloworld` library as well. Change to `/path/to`, clone the directory `helloworld`, and look at the three files contained in the result `/path/to/helloworld`. The file `default.nix`:
```nix
{ melpaBuild }:
melpaBuild {
  pname = "helloworld";
  version = "0.1.0.0";
  src = ./.;
}
```
defines an Emacs package `helloworld`. It is important that the name of the package is the same as the name of one of the Lisp files present in the directory, up to the `.el` extension. We have a single Lisp file `helloworld.el`:
```lisp
;;; helloworld.el ---

(defun helloworld-fun () ""
       (interactive)
       (print "hello world")
       nil)

;;;###autoload
(autoload 'helloworld-fun "helloworld" "" t)

(provide 'helloworld-fun)
```
and its name satisfies the condition mentioned. One should here pay attention to the first line since it defines a minimal correct header for the `helloworld` library. The single function implemented is called  `helloworld-fun` and one can observe that its prefix coincides with the name of the package. This is necessary to avoid the warnings of the Lisp compiler which are meant to prevent the name clashes. This function is going to be auto-loaded in Emacs as described by the code following the line marked `;;;###autoload`. The string `helloworld` is going to be automatically extended with `.el` or `.elc` and the system is going to find the corresponding file in the package directory.

The third file is `release.nix`:
```nix
let
  pkgs = import <nixpkgs> {};
  newpkgs = pkgs.overridePackages (self: super: with self; {

    emacsPackagesNg = super.emacsPackagesNg.override (su: se: with se; {
      helloworld = (callPackage (import ./default.nix) {});
    });

  });
in
  newpkgs.emacsPackagesNg.helloworld
```
It is meant to test if the package gets built up properly. Switch to the `/path/to/helloworld` directory, and run:
```bash
nix-build release.nix
tree result
```  
This should normally yield:
```
result
├── nix-support
│   └── setup-hook
└── share
    └── emacs
        └── site-lisp
            └── elpa
                └── helloworld-0.1.0.0
                    ├── helloworld-autoloads.el
                    ├── helloworld-autoloads.el~
                    ├── helloworld.el
                    ├── helloworld.elc
                    ├── helloworld-pkg.el
                    └── helloworld-pkg.el
```
Notice that there is an automatically generated file `helloworld-autoloads.el` which describes the auto-loads extracted from `helloworld.el`.

To check-out the package, one can put the following in `~/.nixpkgs/config.nix`:
```nix
{
  packageOverrides = super: let self = super.pkgs; in with self; {

    emacsPackagesNg = super.emacsPackagesNg.override (su: se: with se; {
      helloworld = (callPackage (import /path/to/helloworld/default.nix) {});
    });

    myEmacs = emacsPackagesNg.emacsWithPackages (es: with es; [ helloworld ]);

  };
}
```
Install `myEmacs` in your profile:
```bash
nix-env -f /path/to/nixpkgs -iA pkgs.myEmacs
```
and launch Emacs. You should be able to run the `helloworld-fun` function by typing `M-x helloworld-fun <ENTER>`. To define a global installation, become a sudo and edit the file `/etc/nixos/configuration.nix`. Put the same value for `packageOverrides` in the attribute set corresponding to `nixpkgs.config`, and put additionally:
```nix
{
  environment.systemPackages = [ pkgs.myEmacs ];
}
```
Rebuild the system, and switch to the new configuration. The overrides for Emacs packages are implemented in a similar way to the overrides of Haskell packages. The counterpart of `ghcWithPackages` is `emacsWithPackages`, and the counterpart of `haskellPackages` is `emacsPackagesNg`. One should keep in mind that the analogy is not perfect though. As explained in `/path/to/nixpkgs/pkgs/build-support/emacs/wrapper.nix`, the right way to override an Emacs package is via overriding the definition of `emacsPackagesNg`, which contains the `emacsWithPackages` wrapper.

It is natural to add more packages in the definition of `myEmacs`. One may wish to at least have:
```nix
{
  myEmacs = emacsPackagesNg.emacsWithPackages (es: with es;
    [ haskell-mode nix-mode helloworld ]);
}
```
There is no need to edit the session configuration file `~/.emacs`. If one comments out the auto-load part in `helloworld.el`, it is still possible to access `helloworld-fun`. One should first load the library itself with `M-x load-library <ENTER> helloworld <ENTER>`, and then execute the function.

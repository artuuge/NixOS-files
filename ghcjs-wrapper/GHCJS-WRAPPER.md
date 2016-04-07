## GHCJS Wrapper

To install `ghcjs` in your session, edit the configuration file  `~/.nixpkgs/config.nix`:
```nix
{
  packageOverrides = super: let self = super.pkgs; in with self; {

    myHaskellJS = haskell.packages.ghcjs.ghcWithPackages (qs: with qs; [
      reflex
      reflex-dom
    ]);

  };
}
```
and run:
```bash
nix-env -f /path/to/nixpkgs -iA pkgs.myHaskellJS
```
where `/path/to/nixpkgs` is the path to the `nixpkgs` repository. If everything goes fine, build the supplied example `helloReflex.hs`:
```bash
ghcjs -O2 -j -Wall helloReflex.hs
```
This should create a sub-directory `helloReflex.jsexe` containing several files including the files with the JavaScript code and a file called `index.html`, which can be opened in a web browser.

If one looks at the file `/path/to/nixpkgs/pkgs/top-level/all-packages.nix`, then one may notice that the attribute `haskellPackages`, which is commonly used when working with `ghc`, can actually be perceived as a synonym for a longer name of the shape:  `haskell.packages.ghcXYZ`, where `ghcXYZ` is the current default choice of the compiler. It contains an attribute named `ghc` which refers to the compiler itself, and an attribute named `ghcWithPackages` which refers to the corresponding wrapper around it. Similarly with `ghcjs`: the value of `haskell.packages.ghcjs.ghc` references the current version of the `ghcjs` compiler, and `haskell.packages.ghcjs.ghcWithPackages` stands for the wrapper.

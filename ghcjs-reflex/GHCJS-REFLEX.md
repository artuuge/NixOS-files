## GHCJS Reflex

To make the Reflex libraries visible both to `ghc` and `ghcjs`, it is natural to use a pair of `ghcWithPackages` wrappers. Edit the configuration file  `~/.nixpkgs/config.nix`:
```nix
{
  packageOverrides = super: let self = super.pkgs; in with self; {

    myHaskell = haskellPackages.ghcWithPackages (qs: with qs; [
      reflex
      reflex-dom
    ]);

    myHaskellJS = haskell.packages.ghcjs.ghcWithPackages (qs: with qs; [
      reflex
      reflex-dom
    ]);

  }
}
```
Install `ghc` and `ghcjs` as follows:
```bash
nix-env -f /path/to/nixpkgs -iA pkgs.myHaskell
nix-env -f /path/to/nixpkgs -iA pkgs.myHaskellJS
```
where `/path/to/nixpkgs` is the path to the `nixpkgs` repository. You should now be able to compile the supplied example `helloReflex.hs`:
```bash
ghc -O2 -j -Wall helloReflex.hs
```
and then to run the corresponding binary created in the working directory:
```bash
./helloReflex
```
To test the `ghcjs` installation, execute:
```bash
ghcjs -O2 -j -Wall helloReflex.hs
```
This should create a sub-directory `helloReflex.jsexe` containing several files including the JavaScript code and a file called `index.html`. Open it in a browser to check if everything works fine.

The attribute `haskellPackages` used in the definition of `myHaskell` can actually be perceived as a synonym for a longer name: `haskell.packages.ghc7103`, where `ghc7103` is the current default choice of `ghc`. The latter can be seen in the `all-packages.nix` file located in `/path/to/nixpkgs/pkgs/top-level` directory. The set `haskell.packages.ghc7103` contains an attribute named `ghc` which refers to the actual compiler, and an attribute named `ghcWithPackages` which refers to the corresponding wrapper around it. Similarly with `ghcjs`: the value of `haskell.packages.ghcjs.ghc` references the current version of the `ghcjs` compiler, and `haskell.packages.ghcjs.ghcWithPackages` stands for the wrapper.

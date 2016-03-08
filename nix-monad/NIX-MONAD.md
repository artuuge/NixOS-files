<!-- ## How to Install XMonad on NixOS -->

### Getting Started

The default functionality of XMonad depends on several small utilities which are not required to build it, but which should normally be available at run-time in the login session . Install the following packages in your user profile:
```bash
nix-env -f /path/to/nixpkgs -iA pkgs.xorg.xmessage pkgs.dmenu pkgs.gmrun
```

Edit the file `/etc/nixos/configuration.nix`:
```nix
{
  services.xserver = {
    displayManager.lightdm.enable = true;

    desktopManager = {
      xterm.enable = false;
      xfce.enable = true;
      default = "xfce";
    };

    windowManager = {
      xmonad.enable = true;
      default = "xmonad";
    };
  };  
}
```
The implementation of `xmonad.enable` is defined in terms of `xmonad-with-packages` in `nixpkgs/nixos/modules/services/x11/window-managers/xmonad.nix`. Note that there exists a package called `xmonad` which is found under the attribute `haskellPackages`:
```bash
nix-env -f /path/to/nixpkgs -qaA pkgs.haskellPackages.xmonad
```
while `xmonad-with-packages` is a wrapper around it being an ordinary package:
```bash
nix-env -f /path/to/nixpkgs -qaA pkgs.xmonad-with-packages
```

Create a directory `~/.xmonad` containing a file `xmonad.hs`:
```haskell
module Main (main) where

import XMonad

main :: IO ()
main = xmonad def
```
Rebuild the system and switch to a new configuration:
```bash
nixos-rebuild build -I nixpkgs=/path/to/nixpkgs
sudo -i nixos-rebuild switch -I nixpkgs=/path/to/nixpkgs
```
Logout, reboot, and login.
You should be able to start `dmenu` with `mod-p`, and
`gmrun` with `mod-Shift-p`, where `mod` is the default modifier key `Left-Alt`. To invoke help press `mod-Shift-/` or `mod-?`, depending on your keyboard, and
to open `xterm` press `mod-Shift-Enter`.

### Nix Wrapper

The file defining `xmonad-with-packages` is called `wrapper.nix`, and it is located in the directory: `/path/to/nixpkgs/pkgs/applications/window-managers/xmonad`. The corresponding code attaches a pair of variables, `NIX_GHC` and `XMONAD_XMSSAGE`, to the environment in which `xmonad` is launched. These variables are used in the patch file `xmonad-nix.patch` located in the directory `/path/to/nixpkgs/pkgs/development/haskell-modules/patches`. It is invoked from the file `configuration-common.nix` located in `/path/to/nixpkgs/pkgs/development/haskell-modules`, which implements the initial layer of overrides defined in `default.nix` present in the same directory.

One modifies the function `recompile` from `XMonad/Core.hs`, so that it fits better in the NixOS context. This function is invoked when one recompiles the local configuration of XMonad defined in `~/.xmonad/xmonad.hs` by pressing `mod-q`. The idea is that instead of trying to find `ghc` or `xmessage` in the `PATH`, the program looks-up first the associated environment variable and gives a preference to its value if it is defined.

The patch mentioned is *incomplete* in a sense that the file `XMonad/Config.hs` is not patched, while there is a reference to `xmessage`. In `keys`, in the definition of a key binding `mod-Shift-/` which is meant to show a help screen, the string "xmessage" is substituted in a straightforward manner without taking into account `XMONAD_XMESSAGE`. It follows that if `xmessage` is not installed in the user profile, then one is not going to be able to see the help message.

### Additional Variables

It is straightforward to modify the wrapper to include more knowledge about the available applications. Create a directory `/path/to/nix-monad` containing a file `wrapper.nix`:
```nix
{ stdenv, ghcWithPackages, gxmessage, makeWrapper, packages ? self: [],
  dmenu, gmrun, xfce4-terminal }:

let
xmonadEnv = ghcWithPackages (self: [ self.xmonad ] ++ packages self);
in stdenv.mkDerivation {
  name = "xmonad-with-packages";

  nativeBuildInputs = [ makeWrapper ];

  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper ${xmonadEnv}/bin/xmonad $out/bin/xmonad \
      --set NIX_GHC "${xmonadEnv}/bin/ghc" \
      --set XMONAD_XMESSAGE "${gxmessage}/bin/gxmessage" \
      --set NIX_MONAD_DMENU_RUN "${dmenu}/bin/dmenu_run" \
      --set NIX_MONAD_GMRUN "${gmrun}/bin/gmrun" \
      --set NIX_MONAD_XFCE4_TERMINAL "${xfce4-terminal}/bin/xfce4-terminal"
  '';

  # trivial derivation
  preferLocalBuild = true;
  allowSubstitutes = false;
}
```
The additional variables have a prefix "NIX\_MONAD\_". The `xmessage` package is replaced with its counterpart `gxmessage`. Override the definition of `xmonad-with-packages` in `/etc/nixos/configuration.nix`:
```nix
{
  packageOverrides = pkgs: with pkgs; {
    xmonad-with-packages = callPackage /path/to/nix-monad/wrapper.nix {
      inherit (haskellPackages) ghcWithPackages;
      xfce4-terminal = pkgs.xfce.terminal;
    };
}
```
Rebuild the system, switch to a new configuration, and reboot.

It is now possible to use the variables "NIX_MONAD_DMENU_RUN", "NIX_MONAD_GMRUN", and "NIX_MONAD_XFCE4_TERMINAL" in `~/.xmonad/xmonad.hs`. The latter variable is meant to override the default definition of the terminal, ensuring that the other option is actually installed.

It is worth to point out, that to make `dmenu_run` work out of the box, it is not sufficient to invoke the value supplied by "NIX_MONAD_DMENU_RUN". `dmenu_run` is a shell script, which invokes another shell script `dmenu_path`, which should, in its turn,  be able to find a binary `stest`. To make things work, one needs to append the directory containing `dmenu_run` to the existing PATH, and run the script in this environment.

See the supplied `xmonad.hs` for details. Copy it to `~/.xmonad/xmonad.hs`, and then recompile and restart XMonad with `mod-q`. The resulting binary is found in `~/.xmonad` and its name has a shape:
```haskell
"xmonad-" ++ arch ++ "-" ++ os
```
where `arch` and `os` are defined in [System.Info](https://hackage.haskell.org/package/base-4.8.2.0/docs/System-Info.html). For example, this can yield a filename like: `xmonad-x86_64-linux`. The modifier key is changed to `mod4Mask`, the Windows key, and the default terminal is set to `xfce4-terminal`.

For the purity of experiment, un-install `xmessage`, `dmenu`, and `gmrun`:
```bash
nix-env -e xmessage dmenu gmrun
```
Delete the cache files like `~/.cache/dmenu_run` and `~/.gmrun_history`, and reboot.

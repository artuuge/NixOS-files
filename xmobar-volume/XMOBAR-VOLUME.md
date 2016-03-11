## Volume Control for XMobar

### Setting Up

Assume that XMonad is installed and running on your NixOS system. Denote `/path/to` a clone of the `xmobar-volume` directory on your computer. Put the following in your `/etc/nixos/configuration.nix`:
```nix
{
  nixpkgs.config = {
    packageOverrides = super: let self = super.pkgs; in with self; {

      xmonad-with-packages = callPackage /path/to/xmonad-with-packages/wrapper.nix {
        inherit (haskellPackages) ghcWithPackages xmonad xmonad-contrib;
        inherit (xorg) setxkbmap;
        xfce4-terminal = xfce.terminal;
      };

      xmobar-with-packages = (callPackage /path/to/xmobar-with-packages/wrapper.nix {
        xmobar = haskellPackages.xmobar;
        volumectl = haskellPackages.volumectl;
      });      

      volume-pulse = (callPackage (import /path/to/volume-pulse/wrapper.nix) {
        volumectl = haskellPackages.volumectl;
        pulseaudio = pulseaudioFull;
      });

      haskellPackages = recurseIntoAttrs (super.haskellPackages.override {
        overrides = se: su: with se; {
          volumectl = (callPackage (import /path/to/volumectl/default.nix) {});
        };
      });
    };
  };
}
```
Copy the file `xmonad.hs` to `~/.xmonad` directory, and copy the file `dot_xmobarrc` to your home directory renaming the result into `~/.xmobarrc`. Ensure that the `<nixpkgs>` directory can be found in your `nixPath` and that the collection of packages is recent enough. For example, execute the commands from your home directory:
```bash
git clone https://github.com/NixOS/nixpkgs.git
sudo ln -s /etc/nixos/nixpkgs ~/nixpkgs
```
and set an attribute in your `/etc/nixos/configuration.nix`:
```nix
{
  nix.nixPath = [ "/etc/nixos" "nixos-config=/etc/nixos/configuration.nix" ];   
}
```

Rebuild NixOS and switch to the new configuration:
```bash
nixos-rebuild build
sudo -i nixos-rebuild switch
```
Logout and reboot. Finally, recompile and restart XMonad:
```
xmonad --recompile && xmonad --restart
```
The latter can be normally done with `CTRL-q` unless the default definitions of the key-bindings have not been overridden. Invoke help with `CTRL-SHIFT-/`, or `CTRL-?`.

### Volume Control
The file `~/.xmobarrc` is the configuration file for `xmobar`. It contains a reference to the `volumectl` binary which implements the volume control utility. It is possible to explore what `volumectl` does separately. Switch to `/path/to/volumectl`, and run:
```bash
nix-build release.nix
```
Look at the result:
```bash
tree -F result
```
This should yield a tree similar to the following:
```
result
├── bin/
│   └── volumectl*
└── share/
    └── doc/
        └── x86_64-linux-ghc-7.10.3/
            └── volumectl-0.1.0.0/
                └── LICENSE
```
If you run the `volumectl` binary without argument, it is going to put in the `stdout` a textual representation of the current volume and mute state, which is read from the files `~/.volume` and `~/.mute`. If these files do not exist, they are going to be created initialized with the default values: the middle point of the volume range, and the un-muted state, respectively.

The binary `volumectl` implements, at the same time, a functionality permitting to change the volume. This is done by supplying a command line argument, for which there are four possibilities: `volumectl increase` increases the volume by a fixed step, `volumectl decrease` decreases the volume by the same amount, `volumectl mute` and `volumectl unmute` switches the sound off and on, respectively.

### Nix Wrappers

You can explore the other sub-directories `volume-pulse`, `xmobar-with-packages`, and `xmonad-with-packages`, in a similar way to `volumectl` using the supplied `release.nix` files. One may notice, that the contents of these files correspond to snippets of code put in `/etc/nixos/configuration.nix`, except one needs absolute names of paths in the latter file.

XMobar does not need PulseAudio to implement the textual representation of the volume and mute state. What is required is that `xmobar` should be able to find `volumectl` which is mentioned in the configuration. For this reason, it is natural to implement a wrapper `xmobar-with-packages`, creating a pair of symbolic links to `xmobar` and `volumectl`:
```
result
└── bin
    ├── volumectl -> /nix/store/abcdef...01234-volumectl-0.1.0.0/bin/volumectl
    └── xmobar -> /nix/store/pqrstu...56789-xmobar-0.23.1/bin/xmobar
```
XMobar is then called from XMonad with a `PATH` prefixed with the directory containing the symbolic link to `xmobar` mentioned, so that the symbolic link to `volumectl` falls into the `PATH`.

PulseAudio is needed for the actual change of volume. It is referred in the source code of `volumectl` via an environment variable `NIX_PACTL`.  This motivates the wrapper `volume-pulse` which attaches this variable containing the absolute path to the `pulseaudio` binary to the shell script invoking `volumectl`.

Finally, XMonad needs to be told about the existence of `xmobar` and `volumectl`. This is done in the `xmonad-with-packages` wrapper via the environment variables `NIX_MONAD_XMOBAR` and `NIX_MONAD_VOLUME_PULSE`, respectively. Their values are read as described in the `xmonad.hs` code, so that `volumectl` and `xmobar` are called in the correct environments. One can use this method to invoke other utilities as well, such as `dmenu`, `gmrun`, or `xmessage`. As an additional example, the supplied `xmonad.hs` contains a keyboard switch between a pair of different layouts.

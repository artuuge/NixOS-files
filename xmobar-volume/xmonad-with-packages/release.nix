let
  pkgs = import <nixpkgs> {};
  newpkgs = pkgs.overridePackages (self: super: with self; {

    xmonad-with-packages = callPackage ./wrapper.nix {
      inherit (haskellPackages) ghcWithPackages xmonad xmonad-contrib;
      inherit (xorg) setxkbmap;
      xfce4-terminal = xfce.terminal;
    };

      xmobar-with-packages = (callPackage ../xmobar-with-packages/wrapper.nix {
        xmobar = haskellPackages.xmobar;
        volumectl = haskellPackages.volumectl;
      });

      volume-pulse = (callPackage (import ../volume-pulse/wrapper.nix) {
        volumectl = haskellPackages.volumectl;
        pulseaudio = pulseaudioFull;
      });

      haskellPackages =
        recurseIntoAttrs (super.haskellPackages.override {
          overrides = se: su: with se; {
            volumectl = (callPackage (import ../volumectl/default.nix) {});
          };
      });

  }); 
in 
  newpkgs.xmonad-with-packages

let
  pkgs = import <nixpkgs> {};
  newpkgs = pkgs.overridePackages (self: super: with self; { 

    volume-pulse = (callPackage (import ./wrapper.nix) {
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
  newpkgs.volume-pulse

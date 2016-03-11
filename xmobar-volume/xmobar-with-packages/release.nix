let
  pkgs = import <nixpkgs> {};
  newpkgs = pkgs.overridePackages (self: super: with self; {

    xmobar-with-packages = (callPackage ./wrapper.nix {
      xmobar = haskellPackages.xmobar; 
      volumectl = haskellPackages.volumectl; 
    });
    
    haskellPackages = 
      recurseIntoAttrs (super.haskellPackages.override {
        overrides = (se: su: with se; {
          volumectl = (callPackage (import ../volumectl/default.nix) {});
        });
    });
        
  });
in
  newpkgs.xmobar-with-packages

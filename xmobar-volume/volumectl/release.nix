let
  pkgs = import <nixpkgs> {};
  newpkgs = pkgs.overridePackages (self: super: with self; {

    haskellPackages = 
      recurseIntoAttrs (super.haskellPackages.override { 
        overrides = se: su: with se; {
          volumectl = (callPackage (import ./default.nix) {});          
        }; 
    });
 
  }); 
in 
  newpkgs.haskellPackages.volumectl

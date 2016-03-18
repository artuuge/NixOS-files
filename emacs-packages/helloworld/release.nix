let
  pkgs = import <nixpkgs> {};
  newpkgs = pkgs.overridePackages (self: super: with self; {

    emacsPackagesNg = super.emacsPackagesNg.override (su: se: with se; {
      helloworld = (callPackage (import ./default.nix) {}); 
    });
  
  });
in
  newpkgs.emacsPackagesNg.helloworld

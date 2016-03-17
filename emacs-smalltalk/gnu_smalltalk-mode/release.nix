let
  pkgs = import <nixpkgs> {};
  newpkgs = pkgs.overridePackages (self: super: with self; {

    emacsPackagesNg = super.emacsPackagesNg.override (su: se: with se; {
      gnu_smalltalk-mode = (callPackage (import ./default.nix) {}); 
    });
  
  });
in
  newpkgs.emacsPackagesNg.gnu_smalltalk-mode

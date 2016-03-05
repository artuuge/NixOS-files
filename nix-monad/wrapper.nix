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

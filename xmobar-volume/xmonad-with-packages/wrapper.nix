{ stdenv, makeWrapper, packages ? self: [], 
  gxmessage, dmenu, gmrun, xfce4-terminal, setxkbmap,  
  ghcWithPackages, xmonad, xmonad-contrib, xmobar-with-packages, volume-pulse }:

let
  xmonadEnv = ghcWithPackages (self: with self; [ xmonad xmonad-contrib ] ++ packages self);
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
      --set NIX_MONAD_XFCE4_TERMINAL "${xfce4-terminal}/bin/xfce4-terminal" \
      --set NIX_MONAD_XMOBAR "${xmobar-with-packages}/bin/xmobar" \
      --set NIX_MONAD_SETXKBMAP "${setxkbmap}/bin/setxkbmap" \
      --set NIX_MONAD_VOLUME_PULSE "${volume-pulse}/bin/volume_pulse"
  '';

  # trivial derivation
  preferLocalBuild = true;
  allowSubstitutes = false;
}

{stdenv, xmobar, volumectl}:

stdenv.mkDerivation {
  name = "xmobar-with-packages";
  
  buildCommand = ''
    mkdir -p $out/bin
    ln -s "${xmobar}/bin/xmobar" $out/bin/xmobar
    ln -s "${volumectl}/bin/volumectl" $out/bin/volumectl
  '';
  
}

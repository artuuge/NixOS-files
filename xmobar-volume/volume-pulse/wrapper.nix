{stdenv, makeWrapper, volumectl, pulseaudio}:

stdenv.mkDerivation {
  name = "volume-pulse";
  nativeBuildInputs = [ makeWrapper ]; 

  buildCommand = ''
    makeWrapper "${volumectl}/bin/volumectl" $out/bin/volume_pulse \
      --set NIX_PACTL "${pulseaudio}/bin/pactl"
  ''; 
  
}

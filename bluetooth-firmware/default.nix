{ stdenv }:
stdenv.mkDerivation {
  name = "myBluetooth";
  src = ./.; 
  
  buildCommand = '' 
    source $stdenv/setup
    mkdir -p $out/lib/firmware/brcm
    cp $src/broadcom-abcd-0123.hcd $out/lib/firmware/brcm/BCM.hcd
  ''; 

}

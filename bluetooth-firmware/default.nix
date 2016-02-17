{ stdenv }:
stdenv.mkDerivation {
  name = "myBluetooth";
  src = ./.; 
  builder = ./builder.sh; 
}

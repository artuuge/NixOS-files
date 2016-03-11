{ mkDerivation, attoparsec, base, directory, filepath, stdenv, text
, unix, utf8-string
}:
mkDerivation {
  pname = "volumectl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base directory filepath text unix utf8-string
  ];
  license = stdenv.lib.licenses.gpl3;
}

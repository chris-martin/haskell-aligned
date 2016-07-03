{ mkDerivation, base, stdenv, transformers }:
mkDerivation {
  pname = "aligned";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base transformers ];
  license = stdenv.lib.licenses.asl20;
}

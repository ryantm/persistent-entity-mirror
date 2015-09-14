{ stdenv, mkDerivation, base, ghc, stack, hspec, mysql-simple
}:

mkDerivation {
  pname = "persistent-entity-mirror";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  buildTools = [ ghc stack ];
  buildDepends = [
    base
  ];
  testDepends = [
    base hspec mysql-simple
  ];
  homepage = "https://github.com/ryantm/persistent-entity-mirror";
  description = "In progress: a tool for generating persistent entities from an existing database.";
  license = stdenv.lib.licenses.publicDomain;
}

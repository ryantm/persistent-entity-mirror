{ stdenv, mkDerivation, base, ghc, stack, cabal-install, hspec, mysql-simple
}:

mkDerivation {
  pname = "persistent-entity-mirror";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  buildTools = [ ghc stack cabal-install ];
  buildDepends = [
    base mysql-simple
  ];
  testDepends = [
    base hspec mysql-simple
  ];
  homepage = "https://github.com/ryantm/persistent-entity-mirror";
  description = "In progress: a tool for generating persistent entities from an existing database.";
  license = stdenv.lib.licenses.publicDomain;
}

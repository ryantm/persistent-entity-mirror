{ stdenv, mkDerivation, base, ghc, stack, cabal-install, hspec, mysql-simple
, persistent-template, persistent-mysql, persistent
}:

mkDerivation {
  pname = "persistent-entity-mirror";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  buildTools = [ ghc stack cabal-install ];
  buildDepends = [
    base hspec mysql-simple persistent persistent-template persistent-mysql
  ];
  testDepends = [
    base hspec mysql-simple persistent persistent-template persistent-mysql
  ];
  homepage = "https://github.com/ryantm/persistent-entity-mirror";
  description = "In progress: a tool for generating persistent entities from an existing database.";
  license = stdenv.lib.licenses.publicDomain;
}

{ mkDerivation, base, bencoding, bytestring, containers, cryptohash
, hspec, hspec-expectations, HUnit, mtl, network, QuickCheck
, random, split, stdenv, time
}:
mkDerivation {
  pname = "gootea";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base bencoding bytestring containers cryptohash mtl network random
    time
  ];
  executableHaskellDepends = [
    base bencoding bytestring network split
  ];
  testHaskellDepends = [
    base bencoding bytestring containers hspec hspec-expectations HUnit
    mtl network QuickCheck random time
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/gootea/gootea";
  license = stdenv.lib.licenses.bsd3;
}

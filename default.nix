{ mkDerivation, base, hpack, optparse-applicative, process, stdenv
, text
}:
mkDerivation {
  pname = "opdt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base optparse-applicative process text ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base optparse-applicative process text
  ];
  testHaskellDepends = [ base optparse-applicative process text ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/opdt#readme";
  license = stdenv.lib.licenses.bsd3;
}

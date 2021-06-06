{ mkDerivation, aeson, aeson-yaml, base, bytestring, containers
, dhall, directory, filepath, hpack, lib, optparse-applicative
, process, raw-strings-qq, string-interpolate, text
}:
mkDerivation {
  pname = "opdt";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-yaml base bytestring containers dhall directory
    filepath optparse-applicative process raw-strings-qq
    string-interpolate text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-yaml base bytestring containers dhall directory
    filepath optparse-applicative process raw-strings-qq
    string-interpolate text
  ];
  testHaskellDepends = [
    aeson aeson-yaml base bytestring containers dhall directory
    filepath optparse-applicative process raw-strings-qq
    string-interpolate text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/opdt#readme";
  license = lib.licenses.bsd3;
}

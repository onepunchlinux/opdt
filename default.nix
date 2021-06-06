{ compiler ? "ghc8104" }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = new: old: rec {
          opdt = with pkgs.haskell.lib; dontCheck (dontHaddock (new.callPackage ./nix/opdt.nix {}));
        };
      };

      shell = pkgs.mkShell {
        buildInputs = with pkgs.haskellPackages; [ cabal-install haskell-language-server ];
        inputsFrom = [ haskellPackages.opdt.env ];
      };
    };
  };
  pkgs = import (fetchGit (import ./version.nix)) { inherit config; };

in {
  opdt = pkgs.haskellPackages.opdt;
  shell = pkgs.shell;
}

{ nixpkgs  ? import <nixpkgs> {}
, compiler ? "ghc7101"
}:

let
  pkgs = nixpkgs.pkgs;
  ghc  = pkgs.haskell.packages.${compiler};
  f    = import <pwd>;
  drv  = ghc.callPackage f {};
in
  (pkgs.haskell.lib.addBuildTools drv [
    pkgs.git
    ghc.cabal-install
    ghc.hlint
  ]).env

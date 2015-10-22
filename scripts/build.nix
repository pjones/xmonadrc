{ nixpkgs  ? import <nixpkgs> {}
, compiler ? "ghc7102"
}:

let
  pkgs = nixpkgs.pkgs;
  ghc  = pkgs.haskell.packages.${compiler};
  f    = import <pwd>;
  drv  = ghc.callPackage f {};
in
  (pkgs.haskell.lib.addBuildTools drv [
    pkgs.git
    pkgs.pkgconfig
    pkgs.gtk2
    ghc.cabal-install
    ghc.hlint
    ghc.gtk
  ]).env

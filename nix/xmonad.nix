# Pull in xmonad from my mirror:
{ pkgs ? import <nixpkgs> { }
}:

with pkgs.lib;

let
  src = pkgs.fetchgit (removeAttrs (importJSON ./xmonad.json) ["date"]);
  haskell = pkgs.haskellPackages;
  withSrc = args: haskell.mkDerivation (args // { inherit src; });
in

haskell.callPackage ./xmonad.cabal.nix { mkDerivation = withSrc; }

# Pull in xmonad-contrib from my mirror:
{ pkgs ? import <nixpkgs> { }
}:

with pkgs.lib;

let
  src = pkgs.fetchgit (removeAttrs (importJSON ./xmonad-contrib.json) ["date"]);
  haskell = pkgs.haskellPackages;
  withSrc = args: haskell.mkDerivation (args // { inherit src; });
in

haskell.callPackage ./xmonad-contrib.cabal.nix { mkDerivation = withSrc; }

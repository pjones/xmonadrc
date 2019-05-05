# Pull in xmonad-contrib from my mirror:
{ pkgs
, haskell
}:

with pkgs.lib;

let
  src = pkgs.fetchgit (removeAttrs (importJSON ./xmonad-contrib.json) ["date"]);
  withSrc = args: haskell.mkDerivation (args // { inherit src; });
in

haskell.callPackage ./xmonad-contrib.cabal.nix { mkDerivation = withSrc; }

# Pull in xmonad from my mirror:
{ pkgs
, haskell
}:

with pkgs.lib;

let
  src = pkgs.fetchgit (removeAttrs (importJSON ./xmonad.json) ["date"]);
  withSrc = args: haskell.mkDerivation (args // { inherit src; });
in

haskell.callPackage ./xmonad.cabal.nix { mkDerivation = withSrc; }

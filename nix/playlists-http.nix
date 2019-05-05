{ pkgs
, haskell
}:

with pkgs.lib;

let
  src = pkgs.fetchgit (removeAttrs (importJSON ./playlists-http.json) ["date"]);
  withSrc = args: haskell.mkDerivation (args // { inherit src; });
in

haskell.callPackage "${src}/playlists-http.nix" { mkDerivation = withSrc; }

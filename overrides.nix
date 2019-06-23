{ pkgs ? import <nixpkgs> {}
}:

with pkgs.lib;

let
  # Replace the source attribute so it points at the source found in
  # the given JSON file:
  replaceSrc = haskell: nix: json:
    let src = pkgs.fetchgit (removeAttrs (importJSON json) ["date"]);
        mkD = args: haskell.mkDerivation (args // { inherit src; });
    in haskell.callPackage nix { mkDerivation = mkD; };

  # Use my copy of some packages:
  overrides = self: super: with pkgs.haskell.lib; {
    playlists = import ./nix/playlists.nix { inherit pkgs; haskell = self; };
    playlists-http = import ./nix/playlists-http.nix { inherit pkgs; haskell = self; };

    xmonad = replaceSrc self ./nix/xmonad.cabal.nix ./nix/xmonad.json;
    xmonad-contrib = replaceSrc self ./nix/xmonad-contrib.cabal.nix ./nix/xmonad-contrib.json;

    http-client = if super ? http-client_0_6_2
      then super.http-client_0_6_2
      else super.http-client;

    xmonadrc = self.callPackage ./xmonadrc.nix { };
  };

in
  # Apply the overrides from above:
  pkgs.haskellPackages.override (orig: {
    overrides = pkgs.lib.composeExtensions
      (orig.overrides or (_: _: {})) overrides; })

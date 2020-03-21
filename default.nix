{ pkgs ? import <nixpkgs> { }
}:

let
  nix-hs = import (fetchGit {
    url = "https://code.devalot.com/open/nix-hs.git";
    rev = "528e487580c701e310e17347f8cf2a8e2796054d";
  }) { inherit pkgs; };

  # Replace the source attribute so it points at the source found in
  # the given JSON file:
  replaceSrc = haskell: nix: json:
    let src = pkgs.fetchgit (removeAttrs (pkgs.lib.importJSON json) ["date"]);
        mkD = args: haskell.mkDerivation (args // { inherit src; });
    in haskell.callPackage nix { mkDerivation = mkD; };

in nix-hs {
  cabal = ./xmonadrc.cabal;

  overrides = lib: self: super: with lib; rec {
    playlists = import ./nix/playlists.nix { inherit pkgs; haskell = self; };
    playlists-http = import ./nix/playlists-http.nix { inherit pkgs; haskell = self; };

    xmonad = replaceSrc self ./nix/xmonad.cabal.nix ./nix/xmonad.json;
    xmonad-contrib = replaceSrc self ./nix/xmonad-contrib.cabal.nix ./nix/xmonad-contrib.json;

    http-client = if super ? http-client_0_6_2
      then super.http-client_0_6_2
      else super.http-client;
  };
}

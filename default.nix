{ pkgs ? import <nixpkgs> {}
}:

let
  # Use my copy of some packages:
  overrides = self: super: with pkgs.haskell.lib; {
    playlists = import ./nix/playlists.nix { inherit pkgs haskell; };
    playlists-http = import ./nix/playlists-http.nix { inherit pkgs haskell; };

    xmonad = import ./nix/xmonad.nix { inherit pkgs haskell; };
    xmonad-contrib = import ./nix/xmonad-contrib.nix { inherit pkgs haskell; };

    http-client = if super ? http-client_0_6_2
      then super.http-client_0_6_2
      else super.http-client;
  };

  # Apply the overrides from above:
  haskell = pkgs.haskellPackages.override (orig: {
    overrides = pkgs.lib.composeExtensions
      (orig.overrides or (_: _: {})) overrides; });

in haskell.callPackage ./xmonadrc.nix { }

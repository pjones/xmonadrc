{ pkgs ? import <nixpkgs> {}
}:

let
  # Use my copy of some packages:
  overrides = self: super: with pkgs.haskell.lib; {
    xmonad = import ./nix/xmonad.nix { inherit pkgs; };
    xmonad-contrib = import ./nix/xmonad-contrib.nix { inherit pkgs; };
  };

  # Apply the overrides from above:
  haskell = pkgs.haskellPackages.override (orig: {
    overrides = pkgs.lib.composeExtensions
      (orig.overrides or (_: _: {})) overrides; });

in haskell.callPackage ./xmonadrc.nix { }

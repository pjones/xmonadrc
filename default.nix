{ pkgs ? import <nixpkgs> {}
}:

let haskell = import ./overrides.nix { inherit pkgs; };
in  haskell.xmonadrc

{ pkgs ? (import <nixpkgs> {}).pkgs
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/pjones/nix-hs.git";
    rev = "84a5ca465dd7cd1880317a7f458dde682412b9bb";
  };

  nix-hs = (import "${nix-hs-src}/default.nix" {inherit pkgs;});

in

pkgs.mkShell {
  buildInputs = with pkgs; [
    nix-hs
    haskellPackages.hlint
    haskellPackages.hasktags
    # cabal-dependency-licenses
  ];
}

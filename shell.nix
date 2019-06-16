{ pkgs ? (import <nixpkgs> {}).pkgs
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/pjones/nix-hs.git";
    rev = "4a9ea2c8c6712ae3cb5892bc74dc051906535238";
  };

  nix-hs = (import "${nix-hs-src}/default.nix" {inherit pkgs;});

in

pkgs.mkShell {
  buildInputs = with pkgs; [

    # Haskell Dependencies:
    haskellPackages.ghc
    haskellPackages.cabal-install

    # For IDEs:
    nix-hs
    haskellPackages.hoogle
    haskellPackages.hlint
    # haskellPackages.cabal-dependency-licenses
  ];
}

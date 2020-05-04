{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghcide ? sources.ghcide-nix
, ormolu ? sources.ormolu
, ghc ? "default"
}:

nix-hs {
  cabal = ./xmonadrc.cabal;
  compiler = ghc;

  overrides = lib: self: super: with lib; {
    xmonad = super.callCabal2nix "xmonad" sources.xmonad {};
    xmonad-contrib = super.callCabal2nix "xmonad-contrib" sources.xmonad-contrib {};
    playlists = import sources.playlists { inherit (lib) pkgs; };
    playlists-http = import sources.playlists-http { inherit (lib) pkgs; };

    ghcide = import ghcide { ghc = compilerName; };
    ormolu = (import ormolu {
      inherit (lib) pkgs;
      ormoluCompiler = lib.compilerName;
    }).ormolu;
  };
}

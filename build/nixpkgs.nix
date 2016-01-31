with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "xmonadrc";

  buildInputs = [
    # GHC:
    haskell.packages.lts-4_2.ghc

    # Non-Haskell Dependencies:
    libX11
    libXext
    libXinerama
    libXrandr
    libXrender
  ];

  # # Work around a bug in GHC:
  # # https://ghc.haskell.org/trac/ghc/ticket/11042
  # shellHook = ''
  #   export LD_LIBRARY_PATH=${zlib}/lib
  # '';
}

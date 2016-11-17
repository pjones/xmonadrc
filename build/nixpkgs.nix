with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "xmonadrc";

  buildInputs = [
    # GHC:
    haskell.packages.lts-4_2.ghc

    # Non-Haskell Dependencies:
    pkgconfig
    xorg.libX11
    xorg.libXext
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXrender
    xorg.libXft
    libxml2
    gtk2
    pango
    cairo
    glib
  ];
}

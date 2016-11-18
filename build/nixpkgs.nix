with (import <nixpkgs> {});

let ghc = haskell.packages.ghc7103.ghcWithPackages (p: with p; [
            cabal-install
            glib
            gtk
          ]);
in
stdenv.mkDerivation {
  name = "xmonadrc";

  buildInputs = [
    # GHC and some Haskell packages:
    ghc

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

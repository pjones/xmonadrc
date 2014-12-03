# This is currently a Nix expression for building with cabal but I
# plan on changing it so it just builds with GNU Make.  That way all
# of my sandboxing will work correctly and my xmonad executable can go
# into the nix store.
{ pkgs ? (import <nixpkgs> {}) }:

let haskellPackages = pkgs.haskellPackages; in

haskellPackages.cabal.mkDerivation (self: {
  pname = "xmonadrc";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;

  buildDepends = with pkgs; [
    # Basic Haskell tools needed to build this project:
    haskellPackages.ghc haskellPackages.cabalInstall
    haskellPackages.alex haskellPackages.happy

    # Xorg libraries for building xmonad (X11 package):
    xlibs.libX11 xlibs.libXext xlibs.libXinerama
    xlibs.libXrandr xlibs.libXrender

    # Packages needed for building taffybar:
    pkgconfig # zlib

    # Packages for xmonad-extras and my xmonadrc:
    mpd_clientlib
  ];

  pkgconfigDepends = with pkgs; [
    # Needed for building taffybar.
    cairo glib gtk libxml2
  ];

  meta = {
    homepage = "http://www.pmade.com";
    description = "Peter's XMonad Configuration";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})

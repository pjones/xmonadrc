{ mkDerivation, base, bytestring, containers, directory
, extensible-exceptions, filepath, mtl, old-locale, old-time
, process, random, semigroups, stdenv, unix, utf8-string, X11
, X11-xft, xmonad
}:
mkDerivation {
  pname = "xmonad-contrib";
  version = "0.15";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers directory extensible-exceptions filepath
    mtl old-locale old-time process random semigroups unix utf8-string
    X11 X11-xft xmonad
  ];
  homepage = "http://xmonad.org/";
  description = "Third party extensions for xmonad";
  license = stdenv.lib.licenses.bsd3;
}

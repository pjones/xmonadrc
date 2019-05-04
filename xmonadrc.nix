{ mkDerivation, base, bytestring, containers, directory, either
, filepath, hostname, http-client, libmpd, MonadRandom, mtl, parsec
, playlists, playlists-http, setlocale, stdenv, text, X11, xmonad
, xmonad-contrib
}:
mkDerivation {
  pname = "xmonadrc";
  version = "19.5.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory either filepath hostname
    http-client libmpd MonadRandom mtl parsec playlists playlists-http
    setlocale text X11 xmonad xmonad-contrib
  ];
  executableHaskellDepends = [
    base bytestring containers directory either filepath hostname
    http-client libmpd MonadRandom mtl parsec playlists playlists-http
    setlocale text X11 xmonad xmonad-contrib
  ];
  homepage = "https://github.com/pjones/xmonadrc";
  description = "Peter's XMonad Configuration";
  license = stdenv.lib.licenses.bsd3;
}

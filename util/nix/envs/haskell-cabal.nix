{ nixpkgs   ? import <nixpkgs> {}
, compiler  ? null
, profiling ? false
}:

let
  pkgs = nixpkgs.pkgs;

  # Use the default Haskell compiler if one isn't specified on the
  # command line.
  baseghc =
    if compiler != null
      then pkgs.haskell.packages.${compiler}
      else pkgs.haskellPackages;

  # Function to optionally enable profiling.
  profilingOverride = super:
    if profiling
      then { mkDerivation = args: super.mkDerivation
               (args // { enableLibraryProfiling = true; }); }
      else { };

  # Haskell compiler with overrides in place.
  ghc = baseghc.override {
    overrides = self: super: profilingOverride super;
  };

  # Load the generated "default.nix" from the specified directory.
  f = import <pwd>;

  # Make the actual derivation to build the Haskell project.
  drv = ghc.callPackage f {};
in
  (pkgs.haskell.lib.addBuildTools drv [
    # Additional tools to make available:
    pkgs.git
    ghc.cabal-install
    ghc.hlint
  ]).env

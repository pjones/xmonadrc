# Helpful Utilities for Haskell Programming

## Utilities

  * `hsbuild`: Runs `cabal` to build a project inside a sandbox

## NixOS/Nixpkgs

### Utilities

  * `nix-hs-derivation`: Uses `cabal2nix` to generate a `default.nix` file

  * `nix-hs-shell`: Starts a shell with Haskell tools installed after
    using `nix-hs-derivation` to create a `default.nix` file

  * `nix-hs-build`: Uses `nix-hs-shell` and `hsbuild` to build a
    Haskell project

### Options

The `nix-hs-shell` and `nix-hs-build` utilities can be given an
argument to select the appropriate version of GHC.  For example:

    nix-hs-build --argstr compiler ghc784

### Nix Expressions

  * `haskell-cabal.nix`: Allows setting the compiler version and
    injecting extra packages into a Haskell build.  Used by
    `nix-hs-shell` and `nix-hs-build`.

#!/bin/sh -eu

################################################################################
#
# Execute my custom Nix environment for building Haskell projects.
#
################################################################################

# Make sure dependencies are installed:
scripts/dependencies.sh

# Build everything:
derivation=$PWD/scripts/build.nix
builder=$PWD/util/bin/hsbuild

util/nix/bin/nix-hs-derivation
nix-shell -I pwd=$PWD --pure --command $builder $derivation

# Install/restart:
scripts/install.sh "$@"

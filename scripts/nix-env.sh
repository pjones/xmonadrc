#!/bin/sh -eu

################################################################################
#
# Execute my custom Nix environment for building Haskell projects.
#
################################################################################

# Make sure dependencies are installed:
scripts/dependencies.sh

# Build everything:
override=scripts/build.nix
# FIXME: need to call out to nix-hs-build

# Install/restart:
scripts/install.sh "$@"

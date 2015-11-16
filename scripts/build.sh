#!/bin/sh -eu

################################################################################
#
# Custom build script.
#
################################################################################
if [ -r ~/.nix-profile ]; then
  exec `dirname $0`/nix-env.sh
else
  exec `dirname $0`/../util/bin/hsbuild
fi

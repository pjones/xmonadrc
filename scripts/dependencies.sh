#!/bin/sh -eu

################################################################################
# This is a helper script that will prepare any necessary dependencies.
VENDOR_DIR=vendor

################################################################################
# Get the source code for a dependency.
#
# $1: The directory (under VENDOR_DIR) where the dependency goes.
# $@: Everything else is the command to run to fetch the dependency.
get_src () {
  mkdir -p $VENDOR_DIR
  dir=$1; shift

  if [ ! -d $VENDOR_DIR/$dir ]; then
    (cd $VENDOR_DIR && "$@")
  fi
}

################################################################################
# When using xmonad from darcs.
get_xmonad_src () {
  url="pjones@dracula.pmade.com:darcs/oss/xmonad"
  get_src xmonad darcs get --lazy "$url"
}

################################################################################
# When using xmonad-contrib from darcs.
get_xmonad_contrib_src () {
  url="pjones@dracula.pmade.com:darcs/oss/XMonadContrib"
  get_src xmonad-contrib darcs get --lazy "$url" xmonad-contrib
}

################################################################################
# I'm currently using a custom version of taffybar.
get_taffybar_src () {
  repo=https://github.com/pjones/taffybar.git
  get_src taffybar git clone -b feature/mpris-config $repo
}

################################################################################
# Install dependencies and build.
get_xmonad_src
get_xmonad_contrib_src
get_taffybar_src

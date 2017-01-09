#!/bin/sh -eu

################################################################################
# Configure Git remote repositories.

################################################################################
set_remote() {
  name=$1
  url=$2

  if git remote get-url "$name" > /dev/null 2>&1; then
    git remote set-url "$name" "$url"
  else
    git remote add "$name" "$url"
  fi
}

################################################################################
# xmonad
( cd vendor/xmonad
  set_remote github   https://github.com/pjones/xmonad.git
  set_remote upstream https://github.com/xmonad/xmonad.git
)

################################################################################
# xmonad-contrib
( cd vendor/xmonad-contrib
  set_remote github   https://github.com/pjones/xmonad-contrib.git
  set_remote upstream https://github.com/xmonad/xmonad-contrib.git
)

################################################################################
# x11
( cd vendor/x11
  set_remote upstream https://github.com/xmonad/X11.git
)

#!/bin/sh

################################################################################
#
# Helper script to run taffybarrc with the correct args.
TAFFYBAR=$HOME/.xmonad/taffybar

################################################################################
if [ "$1" = restart ]; then
  killall --wait taffybar
fi

################################################################################
cd $HOME
exec $TAFFYBAR +RTS -I0 -V0

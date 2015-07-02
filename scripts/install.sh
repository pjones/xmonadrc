#!/bin/sh -eu

################################################################################
# Helper script to install a custom xmonad in the correct location.
ARCH=`uname -m`
OS=`uname -s | tr '[A-Z]' '[a-z]'`
XMONAD_BIN=${HOME}/.xmonad/xmonad-${ARCH}-${OS}
XMONAD_SRC=dist/build/xmonadrc/xmonadrc
CHECK_BIN=dist/build/checkrc/checkrc
TAFFYBAR_SRC=dist/build/taffybarrc/taffybarrc
TAFFYBAR_WRAPPER=scripts/taffybar.sh

################################################################################
BIN_DIR=$HOME/bin

################################################################################
do_install () {
  $CHECK_BIN

  mkdir -p $BIN_DIR `dirname $XMONAD_BIN` $HOME/.config/taffybar
  if [ -r $XMONAD_BIN ]; then mv $XMONAD_BIN ${XMONAD_BIN}.prev; fi

  install -m 0755 $XMONAD_SRC       $XMONAD_BIN
  install -m 0755 $XMONAD_SRC       $BIN_DIR/xmonad
  install -m 0755 $TAFFYBAR_SRC     $BIN_DIR/taffybarrc
  install -m 0755 $TAFFYBAR_WRAPPER $BIN_DIR/`basename $TAFFYBAR_WRAPPER`
  install -m 0644 etc/taffybar.gtk  $HOME/.config/taffybar/taffybar.rc

  cd `dirname $XMONAD_BIN` && ln -nfs `basename $XMONAD_BIN` xmonadrc
}

################################################################################
do_restart () {
  $XMONAD_BIN --restart
  $BIN_DIR/`basename $TAFFYBAR_WRAPPER` restart > /dev/null 2>&1 &
}

################################################################################
case ${1:-install} in
  install)
    do_install
    ;;

  restart)
    do_install
    do_restart
    ;;

  *)
    echo "Usage: $0 [install | restart]"
    exit 1
    ;;
esac

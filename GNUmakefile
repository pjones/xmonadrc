################################################################################
.PHONEY: all install restart

################################################################################
# Set up the default target.
all::

################################################################################
# Ask `git' to update the submodule and make haskell.mk available.
util/haskell.mk:
	git submodule update --init

################################################################################
# Settings for `haskell.mk'.
CABAL_FLAGS =
include util/haskell.mk

################################################################################
ARCH = $(shell uname -m)
OS   = $(shell uname -s | tr '[A-Z]' '[a-z]')
BIN  = $(HOME)/.xmonad/xmonad-$(ARCH)-$(OS)

################################################################################
DEST              = $(HOME)/bin
XMONAD            = .cabal-sandbox/bin/xmonad
XMONADRC          = dist/build/xmonadrc/xmonadrc
TAFFYBAR          = taffybar.sh
TAFFYBARRC        = dist/build/taffybarrc/taffybarrc
DO_CHECK         ?= YES

################################################################################
ifeq ($(DO_CHECK),YES)
  CHECK = dist/build/checkrc/checkrc
else
  CHECK = :
endif

################################################################################
install: $(TARGETS)
	$(CHECK)
	mkdir -p $(DEST) $(dir $(BIN)) $(HOME)/.config/taffybar
	if [ -r $(BIN) ]; then mv $(BIN) $(BIN).prev; fi
	cp -p $(XMONADRC) $(BIN)
	cd $(dir $(BIN)) && ln -nfs $(notdir $(BIN)) xmonadrc
	cp -f $(XMONADRC) $(DEST)/xmonad
	cp -f $(TAFFYBARRC) $(DEST)
	cp -f $(TAFFYBAR) $(DEST)
	cp -f taffybar.gtk $(HOME)/.config/taffybar/taffybar.rc

################################################################################
restart: install
	$(XMONAD) --restart
	$(DEST)/$(TAFFYBAR) restart > /dev/null 2>&1 &

################################################################################
ARCH              = $(shell uname -m)
OS                = $(shell uname -s | tr '[A-Z]' '[a-z]')
TARGET            = $(HOME)/.xmonad/xmonad-$(ARCH)-$(OS)

################################################################################
SRC               = $(shell find . -type f -name '*.hs')
SRC              += xmonadrc.cabal

################################################################################
DEST              = $(HOME)/bin
SANDBOX           = cabal.sandbox.config
XMONAD            = .cabal-sandbox/bin/xmonad
XMONADRC          = dist/build/xmonadrc/xmonadrc
TAFFYBAR          = taffybar.sh
TAFFYBARRC        = dist/build/taffybarrc/taffybarrc
CABAL_FLAGS       = --enable-optimization=2
CABAL_ADD_SOURCE ?=
DO_CHECK         ?= YES

################################################################################
.PHONEY: all install restart clean realclean

################################################################################
all: $(XMONADRC)

################################################################################
install: $(TARGET)
	mkdir -p $(DEST) $(HOME)/.config/taffybar
	cp -f $(XMONADRC) $(DEST)/xmonad
	cp -f $(TAFFYBARRC) $(DEST)
	cp -f $(TAFFYBAR) $(DEST)
	cp -f taffybar.gtk $(HOME)/.config/taffybar/taffybar.rc

################################################################################
restart: install
	$(XMONAD) --restart
	$(DEST)/$(TAFFYBAR) restart > /dev/null 2>&1 &

################################################################################
clean:
	rm -rf dist $(XMONADRC) $(CHECK) $(SANDBOX)

################################################################################
realclean:
	rm -rf .cabal-sandbox

################################################################################
ifeq ($(DO_CHECK),YES)
  CHECK = dist/build/checkrc/checkrc
else
  CHECK = :
endif

################################################################################
$(XMONADRC): $(SRC) $(SANDBOX)
	ghc -V | grep -q 7.8.3 # Required compiler version.
	cabal build
	$(CHECK)

################################################################################
$(SANDBOX):
	cabal sandbox init
	$(if $(CABAL_ADD_SOURCE),cabal sandbox add-source $(CABAL_ADD_SOURCE),)
	cabal install gtk2hs-buildtools
	cabal install $(CABAL_FLAGS) xmonad-extras -fwith_mpd -f-with_hint
	cabal install --only-dependencies $(CABAL_FLAGS)
	cabal configure $(CABAL_FLAGS)
	touch $@

################################################################################
$(TARGET): $(XMONADRC)
	mkdir -p $(dir $@)
	if [ -r $@ ]; then mv $@ $@.prev; fi
	cp -p $< $@
	cd $(dir $@) && ln -nfs $(notdir $@) xmonadrc

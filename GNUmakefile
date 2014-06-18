################################################################################
ARCH     = $(shell uname -m)
OS       = $(shell uname -s | tr '[A-Z]' '[a-z]')
TARGET   = $(HOME)/.xmonad/xmonad-$(ARCH)-$(OS)
SRC      = $(shell find . -type f -name '*.hs')
SANDBOX  = cabal.sandbox.config
SANDBOXD = .cabal-sandbox
BIN      = $(SANDBOXD)/bin
XMONAD   = $(HOME)/bin/xmonad
XMONADRC = $(BIN)/xmonadrc
DO_CHECK ?= YES

################################################################################
.PHONEY: all install restart clean realclean

################################################################################
all: $(XMONADRC)

################################################################################
install: $(TARGET)
	mkdir -p $(dir $(XMONAD))
	cp -r $(BIN)/xmonad $(XMONAD)

################################################################################
restart: install
	$(XMONAD) --restart

################################################################################
clean:
	rm -rf dist $(XMONADRC) $(CHECK)

################################################################################
realclean:
	rm -rf $(SANDBOXD) $(SANDBOX)

################################################################################
ifeq ($(DO_CHECK),YES)
  CHECK = $(BIN)/checkrc
else
  CHECK = :
endif

$(XMONADRC): $(SRC) $(SANDBOX)
	ghc -V | grep -q 7.6.3 # Required compiler version.
	cabal install
	$(CHECK)

################################################################################
$(SANDBOX):
	cabal sandbox init

################################################################################
$(TARGET): $(XMONADRC)
	mkdir -p $(dir $@)
	if [ -r $@ ]; then mv $@ $@.prev; fi
	cp -p $< $@
	cd $(dir $@) && ln -nfs $(notdir $@) xmonadrc

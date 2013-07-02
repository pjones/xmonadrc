################################################################################
ARCH     = $(shell uname -m)
OS       = $(shell uname -s | tr '[A-Z]' '[a-z]')
TARGET   = $(HOME)/.xmonad/xmonad-$(ARCH)-$(OS)
SRC      = $(shell find . -type f -name '*.hs')
BIN      = cabal-dev/bin
XMONAD   = $(HOME)/bin/xmonad
XMONADRC = $(BIN)/xmonadrc
CHECK    = $(BIN)/checkrc

################################################################################
.PHONEY: install restart clean

################################################################################
all: $(XMONADRC)

################################################################################
install: $(TARGET)
	cp -r $(BIN)/xmonad $(XMONAD)

################################################################################
restart: install
	$(XMONAD) --restart

################################################################################
clean:
	rm -rf dist $(XMONADRC) $(CHECK)

################################################################################
$(XMONADRC): $(SRC)
	cabal-dev install
	$(CHECK)

################################################################################
$(TARGET): $(XMONADRC)
	if [ -r $@ ]; then mv $@ $@.prev; fi
	cp -p $< $@
	cd $(dir $@) && ln -nfs $(notdir $@) xmonadrc

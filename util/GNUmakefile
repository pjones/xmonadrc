################################################################################
BIN_DIR  = $(HOME)/bin
ENVS_DIR = $(HOME)/.nixpkgs/envs

################################################################################
define INSTALL_BIN
all: $(BIN_DIR)/$(notdir $(1))
$(BIN_DIR)/$(notdir $(1)): $(1)
	@ mkdir -p $(BIN_DIR)
	install -m 0755 $$< $$@
endef

################################################################################
define INSTALL_ENVS
all: $(ENVS_DIR)/$(notdir $(1))
$(ENVS_DIR)/$(notdir $(1)): $(1)
	@ mkdir -p $(ENVS_DIR)
	install -m 0644 $$< $$@
endef

################################################################################
# Install everything in the `bin' directory.
$(foreach f,$(wildcard bin/*),$(eval $(call INSTALL_BIN,$(f))))

################################################################################
# Install Nixpkgs stuff if we see Nixpkgs.
ifeq ($(shell type nix-shell > /dev/null || echo "no"),)
$(foreach f,$(wildcard nix/bin/*),$(eval $(call INSTALL_BIN,$(f))))
$(foreach f,$(wildcard nix/envs/*),$(eval $(call INSTALL_ENVS,$(f))))
endif

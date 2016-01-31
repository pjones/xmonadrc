################################################################################
export TMPDIR = $(HOME)/tmp

################################################################################
TMP_DUMMY  = $(TMPDIR)/.dummy
STACK_OPTS = --stack-yaml=build/stack.yaml

################################################################################
all: $(TMP_DUMMY)
	stack $(STACK_OPTS) setup
	stack $(STACK_OPTS) build

################################################################################
$(TMP_DUMMY):
	mkdir -p $(dir $@)
	touch $@
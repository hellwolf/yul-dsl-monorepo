########################################################################################################################
## VARIABLES
########################################################################################################################

# Project Configurations
LINEAR_SMC_VERSION = 2.2.3
LINEAR_SMC_PATH_FILE = 3rd-parties/linear-smc-$(LINEAR_SMC_VERSION).patch

# Output directories
TEST_BUILDDIR ?= build/dist-test
TEST_COVERAGE_BUILDDIR ?= build/dist-coverage
DOCS_BUILDDIR ?= build/dist-docs

# Build options
BUILD_OPTIONS ?=

# Test options
TEST_SHOW_DETAILS_MODE ?= direct # alternatively: always | failure | never
TEST_PROP_NUM_RUNS ?= 1000
TEST_OPTIONS ?= \
    --test-show-details=$(TEST_SHOW_DETAILS_MODE) \
    --test-options="--maximum-generated-tests=$(TEST_PROP_NUM_RUNS)"

# Cabal flavors
CABAL ?= cabal
CABAL_BUILD    = $(CABAL) --builddir=$(TEST_BUILDDIR)
CABAL_TEST     = $(CABAL) --builddir=$(TEST_BUILDDIR)
CABAL_COVERAGE = $(CABAL) --builddir=$(TEST_COVERAGE_BUILDDIR)
CABAL_DOCS     = $(CABAL) --builddir=$(DOCS_BUILDDIR)

# Misc
DEV_TARGETS = build-all-modules test-loliyul

########################################################################################################################
# TARGETS
########################################################################################################################

gen-patch-linear-smc:
	diff -ur -p2 3rd-parties/linear-smc-"$(LINEAR_SMC_VERSION)" 3rd-parties/linear-smc | tee "$(LINEAR_SMC_PATH_FILE)"
	# delete the patch if empty
	[[ -s "$(LINEAR_SMC_PATH_FILE)" ]] & rm -f "$(LINEAR_SMC_PATH_FILE)"

build-all-modules:
	$(CABAL_BUILD) build all $(BUILD_OPTIONS)

build-docs:
	$(CABAL_DOCS) haddock loliyul
	xdg-open $(DOCS_BUILDDIR)/build/*/*/loliyul-*/doc/html/loliyul/index.html

clean:
	rm -rf build

test-loliyul:
	$(CABAL_TEST) test loliyul $(TEST_OPTIONS)

dev:
	nodemon -w pkgs -e "hs cabal" -x "make $(DEV_TARGETS) || exit 1"

.PHONY: gen-* build-* clean test-* dev

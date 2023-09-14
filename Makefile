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
DEV_TARGETS = build-all-modules test-yul-dsl

########################################################################################################################
# TARGETS
########################################################################################################################

all: build test

gen-patch-linear-smc:
	diff -ur -p2 3rd-parties/linear-smc-"$(LINEAR_SMC_VERSION)" 3rd-parties/linear-smc | tee "$(LINEAR_SMC_PATH_FILE)"
	# delete the patch if empty
	[[ -s "$(LINEAR_SMC_PATH_FILE)" ]] & rm -f "$(LINEAR_SMC_PATH_FILE)"

build: build-all-modules build-docs

build-all-modules:
	$(CABAL_BUILD) build all $(BUILD_OPTIONS)

build-docs:
	$(CABAL_DOCS) haddock yul-dsl yul-dsl-linear-smc

build-docs-and-display: build-docs
	xdg-open $(DOCS_BUILDDIR)/build/*/*/yul-dsl-*/doc/html/yul-dsl/index.html
	xdg-open $(DOCS_BUILDDIR)/build/*/*/yul-dsl-linear-smc-*/doc/html/yul-dsl-linear-smc/index.html

clean:
	rm -rf build

test: test-yul-dsl test-yolc-basic

test-yul-dsl:
	$(CABAL_TEST) test yul-dsl $(TEST_OPTIONS)

test-yolc-basic:
	yolc -m yul yolc-examples:Basic
	yolc -m yul yolc-examples:ERC20

dev:
	nodemon -w hs-pkgs -w yolc-examples -e "hs cabal" -x "make $(DEV_TARGETS) || exit 1"

.PHONY: all gen-* build-* clean test test-* dev

LINEAR_SMC_VERSION = 2.2.3
LINEAR_SMC_PATH_FILE = 3rd-parties/linear-smc-$(LINEAR_SMC_VERSION).patch

TEST_SHOW_DETAILS_MODE ?= direct # alternatively: always | failure | never
TEST_PROP_NUM_RUNS ?= 1000

TEST_OPTIONS = \
    --test-show-details=$(TEST_SHOW_DETAILS_MODE) \
    --test-options="--maximum-generated-tests=$(TEST_PROP_NUM_RUNS)"

gen-patch-linear-smc:
	diff -ur -p2 3rd-parties/linear-smc-$(LINEAR_SMC_VERSION) 3rd-parties/linear-smc | tee $(LINEAR_SMC_PATH_FILE)
	# delete the patch if empty
	[[ -s $(LINEAR_SMC_PATH_FILE) ]] & rm -f $(LINEAR_SMC_PATH_FILE)

build-all-modules:
	cabal build all

build-docs:
	cabal haddock loliyul
	xdg-open dist-newstyle/build/*/*/loliyul-*/doc/html/loliyul/index.html

clean:
	cabal clean

test-loliyul:
	cabal test loliyul $(TEST_OPTIONS)

dev:
	nodemon -e "hs cabal" -x "make build-all-modules test-loliyul || exit 1"

.PHONY: gen-* build-* clean test-* dev

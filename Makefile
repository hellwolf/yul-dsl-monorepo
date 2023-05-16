gen-patch-linear-smc-1.0.1:
	diff -ur -p2 3rd-parties/linear-smc-1.0.1 pkgs/linear-smc | tee 3rd-parties/linear-smc-1.0.1.patch

.PHONY: gen-*

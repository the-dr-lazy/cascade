wtest:
ifeq ($(dir),)
	echo "You must specify the package directory for this command."
else
	cd $(dir) && $(MAKE) wtest
endif

wtest-api:
	$(MAKE) wtest dir=cascade-api

clean:
	cabal new-clean
	git clean -Xdf

setup:
	git config core.hooksPath .githooks

.PHONY: wtest wtest-api clean

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

wcabal:
	find . -type f -path "./*/*" -name *.dhall | entr -d dhall-hpack-cabal --package-dhall /_

.PHONY: wtest wtest-api clean setup wcabal

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
	./scripts/cabal

wcabal:
	while sleep 0.3; do \
	  git ls-files -cmo | egrep "\.hs$$|\.dhall$$" | entr -cdr ./scripts/cabal; \
	done

.PHONY: wtest wtest-api clean setup wcabal

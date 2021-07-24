wtest:
ifeq ($(dir),)
	@echo "You must specify the package directory for this command."
else
	@cd $(dir) && $(MAKE) wtest
endif

wtest-api:
	$(MAKE) wtest dir=cascade-api

clean:
	@cabal new-clean
	@git clean -Xdf

setup:
	@git config core.hooksPath .githooks
	@echo "Git hooks have been linked."
	@./scripts/cabal.sh
	@echo "Cabal files regenerated."

wcabal: ## Generate Cabal files from Dhall in watch mode.
	@./scripts/watched-cabal.sh

format: ## Run the code formatter.
	@stylish-haskell -r -i .

help: ## This help message.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help

help: ## print this help
	echo "Usage: make <command>\n"
	grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "%-10s %s\n", $$1, $$2}'

build: ## compiles Elm project to Javascript
	elm make --optimize src/Main.elm --output=assets/main.js

format:  ## format Elm source files
	elm-format --yes src/

.PHONY .SILENT: help build format
.DEFAULT_GOAL := help
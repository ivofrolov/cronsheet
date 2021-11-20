.PHONY .SILENT: help build
.DEFAULT_GOAL := help

help: ## print this help
	# see https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
	echo "Usage: make <command>\n"
	grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "%-10s %s\n", $$1, $$2}'

build: ## compiles Elm project to Javascript assets
	elm make src/Main.elm --output=assets/main.js

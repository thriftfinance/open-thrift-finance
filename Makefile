hoogle: requires_nix_shell
	hoogle server --local --port=8070 > /dev/null &

build: requires_nix_shell
	cabal v2-build $(GHC_FLAGS)

watch: requires_nix_shell
	while sleep 1; do find src thrift-finance.cabal | entr -cd make build; done

test: requires_nix_shell
	cabal v2-test

# Useful for making sure the latest version of the scripts is serialised
offchain_test: requires_nix_shell
	env -C onchain nix run ..#serialise-scripts
	env -C offchain cabal v2-test

accept_pirs: requires_nix_shell
	stack build --test $(STACK_FLAGS) $(GHC_FLAGS) --ta '-p MarketAction --accept'

ghci: requires_nix_shell
	cabal v2-repl $(GHC_FLAGS) thrift-finance

# Run fourmolu formatter
format: requires_nix_shell
	env -C onchain fourmolu --mode inplace --check-idempotence -e $(shell env -C onchain fd -ehs)
	env -C offchain fourmolu --mode inplace --check-idempotence -e $(shell env -C offchain fd -ehs)
	nixpkgs-fmt $(NIX_SOURCES)
	cabal-fmt -i $(CABAL_SOURCES)

# Check formatting (without making changes)
format_check:
	env -C onchain fourmolu --stdin-input-file . --mode check --check-idempotence -e $(shell env -C onchain fd -ehs)
	env -C offchain fourmolu --stdin-input-file . --mode check --check-idempotence -e $(shell env -C offchain fd -ehs)
	nixpkgs-fmt --check $(NIX_SOURCES)
	cabal-fmt -c $(CABAL_SOURCES)

# Execute CI
ci: 
	nix-build ./nix/ci.nix

NIX_SHELL = nix develop
HLS_SHELL = $(NIX_SHELL) -c nix-shell -p bashInteractive haskell-language-server

shell:
	$(NIX_SHELL)

hls_shell:
	$(HLS_SHELL)

code:
	$(HLS_SHELL) --run "code ."

# Nix files to format
NIX_SOURCES := $(shell fd -enix)
CABAL_SOURCES := $(shell fd -ecabal)

# Apply hlint suggestions
lint: requires_nix_shell
	find -name '*.hs' -not -path './dist-*/*' -exec hlint --refactor --refactor-options="--inplace" {} \;

# Check hlint suggestions
lint_check: requires_nix_shell
	hlint $(shell fd -ehs)

readme_contents:
	echo "this command is not nix-ified, you may receive an error from npx"
	npx markdown-toc ./README.md --no-firsth1

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)


PLUTUS_BRANCH = $(shell jq '.plutus.branch' ./nix/sources.json )
PLUTUS_REPO = $(shell jq '.plutus.owner + "/" + .plutus.repo' ./nix/sources.json )
PLUTUS_REV = $(shell jq '.plutus.rev' ./nix/sources.json )
PLUTUS_SHA256 = $(shell jq '.plutus.sha256' ./nix/sources.json )

update_plutus:
	@echo "Updating plutus version to latest commit at $(PLUTUS_REPO) $(PLUTUS_BRANCH)"
	niv update plutus
	@echo "Latest commit: $(PLUTUS_REV)"
	@echo "Sha256: $(PLUTUS_SHA256)"
	@echo "Make sure to update the plutus rev in stack.yaml with:"
	@echo "    commit: $(PLUTUS_REV)"
	@echo "This may require further resolution of dependency versions."

################################################################################
# Utils

build_path = dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/thrift-finance-0.1
clear_build:
	@[ ! -e $(build_path) ] || rm -rf $(build_path)

################################################################################
# Docs

DIAGRAMS := docs/eutxo-design
DOT_INPUTS := $(wildcard $(DIAGRAMS)/*.dot )
DOT_SVGS := $(patsubst %.dot, %.svg, $(DOT_INPUTS))
DOT_PNGS := $(patsubst %.dot, %.png, $(DOT_INPUTS))


diagram_pngs: $(DOT_PNGS)
diagrams: $(DOT_SVGS)

clean_diagrams:
	rm $(DOT_SVGS)
	rm $(DOT_PNGS)

# This doesn't work for now, some issue with resvg not loading fonts
%.png: %.svg
	convert $< $@

%.svg: %.dot
	dot -Tsvg $< -o $@


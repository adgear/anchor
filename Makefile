CACHEGRIND=qcachegrind
ELVIS=./bin/elvis
REBAR3 ?= ./rebar3

all: compile

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR3) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR3) as compile compile

coveralls:
	@echo "Running rebar3 coveralls send..."
	@$(REBAR3) as test coveralls send

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR3) dialyzer

edoc:
	@echo "Running rebar3 edoc..."
	@$(REBAR3) as edoc edoc

elvis:
	@echo "Running elvis rock..."
	@$(ELVIS) rock

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR3) do eunit -cv

profile:
	@echo "Profiling..."
	@$(REBAR3) as test compile
	@erl -noshell \
	     -pa _build/test/lib/*/ebin \
	     -pa _build/test/lib/*/test \
	     -eval 'anchor_profile:fprofx()' \
	     -eval 'init:stop()'
	@_build/test/lib/fprofx/erlgrindx -p fprofx.analysis
	@$(CACHEGRIND) fprofx.cgrind

fmt:
	@echo "Running rebar3 fmt..."
	@$(REBAR) fmt --write

fmt-check:
	@echo "Running rebar3 fmt check..."
	@$(REBAR) fmt --check

test: eunit dialyzer

travis: test coveralls

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR3) xref

deps:
	@echo "Running rebar3 get-deps..."
	@$(REBAR3) get-deps

.PHONY: clean compile coveralls dialyzer edoc elvis eunit profile xref fmt fmt-check

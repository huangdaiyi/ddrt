PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar
RELEASE=./release.script

.PHONY: all edoc test clean build_plt dialyzer app release

all:
	@$(REBAR) get-deps compile

release:
	@$(REBAR) get-deps compile
	@rm -rf ddrt
	@$(RELEASE) ddrt

edoc: all
	@$(REBAR) doc

test: 
	@rm -rf .eunit 
	@mkdir -p .eunit 
	@cp -rf priv .eunit/
	@$(REBAR) eunit 

clean:
	@$(REBAR) clean

app:
	@$(REBAR) -r create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

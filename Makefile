PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=rebar

.PHONY: all edoc test clean build_plt dialyzer app

all:
	@$(REBAR) get-deps compile

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

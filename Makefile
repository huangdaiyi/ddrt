PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

RELEASE=./release.script
REBAR=rebar

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
	@$(REBAR) eunit skip_deps=true

clean:
	@$(REBAR) clean

app:
	@$(REBAR) -r create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

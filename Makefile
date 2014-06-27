.PHONY: deps

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

clean:
	@./rebar clean

test: deps compile
	./rebar eunit skip_deps=true
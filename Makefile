

all: compile $(OBJS)

compile: deps
	./rebar compile

deps:
	./rebar get-deps

doc:
	./rebar skip_deps=true doc

xref:
	./rebar skip_deps=true xref

clean:
	./rebar clean

test:
	ERL_FLAGS="-sname eunit" ./rebar skip_deps=true eunit

.PHONY: test



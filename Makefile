all: get-deps compile

get-deps:
	./rebar get-deps
compile:
	./rebar compile
clean:
	./rebar clean
distclean: clean
	rm -rfv deps ebin erl_crash.dump

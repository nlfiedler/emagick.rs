#
# Makefile to invoke cargo with the appropriate arguments depending on the
# operating system. For Darwin, we need to flatten the namespace and suppress
# undefined symbols in order for the linker to succeed.
#

.PHONY: build clean

OS=$(shell uname -o)
ifeq ($(OS),Darwin)
build:
	cargo rustc -- --codegen link-args='-flat_namespace -undefined suppress'
	mkdir -p priv
	cp target/debug/libemagick_rs.dylib priv/libemagick_rs.so
else
build:
	cargo build
endif
	rebar compile

clean:
	rebar clean
	cargo clean

test: build
	rebar ct

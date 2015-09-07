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
	cp target/debug/libemagick_rs.dylib target/debug/libemagick_rs.so
else
build:
	cargo build
endif

clean:
	cargo clean

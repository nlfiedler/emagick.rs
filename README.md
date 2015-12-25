# emagick.rs

A basic (Erlang)[http://www.erlang.org] interface to the (ImageMagick)[http://www.imagemagick.org/] library, via (Rust)[https://www.rust-lang.org] bindings. Currently very few fuctions are exposed, generally only those that are needed for the tanuki project.

## Requirements

* Erlang/OTP R17|R18
* (Rebar)[https://github.com/rebar/rebar]
* Rust (1.3 or higher should work)
* Cargo
* ImageMagick (tested with 6.9)

## Building and Testing

The following should clean and build everything from scratch, including downloading dependencies.

```
$ make clean
$ make test
...
DONE.
Testing projects.emagick.rs: TEST COMPLETE, 1 ok, 0 failed of 1 test cases
```

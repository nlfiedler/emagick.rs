# emagick.rs

A basic [Erlang](http://www.erlang.org) interface to the [ImageMagick](http://www.imagemagick.org) library, via [Rust](https://www.rust-lang.org) bindings. Currently very few functions are exposed, generally only those that are needed for the [tanuki](https://github.com/nlfiedler/tanuki) project.

## Requirements

* Erlang/OTP R17 or higher
* [Rebar3](https://github.com/erlang/rebar3) 3.0.0 or higher
* Rust (1.3 or higher should work)
* Cargo
* ImageMagick 6.9
    - May not work with earlier versions.
    - Does not compile against 7.0 due to incompatible API changes.
    - See the development setup docs in the `magick-rust` project.

## Building and Testing

The following should build and test everything from scratch, including downloading dependencies.

```
$ rebar3 ct
===> Verifying dependencies...
===> Compiling emagick_rs
===> Running Common Test suites...
%%% emagick_rs_SUITE ==> test_image_fit: OK
%%% emagick_rs_SUITE ==> test_image_get_property: OK
%%% emagick_rs_SUITE ==> test_auto_orient: OK
%%% emagick_rs_SUITE ==> test_requires_orientation: OK
All 4 tests passed.
```

## Example

Include as a dependency in your release, using rebar...

```
{deps, [
    {emagick_rs, {git, "https://github.com/nlfiedler/emagick.rs", {tag, "0.4.8"}}}
]}.
```

Be sure to include `emagick_rs` in the `included_applications` list of your application configuration before building a release.

Fetching a property from an image...

```
{ok, ImageData} = file:read_file("IMG_5745.JPG"),
{ok, Value} = emagick_rs:image_get_property(ImageData, "exif:DateTimeOriginal").
% Value: "2014:04:23 13:33:08"
```

## Escript Usage

When using this library with an escript, it may be necessary to set the environment variable `NIF_DIR` to the path that contains the `.so` file for emagick.rs, which may look something like `_build/default/lib/emagick_rs/priv`, if you are using rebar3.

## Dirty Scheduler Support

Technically the functions in this library are CPU intensive, since they are processing image files and that takes a non-trivial amount of time. However, support for dirty CPU schedulers in Erlang 19 has been marked as experimental, and as such is often not available with a base installation. When this status changes, the dirty CPU flags will be added back to the NIF function definitions.

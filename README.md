# emagick.rs

A basic [Erlang](http://www.erlang.org) interface to the [ImageMagick](http://www.imagemagick.org) library, via [Rust](https://www.rust-lang.org) bindings. Currently very few fuctions are exposed, generally only those that are needed for the [tanuki](https://github.com/nlfiedler/tanuki) project.

## Requirements

* Erlang/OTP R17|R18
* [Rebar3](https://github.com/erlang/rebar3) 3.0.0 or higher
* Rust (1.3 or higher should work)
* Cargo
* ImageMagick (tested with 6.9)
    - See the development setup docs in the `magick-rust` project.

## Building and Testing

The following should build and test everything from scratch, including downloading dependencies.

```
$ rebar3 ct
...
===> Running Common Test suites...
%%% emagick_rs_SUITE ==> test_image_fit: OK
%%% emagick_rs_SUITE ==> test_image_get_property: OK
All 2 tests passed.
```

## Example

Include as a dependency in your release, using rebar...

```
{deps, [
    {emagick_rs, {git, "https://github.com/nlfiedler/emagick.rs", {tag, "0.3.5"}}}
]}.
```

Fetching a property from an image...

```
{ok, ImageData} = file:read_file("IMG_5745.JPG"),
{ok, Value} = emagick_rs:image_get_property(ImageData, "exif:DateTimeOriginal").
% Value: "2014:04:23 13:33:08"
```

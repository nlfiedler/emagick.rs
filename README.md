# emagick.rs

A basic [Erlang](http://www.erlang.org) interface to the [ImageMagick](http://www.imagemagick.org) library, via [Rust](https://www.rust-lang.org) bindings. Currently very few fuctions are exposed, generally only those that are needed for the [tanuki](https://github.com/nlfiedler/tanuki) project.

## Requirements

* Erlang/OTP R17|R18
* [Rebar](https://github.com/rebar/rebar)
* Rust (1.3 or higher should work)
* Cargo
* ImageMagick (tested with 6.9)

## Building and Testing

The following should clean and build everything from scratch, including downloading dependencies.

```
$ rebar clean
$ rebar compile
$ rebar ct
...
DONE.
Testing projects.emagick.rs: TEST COMPLETE, 1 ok, 0 failed of 1 test cases
```

## Example

Include as a dependency in your release, using rebar...

```
{deps, [
    {emagick_rs, ".*", {git, "https://github.com/nlfiedler/emagick.rs", {tag, "0.3.1"}}}
]}.
```

Fetching a property from an image...

```
{ok, ImageData} = file:read_file("IMG_5745.JPG"),
{ok, Value} = emagick_rs:image_get_property(ImageData, "exif:DateTimeOriginal").
% Value: "2014:04:23 13:33:08"
```

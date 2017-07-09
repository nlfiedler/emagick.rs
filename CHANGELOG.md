# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [0.4.7] - 2017-07-08
### Changed
- Upgrade magick_rust to version 0.6.6

## [0.4.6] - 2017-07-03
### Changed
- Upgrade erlang_nif-sys to support Erlang/OTP 20.

## [0.4.5] - 2017-04-09
### Changed
- Upgrade dependencies: magick-rust and erlang_nif-sys.

## [0.4.4] - 2016-10-27
### Changed
- Upgrade dependencies: magick-rust and erlang_nif-sys.
- Remove the dirty CPU scheduler flags since it is currently a hindrance.

## [0.4.3] - 2016-09-20
### Changed
- Upgrade dependencies: magick-rust.

## [0.4.2] - 2016-07-17
### Changed
- Upgrade dependencies: magick-rust and erlang_nif-sys (née ruster_unsafe).

## [0.4.1] - 2016-07-16
### Changed
- Add function to indicate if an image requires orientation correction.

## [0.4.0] - 2016-03-29
### Changed
- Add function to auto-orient images.

## [0.3.5] - 2016-03-23
### Changed
- Switch back to original ruster_unsafe crate.

## [0.3.4] - 2016-03-17
### Changed
- Fix image resize test to be more flexible.

## [0.3.3] - 2016-03-14
### Changed
- Upgrade to rebar3 build system.
- Enhanced the native code loading function.

## [0.3.2] - 2016-02-10
### Changed
- Update version of `magick-rust` to 0.3.2.

## [0.3.1] - 2016-01-14
### Changed
- If `enif_make_existing_atom()` returns `0` then need to call `enif_make_atom()`.

## [0.3.0] - 2016-01-02
### Changed
- Add `image_get_property()` function to retrieve, for example, EXIF data.

## [0.2.0] - 2015-12-27
### Changed
- The `image_fit/3` function now returns `{ok, Binary}` or `{error, Reason}` in order
  to indicate success or failure.

## [0.1.2] - 2015-12-26
### Changed
- Upgrade to latest dependencies (mostly `libc` to reduce build incompatibilities).

## [0.1.1] - 2015-12-26
### Changed
- Change build system to rely entirely on rebar so others can easily include this module.

## [0.1.0] - 2015-12-24
### Changed
- Initial release

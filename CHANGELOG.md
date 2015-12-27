# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

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

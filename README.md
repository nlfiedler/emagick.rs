# emagick.rs

## TODO

1. Get `ruster_unsafe` to build, currently having trouble
    * See ruster_unsafe [issue #3](https://github.com/goertzenator/ruster_unsafe/issues/3)
        - c.f. https://github.com/rust-lang/rust/issues/14344
        - Apparently fixed in rustc nightly in commit 9bc8e6d
    * Builds on Linux, fails on Mac
    * Is it possible to cross-compile from Linux to Mac?
        - https://github.com/japaric/ruststrap/blob/master/1-how-to-cross-compile.md
    * Consider going low level with https://github.com/lavrin/erlang-rust-nif
1. Incorporate `magick_rust` crate for the underlying interface to ImageMagick
1. Develop a Rust NIF wrapper that can be called from Erlang.
	* Use https://github.com/goertzenator/ruster_unsafe
	* See http://goertzenator.github.io/ruster_unsafe/ruster_unsafe/index.html
1. Write unit tests
1. Add a license and copyright headers
1. Test it on lots of images in batches to stress test it; should not crash

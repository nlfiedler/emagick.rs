# emagick.rs README

## TODO

1. Develop a Rust crate that invokes functions in ImageMagick library.
    * FFI docs: https://doc.rust-lang.org/stable/book/ffi.html
    * C++/Erlang bindings: https://github.com/mokele/eim
    * Old Rust bindings: https://github.com/influenza/wand-of-rust
    * Wand API: http://www.imagemagick.org/script/magick-wand.php
1. Develop a Rust NIF wrapper that can be called from Erlang.
	* Use https://github.com/goertzenator/ruster_unsafe
	* See http://goertzenator.github.io/ruster_unsafe/ruster_unsafe/index.html
1. Write unit tests
1. Add a license and copyright headers
1. Test it on lots of images in batches to stress test it; should not crash

## Other Ideas

* Use [rust-bindgen](https://github.com/crabtw/rust-bindgen) tool to generate updated ImageMagick bindings.

## Using rust-bindgen

This example is using the [Homebrew](http://brew.sh) installed version of ImageMagick. Your paths may be different depending on the operating system and manner in which ImageMagick was installed.

```
$ git clone https://github.com/crabtw/rust-bindgen.git
$ cd rust-bindgen
$ cargo build
$ DYLD_LIBRARY_PATH=/Library/Developer/CommandLineTools/usr/lib \
    ./target/debug/bindgen \
    -l MagickWand-6.Q16 \
    -o ~/bindings.rs \
    -I/usr/local/Cellar/imagemagick/6.9.1-4/include/ImageMagick-6 \
    /usr/local/Cellar/imagemagick/6.9.1-4/include/ImageMagick-6/wand/MagickWand.h
```

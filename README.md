# emagick.rs README

## TODO

1. Develop a Rust crate that invokes functions in ImageMagick library.
    * Research how to link to external C libraries
    * Look at how https://github.com/mokele/eim works
    * Test it on lots of images in batches to stress test it; should not crash
1. Develop a Rust NIF wrapper that can be called from Erlang.
	* Use https://github.com/goertzenator/ruster_unsafe
	* See http://goertzenator.github.io/ruster_unsafe/ruster_unsafe/index.html

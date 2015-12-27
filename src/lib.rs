/*
 * Copyright 2015 Nathan Fiedler
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#[macro_use]
extern crate ruster_unsafe;
extern crate magick_rust;

use ruster_unsafe::*;
use magick_rust::{MagickWand, magick_wand_genesis, magick_wand_terminus};
use std::mem::uninitialized;

/// Create NIF module data and init function.
nif_init!(b"emagick_rs\0", Some(load), None, None, Some(unload),
     nif!(b"image_fit\0", 3, image_fit, ERL_NIF_DIRTY_JOB_CPU_BOUND)
    );

/// Resize the image to fit the given dimensions. Always produces a JPEG.
/// Arguments are the binary, desired width, and desired height.
/// The aspect ratio will be maintained.
extern "C" fn image_fit(env: *mut ErlNifEnv,
                        argc: c_int,
                        args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    unsafe {
        let mut width:c_int = uninitialized();
        let mut height:c_int = uninitialized();
        let mut bin:ErlNifBinary = uninitialized();
        if argc == 3 &&
           0 != enif_get_int(env, *args.offset(1), &mut width) &&
           0 != enif_get_int(env, *args.offset(2), &mut height) &&
           0 != enif_inspect_binary(env, *args, &mut bin) {
            let wand = MagickWand::new();
            let slice = std::slice::from_raw_parts(bin.data, bin.size as usize);
            let data = Vec::from(slice);
            assert!(wand.read_image_blob(data).is_ok());
            wand.fit(width as usize, height as usize);
            // for now, write_image_blob() only ever returns Ok
            let blob = wand.write_image_blob("jpeg").unwrap();
            let mut out:ERL_NIF_TERM = uninitialized();
            let buf = enif_make_new_binary(env, blob.len() as usize, &mut out);
            std::ptr::copy(blob.as_ptr(), buf, blob.len());
            out
        } else {
            enif_make_badarg(env)
        }
    }
}

/// Initialize the ImageMagick library.
extern "C" fn load(_env: *mut ErlNifEnv,
                   _priv_data: *mut *mut c_void,
                   _load_info: ERL_NIF_TERM)-> c_int {
    magick_wand_genesis();
    0
}

/// Prepare the ImageMagick library for shutdown.
extern "C" fn unload(_env: *mut ErlNifEnv,
                     _priv_data: *mut c_void) {
    magick_wand_terminus();
}

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
use magick_rust::{/*MagickWand, */magick_wand_genesis, magick_wand_terminus};

/// Create NIF module data and init function.
nif_init!(b"emagick_rs\0", Some(load), None, None, Some(unload),
     nif!(b"image_fit\0", 3, image_fit, ERL_NIF_DIRTY_JOB_CPU_BOUND)
    );

extern "C" fn image_fit(env: *mut ErlNifEnv,
                        _argc: c_int,
                        _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    // TODO: initialize a magic wand instance
    // TODO: load erl binary into wand image
    // TODO: size the image to fit the given dimensions
    // TODO: write the image data to a new erl binary
    // TODO: return the erl binary
    unsafe {
        enif_make_int(env, 0)
    }
}

/// Initialize static atom.
extern "C" fn load(_env: *mut ErlNifEnv,
                   _priv_data: *mut *mut c_void,
                   _load_info: ERL_NIF_TERM)-> c_int {
    magick_wand_genesis();
    0
}

/// Does nothing, reports success
extern "C" fn unload(_env: *mut ErlNifEnv,
                     _priv_data: *mut c_void) {
    magick_wand_terminus();
}

//
// read Erlang binary into image data
//
// status=MagickReadImageBlob(wand, data_, size_);
// if (status == MAGICK_FALSE) {
//     throw("An error occured");
// }
// long int x,y;
// status=MagickGetImagePage(wand,&width_,&height_,&x,&y);
// if (status == MAGICK_FALSE) {
//     throw("An error occured");
// }

//
// convert image to final format, convert to Erlang binary
//
// new_blob = handle->image->process(eim_format, &new_length);
// enif_alloc_binary_compat(env, new_length, &new_binary);
// memcpy(new_binary.data, new_blob, new_length);
// return enif_make_binary(env, &new_binary);

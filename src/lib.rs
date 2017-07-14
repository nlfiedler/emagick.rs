/*
 * Copyright 2015-2017 Nathan Fiedler
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
extern crate erlang_nif_sys;
extern crate magick_rust;
extern crate libc;

use erlang_nif_sys::*;
use magick_rust::{MagickWand, magick_wand_genesis, magick_wand_terminus};
use std::ffi::CString;
use std::mem::uninitialized;

/// Create NIF module data and init function.
nif_init!(b"emagick_rs\0", Some(load), None, None, Some(unload),
     //
     // Ideally these would all have the ERL_NIF_DIRTY_JOB_CPU_BOUND flag
     // but support for dirty schedulers is still experimental and not
     // available in all distributions (especially FreeBSD).
     //
     nif!(b"image_fit\0", 3, image_fit, 0),
     nif!(b"image_get_property\0", 2, image_get_property, 0),
     nif!(b"image_get_format\0", 1, image_get_format, 0),
     nif!(b"requires_orientation\0", 1, requires_orientation, 0),
     nif!(b"auto_orient\0", 1, auto_orient, 0)
    );

/// Resize the image to fit the given dimensions. Always produces a JPEG.
/// Arguments are the binary, desired width, and desired height. The aspect
/// ratio will be maintained.
extern "C" fn image_fit(env: *mut ErlNifEnv,
                        argc: c_int,
                        args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    let mut width:c_int = unsafe { uninitialized() };
    let mut height:c_int = unsafe { uninitialized() };
    let mut bin:ErlNifBinary = unsafe { uninitialized() };
    if argc == 3 &&
       0 != unsafe { enif_get_int(env, *args.offset(1), &mut width) } &&
       0 != unsafe { enif_get_int(env, *args.offset(2), &mut height) } &&
       0 != unsafe { enif_inspect_binary(env, *args, &mut bin) } {
        let wand = MagickWand::new();
        let slice = unsafe { std::slice::from_raw_parts(bin.data, bin.size as usize) };
        let data = Vec::from(slice);
        if wand.read_image_blob(&data).is_err() {
            return make_err_result(env, "unable to read blob");
        }
        let image_format = wand.get_image_format();
        if image_format.is_err() {
            return make_err_result(env, "unable to read image format");
        }
        wand.fit(width as usize, height as usize);
        let blob_result = wand.write_image_blob(image_format.unwrap().as_str());
        if blob_result.is_err() {
            return make_err_result(env, "unable to write blob");
        }
        let blob = blob_result.unwrap();
        let mut bout:ERL_NIF_TERM = unsafe { uninitialized() };
        let buf = unsafe { enif_make_new_binary(env, blob.len() as usize, &mut bout) };
        unsafe { std::ptr::copy(blob.as_ptr(), buf, blob.len()) };
        make_ok_result(env, &bout)
    } else {
        unsafe { enif_make_badarg(env) }
    }
}

/// Retrieve the named property from the given image data, such as EXIF
/// tagged data. Returns {ok, Data}, or {error, Reason} if an error occurs.
extern "C" fn image_get_property(env: *mut ErlNifEnv,
                                 argc: c_int,
                                 args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    let mut bin:ErlNifBinary = unsafe { uninitialized() };
    if argc == 2 && 0 != unsafe { enif_inspect_binary(env, *args, &mut bin) } {
        let wand = MagickWand::new();
        let slice = unsafe { std::slice::from_raw_parts(bin.data, bin.size as usize) };
        let data = Vec::from(slice);
        if wand.read_image_blob(&data).is_err() {
            return make_err_result(env, "unable to read blob");
        }
        // need to allocate the space for the incoming string
        // (1024 should be enough for the names of most properties)
        let mut name:Vec<c_uchar> = Vec::with_capacity(1024);
        let name_len = unsafe { enif_get_string(env, *args.offset(1), name.as_mut_ptr(), 1024,
            ErlNifCharEncoding::ERL_NIF_LATIN1) };
        if name_len == 0 {
            return make_err_result(env, "invalid name argument");
        }
        unsafe { name.set_len((name_len - 1) as usize) };
        let rname = std::str::from_utf8(&name);
        if rname.is_err() {
            return make_err_result(env, "invalid name");
        }
        let value = wand.get_image_property(rname.unwrap());
        if value.is_err() {
            return make_err_result(env, value.unwrap_err());
        }
        let rvalue = value.unwrap();
        let value_str = unsafe { enif_make_string_len(env, rvalue.as_ptr(), rvalue.len(),
            ErlNifCharEncoding::ERL_NIF_LATIN1) };
        make_ok_result(env, &value_str)
    } else {
        unsafe { enif_make_badarg(env) }
    }
}

/// Retrieve the format from the given image data, such as 'JPEG' or 'PNG'.
/// Returns {ok, Data}, or {error, Reason} if an error occurs.
extern "C" fn image_get_format(env: *mut ErlNifEnv,
                               argc: c_int,
                               args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    let mut bin:ErlNifBinary = unsafe { uninitialized() };
    if argc == 1 && 0 != unsafe { enif_inspect_binary(env, *args, &mut bin) } {
        let wand = MagickWand::new();
        let slice = unsafe { std::slice::from_raw_parts(bin.data, bin.size as usize) };
        let data = Vec::from(slice);
        if wand.read_image_blob(&data).is_err() {
            return make_err_result(env, "unable to read blob");
        }
        let value = wand.get_image_format();
        if value.is_err() {
            return make_err_result(env, value.unwrap_err());
        }
        let rvalue = value.unwrap();
        let value_str = unsafe { enif_make_string_len(env, rvalue.as_ptr(), rvalue.len(),
            ErlNifCharEncoding::ERL_NIF_LATIN1) };
        make_ok_result(env, &value_str)
    } else {
        unsafe { enif_make_badarg(env) }
    }
}

/// Returns true if the image requires auto-orientation, false otherwise.
/// The one argument is the binary image data.
extern "C" fn requires_orientation(env: *mut ErlNifEnv,
                                   argc: c_int,
                                   args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    let mut bin:ErlNifBinary = unsafe { uninitialized() };
    if argc == 1 &&
       0 != unsafe { enif_inspect_binary(env, *args, &mut bin) } {
        let wand = MagickWand::new();
        let slice = unsafe { std::slice::from_raw_parts(bin.data, bin.size as usize) };
        let data = Vec::from(slice);
        if wand.read_image_blob(&data).is_err() {
            return make_err_result(env, "unable to read blob");
        }
        let result = wand.requires_orientation();
        make_boolean(env, result)
    } else {
        unsafe { enif_make_badarg(env) }
    }
}

/// Automatically orient the image so it is suitable for viewing. Always
/// produces a JPEG. The one argument is the binary image data.
extern "C" fn auto_orient(env: *mut ErlNifEnv,
                          argc: c_int,
                          args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    let mut bin:ErlNifBinary = unsafe { uninitialized() };
    if argc == 1 &&
       0 != unsafe { enif_inspect_binary(env, *args, &mut bin) } {
        let wand = MagickWand::new();
        let slice = unsafe { std::slice::from_raw_parts(bin.data, bin.size as usize) };
        let data = Vec::from(slice);
        if wand.read_image_blob(&data).is_err() {
            return make_err_result(env, "unable to read blob");
        }
        if !wand.auto_orient() {
            return make_err_result(env, "unable to orient image");
        }
        let image_format = wand.get_image_format();
        if image_format.is_err() {
            return make_err_result(env, "unable to read image format");
        }
        let blob_result = wand.write_image_blob(image_format.unwrap().as_str());
        if blob_result.is_err() {
            return make_err_result(env, "unable to write blob");
        }
        let blob = blob_result.unwrap();
        let mut bout:ERL_NIF_TERM = unsafe { uninitialized() };
        let buf = unsafe { enif_make_new_binary(env, blob.len() as usize, &mut bout) };
        unsafe { std::ptr::copy(blob.as_ptr(), buf, blob.len()) };
        make_ok_result(env, &bout)
    } else {
        unsafe { enif_make_badarg(env) }
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

/// Produce a 2-tuple consisting of 'ok' and the given result.
fn make_ok_result(env: *mut ErlNifEnv, result: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    make_tuple(env, "ok", result)
}

/// Produce a 2-tuple consisting of 'error' and the given reason.
fn make_err_result(env: *mut ErlNifEnv, reason: &str) -> ERL_NIF_TERM {
    let reason_str = unsafe { enif_make_string_len(env, reason.as_ptr(), reason.len(),
        ErlNifCharEncoding::ERL_NIF_LATIN1) };
    make_tuple(env, "error", &reason_str)
}

/// Produce a 2-tuple consisting of the label and the term.
/// The label is converted to an atom.
fn make_tuple(env: *mut ErlNifEnv, label: &str, result: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    let mut label_atom:ERL_NIF_TERM = unsafe { uninitialized() };
    let c_label_str = CString::new(label).unwrap();
    let c_label_nul = c_label_str.as_bytes_with_nul().as_ptr();
    // Try using an existing atom, but if that fails, create a new one.
    let atom_exists = unsafe { enif_make_existing_atom(
        env, c_label_nul, &mut label_atom, ErlNifCharEncoding::ERL_NIF_LATIN1) };
    if atom_exists == 0 {
        label_atom = unsafe { enif_make_atom(env, c_label_nul) };
    }
    let tuple_args = unsafe { [label_atom, *result] };
    unsafe { enif_make_tuple_from_array(env, tuple_args.as_ptr(), 2) }
}

/// Return an atom for either true or false.
fn make_boolean(env: *mut ErlNifEnv, value: bool) -> ERL_NIF_TERM {
    let mut label_atom:ERL_NIF_TERM = unsafe { uninitialized() };
    let c_label_str = if value {
        CString::new("true").unwrap()
    } else {
        CString::new("false").unwrap()
    };
    let c_label_nul = c_label_str.as_bytes_with_nul().as_ptr();
    // Try using an existing atom, but if that fails, create a new one.
    let atom_exists = unsafe { enif_make_existing_atom(
        env, c_label_nul, &mut label_atom, ErlNifCharEncoding::ERL_NIF_LATIN1) };
    if atom_exists == 0 {
        label_atom = unsafe { enif_make_atom(env, c_label_nul) };
    }
    label_atom
}

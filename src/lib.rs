//
// TODO: replace all of this with ruster-unsafe code to call into magick-rust
//

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

//
//
//

//!
//! "Safe" wrapper around the low-level bindings to ImageMagick.
//!

extern crate libc;

mod bindings;

///
/// MagickWand is a wrapper to the Rust bindings to ImageMagick.
///
pub struct MagickWand {
    wand: *mut bindings::MagickWand
}

impl MagickWand {

    /// Create a new MagickWand instance. This instance will be properly
    /// cleaned up once it falls out of scope.
    pub fn new() -> MagickWand {
        MagickWand {
            wand: unsafe { bindings::NewMagickWand() }
        }
    }
}

// Automate safe cleanup for MagickWand instances.
impl Drop for MagickWand {

    fn drop(&mut self) {
        unsafe {
            bindings::MagickClearException(self.wand);
            bindings::DestroyMagickWand(self.wand);
        }
    }
}

/// This function must be called before any other ImageMagick operations
/// are attempted.
pub fn magick_wand_genesis() {
    unsafe {
        bindings::MagickWandGenesis();
    }
}

/// This function should be called when ImageMagick is no longer needed.
pub fn magick_wand_terminus() {
    unsafe {
        bindings::MagickWandTerminus();
    }
}

#[cfg(test)]
mod test {

    use super::{MagickWand};

    #[test]
    fn test_something() {
    }
}

// fn fit(size_t width, size_t height) {
//     double width_ratio = (double)width / width_;
//     double height_ratio = (double)height / height_;
//     size_t new_width, new_height;
//     if (width_ratio < height_ratio) {
//         new_width = width;
//         new_height = (size_t)(height_ * width_ratio);
//     } else {
//         new_width = (size_t)(width_ * height_ratio);
//         new_height = height;
//     }
//     width_ = new_width;
//     height_ = new_height;
//     MagickResetIterator(wand);
//     while (MagickNextImage(wand) != MAGICK_FALSE) {
//         MagickResizeImage(wand, new_width, new_height, LanczosFilter, 1.0);
//     }
// }

// wand-of-rust wrapper around MagickResizeImage
// pub fn resize_image(&self, width: uint, height: uint,
//                     filter: FilterType, blur_factor: f64) {
//     unsafe {
//         bindings::MagickResizeImage(
//             self.wand, width as size_t, height as size_t,
//             filter as c_uint, blur_factor as c_double
//         );
//     }
// }

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

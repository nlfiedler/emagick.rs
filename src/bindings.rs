//
//
//

use libc::{c_double, c_long, c_uint, c_void, size_t};

#[link(name = "MagickWand-6.Q16")]
extern {
    pub fn MagickWandGenesis();
    pub fn NewMagickWand() -> *mut MagickWand;

    pub fn MagickReadImageBlob(wand: *mut MagickWand, data: *mut c_void,
                           length: size_t) -> MagickBoolean;
    pub fn MagickGetImagePage(wand: *mut MagickWand, width: *mut size_t, height: *mut size_t,
                          x: *mut c_long, y: *mut c_long) -> MagickBoolean;

    pub fn MagickClearException(wand: *mut MagickWand) -> MagickBoolean;
    pub fn DestroyMagickWand(wand: *mut MagickWand) -> *mut MagickWand;
    pub fn MagickWandTerminus();
}

pub type MagickWand = c_void;
pub type MagickBoolean = c_uint;
pub static MAGICK_FALSE: c_uint = 0;
pub static MAGICK_TRUE: c_uint = 1;

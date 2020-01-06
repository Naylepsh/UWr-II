mod utils;

use wasm_bindgen::prelude::*;
extern crate web_sys;
extern crate image;
use web_sys::ImageData;
use wasm_bindgen::Clamped;
use image::ImageBuffer;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct Mandelbrot {
    width: u32,
    height: u32
}

#[wasm_bindgen]
impl Mandelbrot {
    pub fn new(width: u32, height: u32) -> Mandelbrot {
        Mandelbrot { width, height }
    }

    fn contains(x:f32, y:f32) -> u32 {
        let mut real_component = x;
        let mut imaginary_component = y;
        let max_iter = 100;
    
        for i in 0..max_iter {
            let temp_real_component = real_component * real_component
                                    - imaginary_component * imaginary_component
                                    + x;
            let temp_imaginary_component = 2.0 * real_component * imaginary_component
                                    + y;
            real_component = temp_real_component;
            imaginary_component = temp_imaginary_component;
    
            if real_component * imaginary_component > 5.0 {
                return i * 100 / max_iter;
            }
        }
        0
    }

    pub fn to_image(&self, magnification_factor: u32, pan_x: f32, pan_y: f32) -> web_sys::ImageData {
        let mut image = ImageBuffer::new(self.width, self.height);
        for (x, y, pixel) in image.enumerate_pixels_mut() {
            let pos_x = x as f32 / (magnification_factor as f32) - pan_x;
            let pos_y = y as f32 / (magnification_factor as f32) - pan_y;
            let belongs_to_set = Mandelbrot::contains(pos_x, pos_y);

            if belongs_to_set == 0 {
                *pixel = image::Rgba([0, 0, 0, 255]);
            } else {
                let (r, g, b) = Mandelbrot::get_color(belongs_to_set as f32 / 100.0);
                *pixel = image::Rgba([r, g, b, 255]);
                // *pixel = image::Rgba([255, 255, 255, 255]);
            }
        }
        let pixels = image.to_vec();
        
        let mut image = Image::new(pixels, self.width, self.height);
        return image.get_image_data();
    }

    // hsl(0%, 100%, l) to rgb conversion
    fn get_color(l: f32) -> (u8, u8, u8) {
        let c = 1.0 - (2.0*l - 1.0).abs();
        let m = l - c/2.0;

        let r = ((c + m) * 255.0) as u8;
        let g = (m * 255.0) as u8;
        let b = (m * 255.0) as u8;
        return (r, g, b);
    }
}

#[wasm_bindgen]
pub struct Image {
    pixels: Vec<u8>,
    width: u32, 
    height: u32,
}

#[wasm_bindgen]
impl Image {   
    #[wasm_bindgen(constructor)]
    pub fn new(pixels: Vec<u8>, width: u32, height: u32) -> Image {
        return Image { pixels, width, height};
    }

    pub fn get_width(&self) -> u32 {
        self.width
    }

    pub fn get_height(&self) -> u32 {
        self.height
    }

    // Convert the Image's pixels to JS-compatible ImageData.
    pub fn get_image_data(&mut self) -> ImageData {
        let image_data = ImageData::new_with_u8_clamped_array_and_sh(Clamped(&mut self.pixels), self.width, self.height).unwrap();
        image_data
    }
}

/// Convert ImageData to a raw pixel vec of u8s.
#[wasm_bindgen]
pub fn to_pixels(imgdata: ImageData) -> Vec<u8> {
    let img_vec = imgdata.data().to_vec();
    return img_vec;
}

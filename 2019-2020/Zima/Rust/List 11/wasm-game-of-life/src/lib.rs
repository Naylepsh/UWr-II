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
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cell {
    Dead = 0,
    Alive = 1,
}

impl Cell {
    fn toggle(&mut self) {
        *self = match *self {
            Cell::Dead => Cell::Alive,
            Cell::Alive => Cell::Dead,
        };
    }

    fn to_rgb(&self) -> Rgb {
        match *self {
            Cell::Dead => Rgb::new(0,0,0),
            Cell::Alive => Rgb::new(255,255,255)
        }
    }
}

#[wasm_bindgen]
pub struct Universe {
    width: u32,
    height: u32,
    cells: Vec<Cell>,
}

#[wasm_bindgen]
impl Universe {
    pub fn new() -> Universe {
        utils::set_panic_hook();
        let width = 64;
        let height = 64;

        let cells = (0..width * height)
            .map(|i| {
                if i % 2 == 0 || i % 7 == 0 {
                    Cell::Alive
                } else {
                    Cell::Dead
                }
            })
            .collect();

        Universe {
            width,
            height,
            cells,
        }
    }
    
    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn cells(&self) -> *const Cell {
        self.cells.as_ptr()
    }

    /// Set the width of the universe.
    ///
    /// Resets all cells to the dead state.
    pub fn set_width(&mut self, width: u32) {
        self.width = width;
        self.cells = (0..width * self.height).map(|_i| Cell::Dead).collect();
    }

    /// Set the height of the universe.
    ///
    /// Resets all cells to the dead state.
    pub fn set_height(&mut self, height: u32) {
        self.height = height;
        self.cells = (0..self.width * height).map(|_i| Cell::Dead).collect();
    }

    pub fn toggle_cell(&mut self, row: u32, column: u32) {
        let idx = self.get_index(row, column);
        self.cells[idx].toggle();
    }

    fn get_index(&self, row: u32, column: u32) -> usize {
        (row * self.width + column) as usize
    }

    fn live_neighbor_count(&self, row: u32, column: u32) -> u8 {
        let mut count = 0;
        for delta_row in [self.height - 1, 0, 1].iter().cloned() {
            for delta_col in [self.width - 1, 0, 1].iter().cloned() {
                if delta_row == 0 && delta_col == 0 {
                    continue;
                }

                let neighbor_row = (row + delta_row) % self.height;
                let neighbor_col = (column + delta_col) % self.width;
                let idx = self.get_index(neighbor_row, neighbor_col);
                count += self.cells[idx] as u8;
            }
        }
        count
    }

    pub fn tick(&mut self) {
        let mut next = self.cells.clone();

        for row in 0..self.height {
            for col in 0..self.width {
                let idx = self.get_index(row, col);
                let cell = self.cells[idx];
                let live_neighbors = self.live_neighbor_count(row, col);

                let next_cell = match (cell, live_neighbors) {
                    // Rule 1: Any live cell with fewer than two live neighbours
                    // dies, as if caused by underpopulation.
                    (Cell::Alive, x) if x < 2 => Cell::Dead,
                    // Rule 2: Any live cell with two or three live neighbours
                    // lives on to the next generation.
                    (Cell::Alive, 2) | (Cell::Alive, 3) => Cell::Alive,
                    // Rule 3: Any live cell with more than three live
                    // neighbours dies, as if by overpopulation.
                    (Cell::Alive, x) if x > 3 => Cell::Dead,
                    // Rule 4: Any dead cell with exactly three live neighbours
                    // becomes a live cell, as if by reproduction.
                    (Cell::Dead, 3) => Cell::Alive,
                    // All other cells remain in the same state.
                    (otherwise, _) => otherwise,
                };

                next[idx] = next_cell;
            }
        }

        self.cells = next;
    }

    pub fn render(&self) -> String {
        self.to_string()
    }

    pub fn to_image(&self, cell_size: u32) -> ImageData {
        let width = self.width() * cell_size;
        let height = self.height() * cell_size;
        let mut image = ImageBuffer::new(width, height);
        for (x, y, pixel) in image.enumerate_pixels_mut() {
            let i = self.get_index(x / cell_size, y / cell_size);
            let cell = self.cells[i];
            let r = 255*cell as u8;
            let g = 255*cell as u8;
            let b = 255*cell as u8;
            *pixel = image::Rgba([r, g, b, 255]);
        }
        let pixels = image.to_vec();
        
        let mut image = Image::new(pixels, width, height);
        image.get_image_data()
    }
}

impl Universe {
    /// Get the dead and alive values of the entire universe.
    pub fn get_cells(&self) -> &[Cell] {
        &self.cells
    }

    /// Set cells to be alive in a universe by passing the row and column
    /// of each cell as an array.
    pub fn set_cells(&mut self, cells: &[(u32, u32)]) {
        for (row, col) in cells.iter().cloned() {
            let idx = self.get_index(row, col);
            self.cells[idx] = Cell::Alive;
        }
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

    pub fn get_pixels(&self) -> Vec<u8> {
        self.pixels.clone()
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

/// Create a new Image from a Vec of u8s representing image pixels.
impl From<ImageData> for Image {
    fn from(imgdata: ImageData) -> Self {
        let width = imgdata.width();
        let height = imgdata.height();
        let pixels = to_pixels(imgdata);
        return Image {pixels, width, height}
    }
}

/// RGB color type.
#[wasm_bindgen]
pub struct Rgb {
    r: u8,
    g: u8,
    b: u8
}

#[wasm_bindgen]
impl Rgb {
    pub fn new(r: u8, g: u8, b: u8) -> Rgb {
        return Rgb {r: r, g: g, b: b};
    }

    pub fn get_red(&self) -> u8 {
        self.r
    }

    pub fn get_green(&self) -> u8 {
        self.g
    }

    pub fn get_blue(&self) -> u8 {
        self.b
    }
}

impl From<Vec<u8>> for Rgb {
    fn from(vec: Vec<u8>) -> Self {
        if vec.len() != 3 {
            panic!("Vec length must be equal to 3.")
        }
        let rgb = Rgb::new(vec[0], vec[1], vec[2]);
        rgb
    }
}

/// Convert ImageData to a raw pixel vec of u8s.
#[wasm_bindgen]
pub fn to_pixels(imgdata: ImageData) -> Vec<u8> {
    let img_vec = imgdata.data().to_vec();
    return img_vec;
}

use std::fmt;

impl fmt::Display for Universe {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for line in self.cells.as_slice().chunks(self.width as usize) {
            for &cell in line {
                let symbol = if cell == Cell::Dead { '◻' } else { '◼' };
                write!(f, "{}", symbol)?;
            }
            write!(f, "\n")?;
        }

        Ok(())
    }
}

// // A macro to provide `println!(..)`-style syntax for `console.log` logging.
// macro_rules! log {
//     ( $( $t:tt )* ) => {
//         web_sys::console::log_1(&format!( $( $t )* ).into());
//     }
// }
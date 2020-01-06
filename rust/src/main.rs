// For reading and opening files
use std::convert::TryInto;
use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

mod pallet;
use pallet::*;
mod pl8;
use pl8::*;

pub struct Image {
    height: u32,
    width: u32,
    data: Vec<u8>,
}

fn pl8_to_image(pl8: &Pl8, pallet: &Pallet) -> Image {
    let mut image: Vec<u8> = Vec::new();

    // Number 4 is here for number of colors used (RGBA). And fill it with
    // transparent color.
    image.resize((pl8.width * pl8.height * 4).try_into().unwrap(), 0);

    for tile in &pl8.tiles {
        for x in 0..tile.width {
            for y in 0..tile.height {
                let tile_index: usize = (y * tile.width + x).try_into().unwrap();
                let pallet_index: usize = tile.pallet_indices[tile_index].try_into().unwrap();
                let data_index = (((tile.y + y) * pl8.width) + tile.x + x)
                    .try_into()
                    .unwrap();
                fill_color(&mut image, data_index, &pallet.colors[pallet_index]);
            }
        }
    }
    Image {
        height: pl8.height,
        width: pl8.width,
        data: image,
    }
}

fn to_png(image: &Image) {
    let file = File::create("image").unwrap();
    let ref mut w = BufWriter::new(file);

    let mut encoder = png::Encoder::new(w, image.width, image.height);
    encoder.set_color(png::ColorType::RGBA);
    encoder.set_depth(png::BitDepth::Eight);
    let mut writer = encoder.write_header().unwrap();
    writer.write_image_data(&image.data).unwrap(); // Save
}

fn fill_color(data: &mut [u8], pixel_index: usize, color: &PalletColor) {
    let pixel_index_ = pixel_index * 4;

    data[pixel_index_] = color.r;
    data[pixel_index_ + 1] = color.g;
    data[pixel_index_ + 2] = color.b;
    data[pixel_index_ + 3] = color.a;
}

fn main() {
    let pallet = read_pallet(Path::new("Base01.256")).unwrap();
    let pl8 = read_pl8(Path::new("Roads1a.pl8")).unwrap();
    let image = pl8_to_image(&pl8, &pallet);
    to_png(&image);
}

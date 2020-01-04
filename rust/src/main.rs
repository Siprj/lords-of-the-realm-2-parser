// For reading and opening files
use std::convert::TryInto;
use std::format;
use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

mod pallet;
use pallet::*;
mod pl8;
use pl8::*;

fn main() {
    println!("Hello, world!");

    let pallet = read_pallet(Path::new("Base01.256")).unwrap();
    println!("{:#?}", pallet.colors[1]);

    let pl8 = read_pl8(Path::new("Roads1a.pl8")).unwrap();
    // println!("{:#?}", pl8);

    println!("number of tiles: {:#?}", pl8.tiles.len());
    for (i, tile) in pl8.tiles.iter().enumerate() {
        let kwa = format!("image{}.png", i.to_string());
        let path = Path::new(&kwa);
        let file = File::create(path).unwrap();
        let ref mut w = BufWriter::new(file);

        let mut encoder = png::Encoder::new(w, tile.width.into(), tile.height.into());
        encoder.set_color(png::ColorType::RGBA);
        encoder.set_depth(png::BitDepth::Eight);
        let mut writer = encoder.write_header().unwrap();

        let data1: Vec<[u8; 4]> = tile
            .pallet_indices
            .iter()
            .map(|&index| {
                let index_: usize = index.try_into().unwrap();
                [
                    pallet.colors[index_].r,
                    pallet.colors[index_].g,
                    pallet.colors[index_].b,
                    pallet.colors[index_].a,
                ]
            })
            .collect();
        let data = data1.concat();
        writer.write_image_data(&data).unwrap(); // Save
    }
}

use nom::error::{ErrorKind, ParseError};
use nom::Err;
use nom::IResult;
use std::cmp::min;
use std::convert::TryInto;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
pub struct TileHeader {
    pub width: u16,
    pub height: u16,
    pub offset: u32,
    pub x: u16,
    pub y: u16,
    pub extra_type: u8,
    pub extra_rows: u8,
    // pub unk: u16,
}

#[derive(Debug)]
pub struct Pl8 {
    pub width: u32,
    pub height: u32,
    pub tiles: Vec<Tile>,
}

#[derive(Debug)]
pub struct Tile {
    pub width: u32,
    pub height: u32,
    pub x: u32,
    pub y: u32,
    pub extra_rows: u32,
    pub pallet_indices: Vec<u8>,
}

fn parse_tile_header(i: &[u8]) -> IResult<&[u8], TileHeader> {
    let (i, width) = nom::number::complete::le_u16(i)?;
    let (i, height) = nom::number::complete::le_u16(i)?;
    let (i, offset) = nom::number::complete::le_u32(i)?;
    let (i, x) = nom::number::complete::le_u16(i)?;
    let (i, y) = nom::number::complete::le_u16(i)?;
    let (i, extra_type) = nom::number::complete::le_u8(i)?;
    let (i, extra_rows) = nom::number::complete::le_u8(i)?;
    let (i, _) = nom::number::complete::le_u16(i)?;
    Ok((
        i,
        TileHeader {
            width,
            height,
            offset,
            x,
            y,
            extra_type,
            extra_rows,
        },
    ))
}

fn parse_simple<'a>(i: &'a [u8], tile_header: &TileHeader) -> IResult<&'a [u8], Tile> {
    // TODO: Check every try_into() ???
    // TODO: Error handling in case of not enough data.
    let offset: usize = tile_header.offset.try_into().unwrap();
    let data = &i[offset..];
    let width: usize = tile_header.width.try_into().unwrap();
    let height: usize = tile_header.height.try_into().unwrap();

    Ok((
        i,
        Tile {
            width: tile_header.width.into(),
            height: tile_header.height.into(),
            x: tile_header.x.into(),
            y: tile_header.y.into(),
            extra_rows: 0,
            pallet_indices: data[..width * height].to_vec(),
        },
    ))
}

fn parse_rle<'a>(i: &'a [u8], tile_header: &TileHeader) -> IResult<&'a [u8], Tile> {
    // TODO: What about all this unwrap stuff.
    let offset: usize = tile_header.offset.try_into().unwrap();
    let data = &i[offset..];
    let mut pallet_indices: Vec<u8> = Vec::new();

    let w_width: usize = tile_header.width.try_into().unwrap();
    let w_height: usize = tile_header.height.try_into().unwrap();
    let to_fill: usize = w_width * w_height;
    pallet_indices.reserve(to_fill);

    let mut index: usize = 0;
    while pallet_indices.len() < to_fill && index < data.len() {
        let number_of_opaque_pixels: usize = data[index].try_into().unwrap();
        index = index + 1;

        // TODO: Try to use nom combinator instead of this thing....
        if number_of_opaque_pixels == 0 {
            let number_of_transparetn_pixels: usize = data[index].try_into().unwrap();
            index = index + 1;

            for _ in 0..min(number_of_transparetn_pixels, to_fill - pallet_indices.len()) {
                pallet_indices.push(0);
            }
        } else {
            for _ in 0..min(number_of_opaque_pixels, to_fill - pallet_indices.len()) {
                let pallet_index = data[index];
                index = index + 1;

                pallet_indices.push(pallet_index);
            }
        }
    }

    Ok((
        i,
        Tile {
            width: tile_header.width.into(),
            height: tile_header.height.into(),
            x: tile_header.x.into(),
            y: tile_header.y.into(),
            extra_rows: 0,
            pallet_indices,
        },
    ))
}

fn parse_iso<'a>(i: &'a [u8], tile_header: &TileHeader) -> IResult<&'a [u8], Tile> {
    let width: usize = tile_header.width.try_into().unwrap();
    let height: usize = tile_header.height.try_into().unwrap();
    let extra_rows: usize = tile_header.extra_rows.try_into().unwrap();

    let half_width = width / 2;
    let half_height = height / 2;

    let offset: usize = tile_header.offset.try_into().unwrap();
    let data = &i[offset..];

    let mut pallet_indices = Vec::new();
    let size = width * (height + extra_rows);
    pallet_indices.resize(size, 0);

    let mut index = 0;
    // Fill top half
    for y in 0..half_height {
        let row_start = (half_height - 1 - y) * 2;
        let row_stop = row_start + (y * 4) + 2;

        for x in row_start..row_stop {
            let pallet_index = data[index];
            index += 1;

            pallet_indices[((y + extra_rows) * width) + x] = pallet_index;
        }
    }
    // Fill bottom half
    for y in half_height..height {
        let row_start = (half_height - 1 - (height - y - 1)) * 2;
        let row_stop = row_start + ((height - y - 1) * 4) + 2;

        for x in row_start..row_stop {
            let pallet_index = data[index];
            index += 1;

            pallet_indices[((y + extra_rows) * width) + x] = pallet_index;
        }
    }

    // Fill extra rows
    for y_ in (1..=extra_rows).rev() {
        let right_offset = if tile_header.extra_type == 3 {
            half_width + 1
        } else {
            width
        };
        let left_offset = if tile_header.extra_type == 4 {
            half_width - 1
        } else {
            0
        };

        for x in left_offset..right_offset {
            let y = if x <= half_width {
                y_ + (half_height - 1) - (x / 2)
            } else {
                y_ + (x / 2) - (half_height - 1)
            };

            let pallet_index = data[index];
            index += 1;
            if pallet_index != 0 {
                pallet_indices[(y * width) + x] = pallet_index;
            }
        }
    }
    let extra_rows: u32 = tile_header.extra_rows.into();
    let x: u32 = tile_header.x.into();
    let y: u32 = tile_header.y.into();
    let height: u32 = tile_header.height.into();
    let width: u32 = tile_header.width.into();
    Ok((
        i,
        Tile {
            x,
            y: y - extra_rows,
            width,
            height: height + extra_rows,
            extra_rows,
            pallet_indices,
        },
    ))
}

fn parse_pl8(orig_i: &[u8]) -> IResult<&[u8], Pl8> {
    let (i, rle_flag) = nom::number::complete::le_u16(orig_i)?;
    let (i, number_of_tiles) = nom::number::complete::le_u16(i)?;
    let (i, _) = nom::number::complete::le_u32(i)?;

    let mut tile_headers = Vec::new();
    tile_headers.reserve(number_of_tiles.try_into().unwrap());

    let mut for_input = i;
    for _ in 0..number_of_tiles {
        match parse_tile_header(for_input) {
            Ok((i, o)) => {
                tile_headers.push(o);
                for_input = i;
            }
            Err(Err::Error(e)) => {
                return Err(Err::Error(ParseError::append(i, ErrorKind::Count, e)));
            }
            Err(e) => {
                return Err(e);
            }
        }
    }

    let mut tiles = Vec::new();
    for tile_header in &tile_headers {
        if (rle_flag & 0b0001) == 1 {
            let (_, tile) = parse_rle(orig_i, tile_header)?;
            tiles.push(tile);
        } else if tile_header.extra_type == 0 {
            let (_, tile) = parse_simple(orig_i, tile_header)?;
            tiles.push(tile);
        } else {
            let (_, tile) = parse_iso(orig_i, tile_header)?;
            tiles.push(tile);
        }
    }

    let height = tiles
        .iter()
        .map(|v| v.height + v.y)
        .max()
        .expect("No tiles present in file!!!")
        .into();
    let width = tiles
        .iter()
        .map(|v| v.width + v.x)
        .max()
        .expect("No tiles present in file!!!")
        .into();

    Ok((
        for_input,
        Pl8 {
            height,
            width,
            tiles,
        },
    ))
}

pub fn read_pl8(path: &Path) -> std::io::Result<Pl8> {
    let mut file = File::open(path)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let (_, pl8) = parse_pl8(&data).unwrap();

    Ok(pl8)
}

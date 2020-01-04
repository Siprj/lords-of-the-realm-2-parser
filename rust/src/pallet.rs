use nom::error::{ErrorKind, ParseError};
use nom::Err;
use nom::IResult;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
pub struct PalletColor {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

fn parse_pallet_color(data: &[u8]) -> IResult<&[u8], PalletColor> {
    let (i, pix) = nom::bytes::complete::take(3usize)(data)?;
    Ok((
        i,
        PalletColor {
            r: pix[0] * 4,
            g: pix[1] * 4,
            b: pix[2] * 4,
            a: 255u8,
        },
    ))
}

#[derive(Debug)]
pub struct Pallet {
    pub colors: Vec<PalletColor>,
}

fn parse_pallet(data: &[u8]) -> IResult<&[u8], Pallet> {
    let mut colors = Vec::new();
    colors.reserve(255);
    colors.push(PalletColor {
        r: 0,
        g: 0,
        b: 0,
        a: 0,
    });
    let (i, _) = parse_pallet_color(&data)?;
    let mut for_input = i;
    // File should contain 256 elements of RBG colors.
    // We skipped first colour and read the rest.
    // First colour is transparent colour so we cant skip push it into the
    // result right away.
    for _ in 0..255 {
        match parse_pallet_color(for_input) {
            Ok((i, o)) => {
                colors.push(o);
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
    Ok((for_input, Pallet { colors }))
}

pub fn read_pallet(path: &Path) -> std::io::Result<Pallet> {
    let mut file = File::open(path)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let (_, pallet) = parse_pallet(&data).unwrap();
    Ok(pallet)
}

extern crate rustler;

use rustler::{Binary, OwnedBinary};

#[rustler::nif]
fn s2f(a: Binary) -> OwnedBinary {
    let float = String::from_utf8(a.to_vec())
        .unwrap()
        .parse::<f32>()
        .unwrap();
    let mut f = float.to_be_bytes();
    let mut binary: OwnedBinary = OwnedBinary::new(f.len()).unwrap();
    binary.as_mut_slice().copy_from_slice(f.as_mut_slice());
    binary
}

#[rustler::nif]
fn s2d(a: Binary) -> OwnedBinary {
    let float = String::from_utf8(a.to_vec())
        .unwrap()
        .parse::<f64>()
        .unwrap();
    let mut f = float.to_be_bytes();
    let mut binary: OwnedBinary = OwnedBinary::new(f.len()).unwrap();
    binary.as_mut_slice().copy_from_slice(f.as_mut_slice());
    binary
}

rustler::init!("s2f_nif", [s2f, s2d]);

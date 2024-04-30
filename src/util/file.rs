use std::process::exit;
use std::str;

use std::fs::read;
use std::path::Path;

pub fn read_src<P>(path: P) -> String
where
    P: AsRef<Path> + std::fmt::Debug + Clone,
{
    match read(path.clone()) {
        Ok(src) => match str::from_utf8(&src) {
            Ok(src) => src.to_string(),
            Err(e) => {
                eprintln!("Error reading input file({:?}): {}", path, e);
                exit(1);
            }
        },
        Err(e) => {
            eprintln!("Error reading input file({:?}): {}", path, e);
            exit(1);
        }
    }
}
pub fn _read_bytes<P>(path: P) -> Vec<u8>
where
    P: AsRef<Path> + std::fmt::Debug + Clone,
{
    match read(path.clone()) {
        Ok(src) => src,
        Err(e) => {
            eprintln!("Error reading input file({:?}): {}", path, e);
            exit(1);
        }
    }
}

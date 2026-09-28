//! Linux-hosted cross-language persistence test, never a CuBit IPC endpoint.
use cubit_config_turso_spike::{Commit, Result, Store, objects::EncodedObject};
use std::{fs, path::Path};

fn main() -> Result<()> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 4 {
        return Err("expected INPUT.hex DATABASE.sqlite OUTPUT.hex".into());
    }
    if Path::new(&args[2]).exists() || Path::new(&args[3]).exists() {
        return Err("refusing to overwrite fixture outputs".into());
    }
    let hex = fs::read_to_string(&args[1])?;
    let hex = hex.trim();
    if hex.len() % 2 != 0 || !hex.is_ascii() {
        return Err("invalid hex".into());
    }
    let bytes: Vec<u8> = (0..hex.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&hex[i..i + 2], 16))
        .collect::<std::result::Result<_, _>>()?;
    let object = EncodedObject::parse(&bytes)?;
    let store = Store::open(Path::new(&args[2]))?;
    assert_eq!(
        store.commit_object("org.cubit.ccl", "test", 0, 1, &object)?,
        Commit::Saved(1)
    );
    assert_eq!(
        store.commit_object("org.cubit.ccl", "test", 0, 1, &object)?,
        Commit::Conflict { actual: 1 }
    );
    store.checkpoint()?;
    store.close()?;
    let store = Store::open(Path::new(&args[2]))?;
    let saved = store
        .read_object("org.cubit.ccl", "test", object.schema())?
        .ok_or("missing object")?;
    assert_eq!(saved.object, object);
    let output: String = saved
        .object
        .bytes()
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect();
    fs::write(&args[3], output + "\n")?;
    store.close()?;
    println!(
        "Turso CCL object: closed/reopened typed payload PASS ({} CBOR bytes)",
        bytes.len()
    );
    Ok(())
}

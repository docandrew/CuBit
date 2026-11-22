//! Linux-hosted Ada schema/value -> Turso -> reopen -> independent consumer.
use cubit_config_turso_spike::{
    Commit, Result, Store,
    objects::EncodedObject,
    schemas::{Creation, EncodedSchema},
};
use std::{fs, io::Write, path::Path};

fn main() -> Result<()> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 4 {
        return Err("expected INPUT_BASE DATABASE OUTPUT_BASE".into());
    }
    let path = Path::new(&args[2]);
    if path.exists() {
        return Err("refusing to overwrite a database".into());
    }
    let schema = EncodedSchema::parse(&fs::read(format!("{}.schema", args[1]))?)?;
    let value = EncodedObject::parse(&fs::read(format!("{}.value", args[1]))?)?;
    let store = Store::open(path)?;
    assert_eq!(
        store.create_object("org.cubit.schema", "machine", &schema)?,
        Creation::Created
    );
    assert_eq!(
        store.read_object("org.cubit.schema", "machine", schema.key())?,
        None
    );
    store.close()?;
    // Recover the declaration before the first value exists.
    let store = Store::open(path)?;
    assert_eq!(
        store.read_definition("org.cubit.schema", "machine")?,
        Some(schema.clone())
    );
    assert_eq!(
        store.create_object("org.cubit.schema", "machine", &schema)?,
        Creation::AlreadyExists
    );
    assert_eq!(
        store.commit_declared_object("org.cubit.schema", "machine", 0, &value)?,
        Commit::Saved(1)
    );
    assert_eq!(
        store.commit_declared_object("org.cubit.schema", "machine", 0, &value)?,
        Commit::Conflict { actual: 1 }
    );
    store.checkpoint()?;
    store.close()?;
    let store = Store::open(path)?;
    let saved_schema = store
        .read_definition("org.cubit.schema", "machine")?
        .ok_or("missing declaration")?;
    let saved_value = store
        .read_object("org.cubit.schema", "machine", saved_schema.key())?
        .ok_or("missing value")?;
    assert_eq!(saved_schema, schema);
    assert_eq!(saved_value.object, value);
    for (suffix, bytes) in [
        ("schema", saved_schema.bytes()),
        ("value", saved_value.object.bytes()),
    ] {
        fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(format!("{}.{suffix}", args[3]))?
            .write_all(bytes)?;
    }
    store.close()?;
    println!("Real Turso: typed create/unset/reopen/set/reopen PASS");
    Ok(())
}

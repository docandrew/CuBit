//! Explicit live-session storage: normal transactions, no disk or reboot state.
use cubit_config_turso_spike::{Commit, Result, Setting, Store};

fn main() -> Result<()> {
    let namespace = "com.cubit.live.test";
    let values = vec![("counter".into(), Setting::Integer(42))];
    let first = Store::open_volatile()?;
    assert_eq!(first.read(namespace, "machine")?, None);
    assert_eq!(
        first.commit(namespace, "machine", 0, 1, &values)?,
        Commit::Saved(1)
    );
    assert_eq!(first.read(namespace, "machine")?.unwrap().entries, values);
    assert_eq!(
        first.commit(namespace, "machine", 0, 1, &values)?,
        Commit::Conflict { actual: 1 }
    );
    let other = Store::open_volatile()?;
    assert_eq!(other.read(namespace, "machine")?, None);
    drop(first);
    let restarted = Store::open_volatile()?;
    assert_eq!(restarted.read(namespace, "machine")?, None);
    println!("Volatile Config: transactions/conflicts, isolated sessions and empty restart PASS");
    Ok(())
}

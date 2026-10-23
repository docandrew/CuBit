//! A program's startup capabilities by name (CAP-002, docs/capability-handles.md).
//!
//! The launcher appends the startup table (CuBit.Startup_Tables) to the
//! launch block it installs before the program first runs, followed by a
//! trailer (u64 table bytes, u64 "CUBITLB1"; CuBit.Launch_Envelopes). The
//! kernel maps the block read-only at a fixed address and starts the main
//! thread with its length in RDI. `Table::load` copies the table once
//! (single fetch) and validates it; lookups are by binding name and kind.
//! No numbers: the only way to get a handle is a lookup in this table.

use crate::Endpoint;

/// Where the kernel maps a launch block (kernel Process_Launch).
const BLOCK_ADDRESS: usize = 0x0000_5A00_0000_0000;
/// "CUBITLB1" read as a little-endian word.
const ENVELOPE_MAGIC: u64 = 0x3142_4C54_4942_5543;
const TRAILER_BYTES: usize = 16;
/// "CBST" read as a little-endian word.
const TABLE_MAGIC: u32 = 0x5453_4243;
const FORMAT_VERSION: u16 = 1;
const MAXIMUM_ENTRIES: usize = 40;
const HEADER_BYTES: usize = 16;
const NAME_BYTES: usize = 64;
const ENTRY_FIXED_BYTES: usize = 16;
const ENTRY_BYTES: usize = ENTRY_FIXED_BYTES + NAME_BYTES;
/// CuBit.Startup_Tables.Maximum_Table_Bytes.
pub const MAXIMUM_TABLE_BYTES: usize = HEADER_BYTES + MAXIMUM_ENTRIES * ENTRY_BYTES;
/// The most the kernel accepts: a 64 KiB payload, the padded table, the trailer.
const MAXIMUM_BLOCK_BYTES: usize = 64 * 1024 + MAXIMUM_TABLE_BYTES + TRAILER_BYTES;

// Entry field offsets.
const KIND_AT: usize = 0;
const NEED_AT: usize = 1;
const STATE_AT: usize = 2;
const NAME_LENGTH_AT: usize = 3;
const HANDLE_AT: usize = 8;
const NAME_AT: usize = ENTRY_FIXED_BYTES;

/// What a startup capability is (CuBit.Startup_Tables.Capability_Kind codes).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Kind {
    Endpoint = 1,
    Notification = 2,
    Framebuffer = 3,
    Render = 4,
    Network = 5,
    DeviceMemory = 6,
    IoPorts = 7,
    Interrupt = 8,
    Dma = 9,
    Scheduling = 10,
    Process = 11,
    Resource = 12,
}

const LAST_KIND: u8 = Kind::Resource as u8;
const NEED_REQUIRED: u8 = 1;
const NEED_OPTIONAL: u8 = 2;
const STATE_GRANTED: u8 = 2;
const STATE_ABSENT: u8 = 3;

/// A validated copy of the startup table.
pub struct Table {
    bytes: [u8; MAXIMUM_TABLE_BYTES],
    count: usize,
}

fn word_at(bytes: &[u8], at: usize) -> u64 {
    let mut value = [0u8; 8];
    value.copy_from_slice(&bytes[at..at + 8]);
    u64::from_le_bytes(value)
}

impl Table {
    /// The table of a launch block of `block_length` bytes (the main
    /// thread's entry RDI), or None when it has none or it is malformed.
    ///
    /// # Safety
    /// `block_length` must be the value the kernel passed at entry: the
    /// kernel maps exactly that many bytes at the launch block address.
    pub unsafe fn load(block_length: u64) -> Option<Table> {
        let length = usize::try_from(block_length).ok()?;
        if length < TRAILER_BYTES || length > MAXIMUM_BLOCK_BYTES {
            return None;
        }
        // SAFETY: the caller guarantees the kernel mapped `length` readable
        // bytes at BLOCK_ADDRESS for the life of the process.
        let block = unsafe {
            core::slice::from_raw_parts(core::ptr::with_exposed_provenance::<u8>(BLOCK_ADDRESS), length)
        };
        Self::from_block(block)
    }

    /// The table, from the length the start code kept (feature
    /// `start-symbols`: every program's entry is the shared
    /// userspace/runtime/start/cubit_start.S, which defines
    /// __cubit_launch_block_length).
    #[cfg(feature = "start-symbols")]
    pub fn from_start() -> Option<Table> {
        unsafe extern "C" {
            static __cubit_launch_block_length: u64;
        }
        // SAFETY: the start code wrote the kernel's entry RDI there before
        // any Rust code ran; it is never written again.
        unsafe { Self::load(core::ptr::read_volatile(&raw const __cubit_launch_block_length)) }
    }

    /// The table a launch block's bytes end with (see `load`).
    pub fn from_block(block: &[u8]) -> Option<Table> {
        let length = block.len();
        if length < TRAILER_BYTES || word_at(block, length - 8) != ENVELOPE_MAGIC {
            return None;
        }
        let table_bytes = usize::try_from(word_at(block, length - TRAILER_BYTES)).ok()?;
        if table_bytes > MAXIMUM_TABLE_BYTES {
            return None;
        }
        let padded = (table_bytes + 7) / 8 * 8;
        if padded > length - TRAILER_BYTES {
            return None;
        }
        let first = length - TRAILER_BYTES - padded;
        let mut table = Table { bytes: [0; MAXIMUM_TABLE_BYTES], count: 0 };
        // One copy: everything below reads the copy only.
        table.bytes[..table_bytes].copy_from_slice(&block[first..first + table_bytes]);
        table.count = table.validate(table_bytes)?;
        Some(table)
    }

    fn validate(&self, length: usize) -> Option<usize> {
        let b = &self.bytes;
        if length < HEADER_BYTES
            || u32::from_le_bytes([b[0], b[1], b[2], b[3]]) != TABLE_MAGIC
            || u16::from_le_bytes([b[4], b[5]]) != FORMAT_VERSION
        {
            return None;
        }
        let count = usize::from(u16::from_le_bytes([b[6], b[7]]));
        if count > MAXIMUM_ENTRIES || length != HEADER_BYTES + count * ENTRY_BYTES {
            return None;
        }
        for index in 0..count {
            let e = &b[HEADER_BYTES + index * ENTRY_BYTES..HEADER_BYTES + (index + 1) * ENTRY_BYTES];
            let name_length = usize::from(e[NAME_LENGTH_AT]);
            let handle = word_at(e, HANDLE_AT);
            let state_ok = match e[STATE_AT] {
                STATE_GRANTED => handle != 0,
                STATE_ABSENT => handle == 0 && e[NEED_AT] == NEED_OPTIONAL,
                _ => false,
            };
            if !(1..=LAST_KIND).contains(&e[KIND_AT])
                || !(NEED_REQUIRED..=NEED_OPTIONAL).contains(&e[NEED_AT])
                || !state_ok
                || !(1..=NAME_BYTES).contains(&name_length)
            {
                return None;
            }
        }
        Some(count)
    }

    /// The handle word of the granted entry `name` of `kind`.
    fn granted(&self, name: &str, kind: Kind) -> Option<u64> {
        for index in 0..self.count {
            let e = &self.bytes[HEADER_BYTES + index * ENTRY_BYTES..HEADER_BYTES + (index + 1) * ENTRY_BYTES];
            let name_length = usize::from(e[NAME_LENGTH_AT]);
            if &e[NAME_AT..NAME_AT + name_length] == name.as_bytes() {
                return if e[KIND_AT] == kind as u8 && e[STATE_AT] == STATE_GRANTED {
                    Some(word_at(e, HANDLE_AT))
                } else {
                    None
                };
            }
        }
        None
    }

    /// The endpoint granted as `name` (a service, render or network entry).
    pub fn endpoint(&self, name: &str) -> Option<Endpoint> {
        [Kind::Endpoint, Kind::Render, Kind::Network]
            .into_iter()
            .find_map(|kind| self.granted(name, kind))
            .map(Endpoint)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::vec::Vec;

    fn entry(kind: Kind, need: u8, state: u8, handle: u64, name: &str) -> Vec<u8> {
        let mut e = std::vec![0u8; ENTRY_BYTES];
        e[KIND_AT] = kind as u8;
        e[NEED_AT] = need;
        e[STATE_AT] = state;
        e[NAME_LENGTH_AT] = name.len() as u8;
        e[HANDLE_AT..HANDLE_AT + 8].copy_from_slice(&handle.to_le_bytes());
        e[NAME_AT..NAME_AT + name.len()].copy_from_slice(name.as_bytes());
        e
    }

    fn block(payload: &[u8], entries: &[Vec<u8>]) -> Vec<u8> {
        let mut table = Vec::new();
        table.extend_from_slice(&TABLE_MAGIC.to_le_bytes());
        table.extend_from_slice(&FORMAT_VERSION.to_le_bytes());
        table.extend_from_slice(&(entries.len() as u16).to_le_bytes());
        table.extend_from_slice(&[1, 0, 0, 0, 0, 0, 0, 0]);
        for e in entries {
            table.extend_from_slice(e);
        }
        let mut b = payload.to_vec();
        b.extend_from_slice(&table);
        while (b.len() - payload.len()) % 8 != 0 {
            b.push(0);
        }
        b.extend_from_slice(&(table.len() as u64).to_le_bytes());
        b.extend_from_slice(&ENVELOPE_MAGIC.to_le_bytes());
        b
    }

    const CLOCK: u64 = (1 << 32) | 5;

    #[test]
    fn finds_granted_entries_by_name_and_kind() {
        let b = block(b"args", &[
            entry(Kind::Endpoint, NEED_REQUIRED, STATE_GRANTED, CLOCK, "clock"),
            entry(Kind::Endpoint, NEED_OPTIONAL, STATE_ABSENT, 0, "mixer"),
            entry(Kind::Notification, NEED_REQUIRED, STATE_GRANTED, CLOCK + 1, "registration"),
        ]);
        let t = Table::from_block(&b).unwrap();
        assert_eq!(t.endpoint("clock").map(|e| e.0), Some(CLOCK));
        assert!(t.endpoint("mixer").is_none());
        assert!(t.endpoint("registration").is_none());
        assert!(t.endpoint("clocks").is_none());
    }

    #[test]
    fn rejects_malformed_tables() {
        let good = block(b"", &[entry(Kind::Endpoint, NEED_REQUIRED, STATE_GRANTED, CLOCK, "clock")]);
        assert!(Table::from_block(&good).is_some());
        // No trailer, wrong magic, a granted entry without a handle, an
        // absent required entry, a length that disagrees with the count.
        assert!(Table::from_block(&good[..good.len() - 8]).is_none());
        let mut bad = good.clone();
        bad[0] ^= 1;
        assert!(Table::from_block(&bad).is_none());
        let zero = block(b"", &[entry(Kind::Endpoint, NEED_REQUIRED, STATE_GRANTED, 0, "clock")]);
        assert!(Table::from_block(&zero).is_none());
        let absent = block(b"", &[entry(Kind::Endpoint, NEED_REQUIRED, STATE_ABSENT, 0, "clock")]);
        assert!(Table::from_block(&absent).is_none());
        let mut short = good.clone();
        let n = short.len();
        short[n - 16..n - 8].copy_from_slice(&((HEADER_BYTES + ENTRY_BYTES + 8) as u64).to_le_bytes());
        assert!(Table::from_block(&short).is_none());
    }

    #[test]
    fn a_block_without_a_table_has_none() {
        assert!(Table::from_block(b"only arguments here").is_none());
        assert!(Table::from_block(b"").is_none());
    }
}

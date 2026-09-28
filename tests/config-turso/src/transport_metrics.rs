//! Opt-in diagnostic wrapper; no counters, locks or clocks in normal builds.
//! Measures the borrowed Transport call, not DMA/device-only time. Single
//! NativeIO owner serializes calls. Snapshots are taken outside measured work.
use crate::native_io::{Operation, Transport, TransportError};
use std::{
    num::NonZeroUsize,
    sync::{Arc, Mutex},
};

pub const OPERATIONS: [Operation; 8] = [
    Operation::OpenExisting,
    Operation::OpenCreate,
    Operation::Close,
    Operation::Read,
    Operation::Write,
    Operation::Size,
    Operation::Resize,
    Operation::Flush,
];

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Entry {
    pub calls: u64,
    pub ticks: u64,
    pub bytes: u64,
    pub errors: u64,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Snapshot {
    pub entries: [Entry; OPERATIONS.len()],
    pub vectors: u64,
}
impl Snapshot {
    pub fn difference(self, earlier: Self) -> Self {
        let mut result = Self::default();
        for (index, current) in self.entries.iter().enumerate() {
            let old = earlier.entries[index];
            result.entries[index] = Entry {
                calls: current.calls.checked_sub(old.calls).unwrap(),
                ticks: current.ticks.checked_sub(old.ticks).unwrap(),
                bytes: current.bytes.checked_sub(old.bytes).unwrap(),
                errors: current.errors.checked_sub(old.errors).unwrap(),
            };
        }
        result.vectors = self.vectors.checked_sub(earlier.vectors).unwrap();
        result
    }
    pub fn entry(&self, operation: Operation) -> Entry {
        self.entries[OPERATIONS.iter().position(|op| *op == operation).unwrap()]
    }
    pub fn ticks(&self) -> u64 {
        self.entries.iter().map(|item| item.ticks).sum()
    }
}

#[derive(Clone, Default)]
pub struct Monitor(Arc<Mutex<Snapshot>>);
impl Monitor {
    pub fn snapshot(&self) -> Snapshot {
        *self.0.lock().unwrap()
    }
}

pub struct Meter<T> {
    inner: T,
    counter: fn() -> u64,
    monitor: Monitor,
}
impl<T> Meter<T> {
    pub fn new(inner: T, counter: fn() -> u64) -> (Self, Monitor) {
        let monitor = Monitor::default();
        (
            Self {
                inner,
                counter,
                monitor: monitor.clone(),
            },
            monitor,
        )
    }
    fn record(
        &self,
        operation: Operation,
        start: u64,
        end: u64,
        result: &Result<u64, TransportError>,
        vector: bool,
    ) {
        let mut snapshot = self.monitor.0.lock().unwrap();
        let index = OPERATIONS.iter().position(|op| *op == operation).unwrap();
        let entry = &mut snapshot.entries[index];
        entry.calls += 1;
        entry.ticks += end.checked_sub(start).expect("transport counter regressed");
        if let Ok(length) = result {
            if matches!(operation, Operation::Read | Operation::Write) {
                // Actual reported bytes, including short results; not the
                // requested count, and never mistaken handles/sizes for bytes.
                entry.bytes += length;
            }
        } else {
            entry.errors += 1;
        }
        snapshot.vectors += u64::from(vector);
    }
}
impl<T: Transport> Transport for Meter<T> {
    fn transfer_capacity(&self) -> NonZeroUsize {
        self.inner.transfer_capacity()
    }
    fn call(
        &mut self,
        operation: Operation,
        handle: u64,
        position: u64,
        input: &[u8],
        output: &mut [u8],
    ) -> Result<u64, TransportError> {
        let start = (self.counter)();
        let result = self.inner.call(operation, handle, position, input, output);
        let end = (self.counter)();
        self.record(operation, start, end, &result, false);
        result
    }
    fn write_vectored(
        &mut self,
        handle: u64,
        position: u64,
        parts: &[&[u8]],
    ) -> Result<u64, TransportError> {
        let start = (self.counter)();
        let result = self.inner.write_vectored(handle, position, parts);
        let end = (self.counter)();
        self.record(Operation::Write, start, end, &result, true);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};
    fn counter() -> u64 {
        static CLOCK: AtomicU64 = AtomicU64::new(0);
        CLOCK.fetch_add(10, Ordering::Relaxed)
    }
    struct Fixture;
    impl Transport for Fixture {
        fn transfer_capacity(&self) -> NonZeroUsize {
            NonZeroUsize::new(64).unwrap()
        }
        fn call(
            &mut self,
            op: Operation,
            handle: u64,
            position: u64,
            input: &[u8],
            output: &mut [u8],
        ) -> Result<u64, TransportError> {
            assert_eq!((handle, position), (7, 11));
            match op {
                Operation::Read => {
                    output[..2].copy_from_slice(b"OK");
                    Ok(2)
                }
                Operation::Write => {
                    assert_eq!(input, b"abc");
                    Ok(2)
                }
                Operation::Flush => Err(TransportError::Failed(9)),
                _ => Ok(37),
            }
        }
        fn write_vectored(
            &mut self,
            handle: u64,
            position: u64,
            parts: &[&[u8]],
        ) -> Result<u64, TransportError> {
            assert_eq!((handle, position), (7, 11));
            assert_eq!(parts, &[b"a".as_slice(), b"bc".as_slice()]);
            Ok(3)
        }
    }
    #[test]
    fn exact_forwarding_and_accounting() {
        let (mut meter, monitor) = Meter::new(Fixture, counter);
        assert_eq!(meter.transfer_capacity().get(), 64);
        let baseline = monitor.snapshot();
        let mut bytes = [0; 2];
        assert_eq!(meter.call(Operation::Read, 7, 11, &[], &mut bytes), Ok(2));
        assert_eq!(&bytes, b"OK");
        assert_eq!(meter.call(Operation::Write, 7, 11, b"abc", &mut []), Ok(2));
        assert_eq!(meter.write_vectored(7, 11, &[b"a", b"bc"]), Ok(3));
        assert_eq!(
            meter.call(Operation::Flush, 7, 11, &[], &mut []),
            Err(TransportError::Failed(9))
        );
        for op in [
            Operation::OpenExisting,
            Operation::OpenCreate,
            Operation::Close,
            Operation::Size,
            Operation::Resize,
        ] {
            assert_eq!(meter.call(op, 7, 11, &[], &mut []), Ok(37));
        }
        let delta = monitor.snapshot().difference(baseline);
        assert_eq!(
            delta.entry(Operation::Read),
            Entry {
                calls: 1,
                ticks: 10,
                bytes: 2,
                errors: 0
            }
        );
        assert_eq!(
            delta.entry(Operation::Write),
            Entry {
                calls: 2,
                ticks: 20,
                bytes: 5,
                errors: 0
            }
        );
        assert_eq!(
            delta.entry(Operation::Flush),
            Entry {
                calls: 1,
                ticks: 10,
                bytes: 0,
                errors: 1
            }
        );
        assert_eq!(delta.entry(Operation::Size).bytes, 0);
        assert_eq!(delta.vectors, 1);
        assert_eq!(delta.ticks(), 90);
        assert_eq!(delta.difference(delta), Snapshot::default());
    }
}

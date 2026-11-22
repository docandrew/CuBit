//! Synchronous Turso File adapter over a scoped CuBit transport.
//! The native transport supplies actual exclusive handles; hosted tests replace
//! only that boundary. No ambient paths, advisory locks, or deferred callbacks.
use std::num::NonZeroUsize;
use std::sync::{Arc, Mutex};
use turso_core::io::{
    FileSyncType,
    clock::{MonotonicInstant, WallClockInstant},
};
use turso_core::{Buffer, Clock, Completion, CompletionError, File, IO, LimboError, OpenFlags};

// Bounded descriptor batching, not a payload-size limit or filesystem ABI.
pub const MAX_WRITE_SEGMENTS: usize = 32;

// Scalar Ada/Rust bridge ABI, not new CuBit IPC opcodes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum Operation {
    OpenExisting,
    OpenCreate,
    Close,
    Read,
    Write,
    Size,
    Resize,
    Flush,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransportError {
    /// Definite admission denial; no acquired handle or uncertain mutation.
    Rejected(u32),
    Failed(u32),
}

/// Every successful open MUST return a kernel/service-enforced deny-sharing
/// handle. Calls consume input/copy output synchronously, retaining no pointers.
/// Close remains available for cleanup after a data error. Implementations must
/// serialize grant ownership and prevent reuse after an uncertain completion.
pub trait Transport: Send + 'static {
    /// Maximum bytes in one Read/Write, fixed throughout this transport's
    /// lifetime. Nonzero by construction; the owner also checks this boundary.
    fn transfer_capacity(&self) -> NonZeroUsize;
    /// One positioned write from nonempty borrowed slices, summed length <=
    /// transfer_capacity and at most MAX_WRITE_SEGMENTS. Copy directly into
    /// owned transport memory; do not retain slices or concatenate elsewhere.
    fn write_vectored(
        &mut self,
        handle: u64,
        position: u64,
        parts: &[&[u8]],
    ) -> Result<u64, TransportError>;
    fn call(
        &mut self,
        operation: Operation,
        handle: u64,
        position: u64,
        input: &[u8],
        output: &mut [u8],
    ) -> Result<u64, TransportError>;
}

struct State<T> {
    transport: T,
    failed: bool,
}
pub struct NativeIO<T> {
    state: Arc<Mutex<State<T>>>,
    allowed: Box<[Box<str>]>,
}

fn error(message: &str) -> LimboError {
    LimboError::InternalError(message.into())
}

impl<T: Transport> NativeIO<T> {
    /// Copy the trusted startup allow-list. Its storage need not be static:
    /// boot configuration may be discarded after constructing the adapter.
    /// This is additional narrowing, never a substitute for FS authority.
    pub fn new(transport: T, allowed: &[&str]) -> Self {
        Self {
            state: Arc::new(Mutex::new(State {
                transport,
                failed: false,
            })),
            allowed: allowed.iter().map(|path| Box::<str>::from(*path)).collect(),
        }
    }
}
impl<T: Transport> Clock for NativeIO<T> {
    fn current_time_monotonic(&self) -> MonotonicInstant {
        MonotonicInstant::now()
    }
    fn current_time_wall_clock(&self) -> WallClockInstant {
        WallClockInstant::now()
    }
}
impl<T: Transport> IO for NativeIO<T> {
    fn open_file(
        &self,
        path: &str,
        flags: OpenFlags,
        _direct: bool,
    ) -> turso_core::Result<Arc<dyn File>> {
        // `direct` is a platform cache hint, not a durability request. CuBit
        // uses the same explicit grant-backed path either way; sync is separate.
        if flags.contains(OpenFlags::ReadOnly)
            || (flags.bits() & !(OpenFlags::Create | OpenFlags::NoLock).bits()) != 0
            || !self.allowed.iter().any(|allowed| allowed.as_ref() == path)
        {
            return Err(error(
                "native storage: unsupported open or unconfigured path",
            ));
        }
        let mut state = self
            .state
            .lock()
            .map_err(|_| error("native storage mutex poisoned"))?;
        if state.failed {
            return Err(error("native storage requires recovery"));
        }
        // NoLock does not weaken the lifetime-exclusive filesystem hold; WAL
        // byte locking/shared-WAL coordination is deliberately not offered.
        let op = if flags.contains(OpenFlags::Create) {
            Operation::OpenCreate
        } else {
            Operation::OpenExisting
        };
        let handle = match state.transport.call(op, 0, 0, path.as_bytes(), &mut []) {
            Ok(handle) => handle,
            Err(e) => {
                if matches!(e, TransportError::Failed(_)) {
                    state.failed = true;
                }
                return Err(error(&format!("native open rejected: {e:?}")));
            }
        };
        if handle == 0 {
            state.failed = true;
            return Err(error("native open returned null handle"));
        }
        Ok(Arc::new(NativeFile {
            state: self.state.clone(),
            handle,
        }))
    }
    fn remove_file(&self, _path: &str) -> turso_core::Result<()> {
        Err(error("native storage does not expose unlink"))
    }
    fn file_id(&self, path: &str) -> turso_core::Result<turso_core::io::FileId> {
        if !self.allowed.iter().any(|allowed| allowed.as_ref() == path) {
            return Err(error("native storage: unconfigured identity path"));
        }
        // Engine-local identity only, never authority. Real inode exclusion is
        // checked by the filesystem on every open (including aliases).
        Ok(turso_core::io::FileId::from_path_hash(path))
    }
}

struct NativeFile<T: Transport> {
    state: Arc<Mutex<State<T>>>,
    handle: u64,
}
impl<T: Transport> NativeFile<T> {
    fn run<R>(
        &self,
        work: impl FnOnce(&mut T) -> Result<R, TransportError>,
    ) -> turso_core::Result<R> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| error("native storage mutex poisoned"))?;
        if state.failed {
            return Err(error("native storage requires recovery"));
        }
        match work(&mut state.transport) {
            Ok(value) => Ok(value),
            Err(e) => {
                state.failed = true;
                Err(error(&format!("native storage failure: {e:?}")))
            }
        }
    }
    fn finish(c: Completion, result: turso_core::Result<usize>) -> turso_core::Result<Completion> {
        // Invoked AFTER releasing the transport mutex: callbacks may reenter.
        match result {
            Ok(count) => c.complete(count as i32),
            Err(_) => c.error(CompletionError::IOError(
                std::io::ErrorKind::Other,
                "native storage operation failed",
            )),
        }
        Ok(c)
    }
    fn range(pos: u64, len: usize) -> turso_core::Result<()> {
        if len > i32::MAX as usize || pos.checked_add(len as u64).is_none() {
            Err(error("native I/O range exceeds completion representation"))
        } else {
            Ok(())
        }
    }
    fn write_buffers(&self, pos: u64, buffers: &[Arc<Buffer>]) -> turso_core::Result<usize> {
        let total = buffers
            .iter()
            .try_fold(0usize, |n, b| n.checked_add(b.len()))
            .ok_or_else(|| error("native vectored size overflow"))?;
        Self::range(pos, total)?;
        self.run(|transport| {
            let capacity = transport.transfer_capacity().get();
            let mut done = 0;
            let mut parts: [&[u8]; MAX_WRITE_SEGMENTS] = [&[]; MAX_WRITE_SEGMENTS];
            let mut used = 0;
            let mut filled = 0;
            for buffer in buffers {
                let mut input = buffer.as_slice();
                while !input.is_empty() {
                    let length = input.len().min(capacity - filled);
                    parts[used] = &input[..length];
                    used += 1;
                    filled += length;
                    input = &input[length..];
                    if filled == capacity || used == MAX_WRITE_SEGMENTS {
                        Self::write_parts(
                            transport,
                            self.handle,
                            pos + done as u64,
                            &parts[..used],
                            filled,
                        )?;
                        done += filled;
                        used = 0;
                        filled = 0;
                    }
                }
            }
            if used != 0 {
                Self::write_parts(
                    transport,
                    self.handle,
                    pos + done as u64,
                    &parts[..used],
                    filled,
                )?;
                done += filled;
            }
            Ok(done)
        })
    }

    fn write_parts(
        transport: &mut T,
        handle: u64,
        pos: u64,
        parts: &[&[u8]],
        bytes: usize,
    ) -> Result<(), TransportError> {
        let written = if parts.len() == 1 {
            transport.call(Operation::Write, handle, pos, parts[0], &mut [])?
        } else {
            transport.write_vectored(handle, pos, parts)?
        };
        if written != bytes as u64 {
            return Err(TransportError::Failed(u32::MAX));
        }
        Ok(())
    }
}
impl<T: Transport> Drop for NativeFile<T> {
    fn drop(&mut self) {
        if let Ok(mut state) = self.state.lock() {
            // Close is handle cleanup, not further data I/O or a flush retry.
            if !matches!(
                state
                    .transport
                    .call(Operation::Close, self.handle, 0, &[], &mut []),
                Ok(0)
            ) {
                state.failed = true;
            }
        }
    }
}
impl<T: Transport> File for NativeFile<T> {
    fn lock_file(&self, exclusive: bool) -> turso_core::Result<()> {
        if !exclusive {
            return Err(error(
                "native file has a lifetime-exclusive hold; shared locks unsupported",
            ));
        }
        self.run(|_| Ok(())) // only constructed after successful exclusive open
    }
    fn unlock_file(&self) -> turso_core::Result<()> {
        Err(error(
            "drop the native file to release its lifetime-exclusive hold",
        ))
    }
    fn size(&self) -> turso_core::Result<u64> {
        self.run(|t| t.call(Operation::Size, self.handle, 0, &[], &mut []))
    }
    fn pread(&self, pos: u64, c: Completion) -> turso_core::Result<Completion> {
        let buffer = c.as_read().buf_arc();
        let result = Self::range(pos, buffer.len()).and_then(|()| {
            self.run(|t| {
                let capacity = t.transfer_capacity().get();
                let output = buffer.as_mut_slice();
                output.fill(0);
                let mut done = 0;
                for chunk in output.chunks_mut(capacity) {
                    let read =
                        t.call(Operation::Read, self.handle, pos + done as u64, &[], chunk)?;
                    if read > chunk.len() as u64 {
                        return Err(TransportError::Failed(u32::MAX));
                    }
                    done += read as usize;
                    if read < chunk.len() as u64 {
                        break;
                    }
                }
                Ok(done)
            })
        });
        Self::finish(c, result)
    }
    fn pwrite(
        &self,
        pos: u64,
        buffer: Arc<Buffer>,
        c: Completion,
    ) -> turso_core::Result<Completion> {
        Self::finish(c, self.write_buffers(pos, &[buffer]))
    }
    fn pwritev(
        &self,
        pos: u64,
        buffers: Vec<Arc<Buffer>>,
        c: Completion,
    ) -> turso_core::Result<Completion> {
        // One parent completion, including failures in any segment. Do not
        // inherit upstream's success-only child aggregation.
        Self::finish(c, self.write_buffers(pos, &buffers))
    }
    fn sync(&self, c: Completion, _kind: FileSyncType) -> turso_core::Result<Completion> {
        Self::finish(
            c,
            self.run(|t| {
                if t.call(Operation::Flush, self.handle, 0, &[], &mut [])? != 0 {
                    return Err(TransportError::Failed(u32::MAX));
                }
                Ok(0)
            }),
        )
    }
    fn truncate(&self, len: u64, c: Completion) -> turso_core::Result<Completion> {
        Self::finish(
            c,
            self.run(|t| {
                if t.call(Operation::Resize, self.handle, len, &[], &mut [])? != 0 {
                    return Err(TransportError::Failed(u32::MAX));
                }
                Ok(0)
            }),
        )
    }
}

#[cfg(test)]
mod tests;

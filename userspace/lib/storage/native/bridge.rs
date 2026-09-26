use config_storage::native_io::{MAX_WRITE_SEGMENTS, Operation, Transport, TransportError};
use std::num::NonZeroUsize;
use std::sync::atomic::{AtomicBool, Ordering};

const OK: u32 = 0xf000;
static CLAIMED: AtomicBool = AtomicBool::new(false);
#[repr(C)]
#[derive(Clone, Copy)]
struct Part {
    data: *const u8,
    length: u64,
}
unsafe extern "C" {
    fn cubit_storage_transfer_capacity() -> u64;
    fn cubit_storage_initialize(endpoint: u64) -> u32;
    fn cubit_storage_execute(
        op: u32,
        handle: u64,
        pos: u64,
        data: *mut u8,
        len: u64,
        value: *mut u64,
    ) -> u32;
    fn cubit_storage_shutdown() -> u32;
    fn cubit_storage_write_vector(
        handle: u64,
        pos: u64,
        parts: *const Part,
        count: u64,
        value: *mut u64,
    ) -> u32;
}

// Non-cloneable singleton. NativeIO owns it behind one mutex shared by all files.
pub struct Bridge(NonZeroUsize);
impl Bridge {
    pub fn new(endpoint: u64) -> Self {
        assert!(
            !CLAIMED.swap(true, Ordering::AcqRel),
            "native storage bridge already claimed"
        );
        // SAFETY: one-time initialization with scalar endpoint reference.
        assert_eq!(unsafe { cubit_storage_initialize(endpoint) }, OK);
        // SAFETY: scalar constant from the same Ada implementation that owns
        // and bounds the loan. Do not duplicate its capacity in Rust.
        let bytes = usize::try_from(unsafe { cubit_storage_transfer_capacity() })
            .expect("storage capacity exceeds addressable memory");
        Self(NonZeroUsize::new(bytes).expect("empty storage transfer buffer"))
    }
}
impl Transport for Bridge {
    fn transfer_capacity(&self) -> NonZeroUsize {
        self.0
    }
    fn write_vectored(
        &mut self,
        handle: u64,
        position: u64,
        parts: &[&[u8]],
    ) -> Result<u64, TransportError> {
        if parts.is_empty() || parts.len() > MAX_WRITE_SEGMENTS {
            return Err(TransportError::Failed(u32::MAX));
        }
        let mut descriptors = [Part {
            data: core::ptr::null(),
            length: 0,
        }; MAX_WRITE_SEGMENTS];
        for (descriptor, part) in descriptors.iter_mut().zip(parts) {
            *descriptor = Part {
                data: part.as_ptr(),
                length: part.len() as u64,
            };
        }
        let mut value = 0;
        // SAFETY: C layout matches Ada's Part; all slices and the descriptor
        // array live until return. Ada validates lengths and copies each part
        // directly into its owned grant before IPC; no pointer escapes.
        let code = unsafe {
            cubit_storage_write_vector(
                handle,
                position,
                descriptors.as_ptr(),
                parts.len() as u64,
                &mut value,
            )
        };
        if code == OK {
            Ok(value)
        } else {
            Err(TransportError::Failed(code))
        }
    }
    fn call(
        &mut self,
        op: Operation,
        handle: u64,
        pos: u64,
        input: &[u8],
        output: &mut [u8],
    ) -> Result<u64, TransportError> {
        let (pointer, length) = match op {
            Operation::OpenExisting | Operation::OpenCreate | Operation::Write
                if output.is_empty() =>
            {
                (input.as_ptr().cast_mut(), input.len())
            }
            Operation::Read if input.is_empty() => (output.as_mut_ptr(), output.len()),
            Operation::Close | Operation::Size | Operation::Resize | Operation::Flush
                if input.is_empty() && output.is_empty() =>
            {
                (core::ptr::null_mut(), 0)
            }
            _ => return Err(TransportError::Failed(u32::MAX)),
        };
        let mut value = 0;
        // SAFETY: Execute borrows only until return; Ada copies through its
        // own aligned grant buffer, never grants/retains either Rust pointer.
        // Input operations read only; Read_Data writes only into output.
        let code = unsafe {
            cubit_storage_execute(op as u32, handle, pos, pointer, length as u64, &mut value)
        };
        if code == OK {
            Ok(value)
        } else if matches!(op, Operation::OpenExisting | Operation::OpenCreate)
            && matches!(code, 0xf002 | 0xf003 | 0xf007 | 0xf00b | 0xf00f)
        {
            // The bridge recognizes these exact filesystem admission denials:
            // no space, read-only, access denied, absent, and sharing conflict.
            Err(TransportError::Rejected(code))
        } else {
            Err(TransportError::Failed(code))
        }
    }
}
impl Drop for Bridge {
    fn drop(&mut self) {
        // SAFETY: all NativeFile references are gone before transport Drop.
        // The Ada buffer is never freed/reused even on incomplete retirement.
        let status = unsafe { cubit_storage_shutdown() };
        if status == OK {
            cubit::debug_write("STORAGE: filesystem grant retired\n");
        } else {
            cubit::debug_write("STORAGE: filesystem grant retirement failed\n");
        }
    }
}

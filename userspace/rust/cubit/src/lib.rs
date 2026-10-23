//! Minimal native CuBit boundary. No libc, descriptors, or ambient stdout.
#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]

#[cfg(feature = "alloc")]
extern crate alloc;
#[cfg(test)]
extern crate std;

use core::arch::asm;

pub mod startup;

// The shared program entry (CAP-003), for programs without the std runtime
// (whose moto-rt carries it instead): see the `start` feature.
#[cfg(feature = "start")]
core::arch::global_asm!(include_str!("../../../runtime/start/cubit_start.S"));
pub mod sync;
#[cfg(feature = "alloc")]
pub mod thread;

#[repr(u64)]
enum Syscall {
    Exit = 0,
    GrowHeap = 8,
    DebugWrite = 12,
    CallViaEndpointCapability = 128,
    ThreadCreate = 90,
    ThreadExit = 91,
    FutexWait = 92,
    FutexWake = 93,
}

/// Early runtime diagnostics through CuBit's existing debug output route.
/// This is not ambient application stdout or a replacement for typed stdlog.
pub fn debug_write(text: &str) {
    // SAFETY: the kernel copies the borrowed bytes synchronously; no pointer
    // survives the syscall. Descriptor 1 is the existing Ada debug route.
    unsafe {
        asm!("syscall",
            inlateout("rax") Syscall::DebugWrite as u64 => _,
            in("rdi") 1_u64, in("rsi") text.as_ptr(), in("rdx") text.len(),
            lateout("rcx") _, lateout("r11") _, options(nostack));
    }
}

/// A synchronous call's deadline: none (docs/ipc-fastpath.md, "Call
/// deadlines"; the kernel takes an absolute monotonic millisecond).
const WAIT_FOREVER: u64 = u64::MAX;

/// Two-argument raw machine boundary for native operations.
///
/// Safety: the caller must satisfy the selected syscall's pointer/lifetime
/// requirements. Do not mark this asm `nomem`, `readonly`, or `preserves_flags`.
unsafe fn syscall2(number: Syscall, arg0: u64, arg1: u64) -> u64 {
    let result;
    unsafe {
        asm!("syscall",
            inlateout("rax") number as u64 => result,
            in("rdi") arg0, in("rsi") arg1,
            lateout("rcx") _, lateout("r11") _,
            options(nostack));
    }
    result
}

/// Three-argument raw machine boundary; same contract as `syscall2`.
#[allow(dead_code)]
unsafe fn syscall3(number: Syscall, arg0: u64, arg1: u64, arg2: u64) -> u64 {
    let result;
    unsafe {
        asm!("syscall",
            inlateout("rax") number as u64 => result,
            in("rdi") arg0, in("rsi") arg1, in("rdx") arg2,
            lateout("rcx") _, lateout("r11") _,
            options(nostack));
    }
    result
}

/// Five-argument raw machine boundary (R10 carries the fourth argument);
/// same contract as `syscall2`.
#[allow(dead_code)]
unsafe fn syscall5(number: Syscall, arg0: u64, arg1: u64, arg2: u64, arg3: u64,
                   arg4: u64) -> u64 {
    let result;
    unsafe {
        asm!("syscall",
            inlateout("rax") number as u64 => result,
            in("rdi") arg0, in("rsi") arg1, in("rdx") arg2, in("r10") arg3,
            in("r8") arg4,
            lateout("rcx") _, lateout("r11") _,
            options(nostack));
    }
    result
}

/// The tag's flag byte (Capability_ABI): the protocol's own four flags in
/// the low nibble, the IPC layer's in the high nibble. A program sets
/// protocol flags only; the kernel's (transferred, refused, offered) are
/// read-only here, and a send claiming them is refused.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(transparent)]
pub struct TagFlags(u8);

impl TagFlags {
    pub const NONE: Self = Self(0);
    const PROTOCOL_MASK: u8 = 0x0F;
    const ACCEPTS_CAPABILITY: u8 = 0x10;
    const CAPABILITY_TRANSFERRED: u8 = 0x20;
    const CAPABILITY_REFUSED: u8 = 0x40;
    const CAPABILITY_OFFERED: u8 = 0x80;

    /// Protocol flags; `None` when they do not fit in four bits.
    pub const fn with_protocol(protocol: u8) -> Option<Self> {
        if protocol & !Self::PROTOCOL_MASK == 0 { Some(Self(protocol)) } else { None }
    }
    pub const fn protocol(self) -> u8 { self.0 & Self::PROTOCOL_MASK }
    pub const fn accepts_capability(self) -> bool { self.0 & Self::ACCEPTS_CAPABILITY != 0 }
    pub const fn capability_transferred(self) -> bool { self.0 & Self::CAPABILITY_TRANSFERRED != 0 }
    pub const fn capability_refused(self) -> bool { self.0 & Self::CAPABILITY_REFUSED != 0 }
    pub const fn capability_offered(self) -> bool { self.0 & Self::CAPABILITY_OFFERED != 0 }
    pub const fn bits(self) -> u8 { self.0 }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct MessageTag {
    pub label: u32,
    pub length: u8,
    pub flags: TagFlags,
    pub reserved: u16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct Message {
    pub tag: MessageTag,
    // Only the kernel's stamp at the receiver is authenticated.
    pub authority_tag: u64,
    pub words: [u64; 4],
}

impl Message {
    pub const fn new(label: u32, length: u8, words: [u64; 4]) -> Self {
        Self {
            tag: MessageTag {
                label,
                length,
                flags: TagFlags::NONE,
                reserved: 0,
            },
            authority_tag: 0,
            words,
        }
    }
}

const _: () = {
    assert!(core::mem::size_of::<MessageTag>() == 8);
    assert!(core::mem::size_of::<Message>() == 48);
    assert!(core::mem::align_of::<Message>() == 8);
    assert!(core::mem::offset_of!(Message, authority_tag) == 8);
    assert!(core::mem::offset_of!(Message, words) == 16);
};

#[derive(Debug, PartialEq, Eq)]
pub enum CallError {
    InvalidMessage,
    Rejected,
    /// The current kernel returns a null tag for missing/stale authority or
    /// unavailable peers. This is not a detailed policy-denial diagnostic.
    Unavailable,
    MalformedReply,
}

/// An endpoint handle (CAP-002): a name in this process's capability table,
/// obtained only from the startup table (`startup::Table::endpoint`). Every
/// call is checked by the kernel against the entry the handle names.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Endpoint(pub(crate) u64);

impl Endpoint {
    pub fn call(&self, mut message: Message) -> Result<Message, CallError> {
        if message.tag.length > 4 || message.tag.reserved != 0 {
            return Err(CallError::InvalidMessage);
        }
        // SAFETY: a uniquely borrowed, initialized, correctly aligned 48-byte
        // message lives until the synchronous syscall returns. The kernel
        // copies the request before blocking; it retains no pointer afterward.
        let returned_tag = unsafe {
            syscall3(
                Syscall::CallViaEndpointCapability,
                self.0,
                &mut message as *mut Message as u64,
                WAIT_FOREVER,
            )
        };
        if returned_tag == u64::MAX {
            return Err(CallError::Rejected);
        }
        if returned_tag == 0 {
            return Err(CallError::Unavailable);
        }
        let expected_tag = u64::from(message.tag.label)
            | (u64::from(message.tag.length) << 32)
            | (u64::from(message.tag.flags.bits()) << 40)
            | (u64::from(message.tag.reserved) << 48);
        if expected_tag != returned_tag || message.tag.length > 4 {
            return Err(CallError::MalformedReply);
        }
        Ok(message)
    }
}

pub fn exit(code: u64) -> ! {
    loop {
        // SAFETY: exit takes a value, not a userspace pointer.
        unsafe {
            syscall2(Syscall::Exit, code, 0);
        }
    }
}

/// Query (zero) or grow the process heap; returns the old break. Failure leaves
/// the old break, data mappings and allocations intact. Growth is zero-filled.
///
/// # Safety
/// Serialize all process-heap users, including other runtimes. Successful
/// growth transfers the new byte range to the caller; do not create overlapping
/// ownership from a query or hand out bytes beyond the requested range.
pub unsafe fn grow_heap(bytes: usize) -> Option<core::ptr::NonNull<u8>> {
    let result = unsafe { syscall2(Syscall::GrowHeap, bytes as u64, 0) };
    if result == u64::MAX {
        return None;
    }
    core::ptr::NonNull::new(core::ptr::with_exposed_provenance_mut(result as usize))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn invalid_messages_never_enter_the_kernel() {
        let endpoint = Endpoint((1 << 32) | 25);
        assert_eq!(
            endpoint.call(Message::new(1, 5, [0; 4])),
            Err(CallError::InvalidMessage)
        );
        let mut message = Message::new(1, 1, [0; 4]);
        message.tag.reserved = 1;
        assert_eq!(endpoint.call(message), Err(CallError::InvalidMessage));
    }
}

#[cfg(test)]
mod tag_flags_tests {
    use super::TagFlags;

    #[test]
    fn protocol_flags_are_four_bits_and_never_ipc() {
        for value in 0u8..=15 {
            let flags = TagFlags::with_protocol(value).expect("four bits fit");
            assert_eq!(flags.protocol(), value);
            assert!(!flags.accepts_capability() && !flags.capability_transferred());
            assert!(!flags.capability_refused() && !flags.capability_offered());
        }
        for value in 16u8..=255 {
            assert!(TagFlags::with_protocol(value).is_none(), "{value} would set an IPC bit");
        }
    }
}

//! Minimal native CuBit boundary. No libc, descriptors, or ambient stdout.
#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]

use core::arch::asm;

#[repr(u64)]
enum Syscall {
    Exit = 0,
    GrowHeap = 8,
    DebugWrite = 12,
    CallViaEndpointCapability = 41,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct MessageTag {
    pub label: u32,
    pub length: u8,
    pub flags: u8,
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
                flags: 0,
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

/// A reference to a slot, NOT minted authority. Every call is checked by the
/// kernel. Constructing or copying this number cannot grant access.
pub struct EndpointSlot(u64);

impl EndpointSlot {
    pub const fn new(slot: u64) -> Option<Self> {
        if slot <= 63 { Some(Self(slot)) } else { None }
    }

    pub fn call(&self, mut message: Message) -> Result<Message, CallError> {
        if message.tag.length > 4 || message.tag.reserved != 0 {
            return Err(CallError::InvalidMessage);
        }
        // SAFETY: a uniquely borrowed, initialized, correctly aligned 48-byte
        // message lives until the synchronous syscall returns. The kernel
        // copies the request before blocking; it retains no pointer afterward.
        let returned_tag = unsafe {
            syscall2(
                Syscall::CallViaEndpointCapability,
                self.0,
                &mut message as *mut Message as u64,
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
            | (u64::from(message.tag.flags) << 40)
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
    fn slots_are_references_not_minted_authority() {
        assert!(EndpointSlot::new(0).is_some());
        assert!(EndpointSlot::new(63).is_some());
        assert!(EndpointSlot::new(64).is_none());
        assert!(EndpointSlot::new(u64::MAX).is_none());
    }

    #[test]
    fn invalid_messages_never_enter_the_kernel() {
        let endpoint = EndpointSlot::new(25).unwrap();
        assert_eq!(
            endpoint.call(Message::new(1, 5, [0; 4])),
            Err(CallError::InvalidMessage)
        );
        let mut message = Message::new(1, 1, [0; 4]);
        message.tag.reserved = 1;
        assert_eq!(endpoint.call(message), Err(CallError::InvalidMessage));
    }
}

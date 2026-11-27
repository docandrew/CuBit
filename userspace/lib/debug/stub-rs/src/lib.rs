// CuBit
// Copyright (C) 2026 Jon Andrew
// SPDX-License-Identifier: GPL-3.0-or-later

//! The cooperative debug stub for Rust programs (DBG-001 preview).
//!
//! The same protocol as the Ada stub, CuBit.Debug_Stub
//! (userspace/runtime/gnat/cubit-debug_protocol.ads): the program registers
//! the debug-target service role; a debugger that requests the service calls
//! it; while answering that call, the stub grants the program's own pages
//! READ-ONLY to that debugger (the kernel's current-reply rule). Observed
//! threads are made with [`spawn`]; each calls [`checkpoint`] where it may be
//! stopped, and parks there while a debugger asks for a pause. A cooperative
//! stop, not a kernel one: a thread that never reaches a checkpoint is never
//! stopped.
//!
//! A Rust shim over the protocol rather than the Ada stub through a C ABI:
//! the Ada stub needs the GNAT runtime's elaboration and its FS-based thread
//! area, and Rust std owns FS for its thread-locals, so linking them into one
//! program would put two runtimes on one thread pointer.

use core::arch::asm;
use core::cell::UnsafeCell;
use std::sync::atomic::{AtomicU32, AtomicU64, AtomicUsize, Ordering};
use std::time::{Duration, Instant};

/// CuBit.Debug_Protocol, word for word (the Ada package is the definition).
#[allow(dead_code)] // the whole layout, though the stub writes only some of it
mod protocol {
    pub const SERVICE_ROLE: u64 = 47;
    pub const OP_ATTACH: u32 = 0x0D01;
    pub const OP_SHARE: u32 = 0x0D02;
    pub const OP_PAUSE: u32 = 0x0D03;
    pub const OP_RESUME: u32 = 0x0D04;
    pub const OP_DETACH: u32 = 0x0D05;
    pub const REPLY_OK: u32 = 0xF000;
    pub const REPLY_NOT_YET: u32 = 0xF001;
    pub const REPLY_REFUSED: u32 = 0xF002;
    pub const PAGE_BYTES: usize = 4096;
    pub const SHARE_PAGES_CEILING: u64 = 256;
    pub const PAUSE_WAIT_CEILING_MS: u64 = 2_000;
    pub const GRANTS_PER_DEBUGGER_CEILING: usize = 512;
    pub const MAGIC: u32 = 0x4742_4443;
    pub const VERSION: u32 = 1;
    pub const MAGIC_AT: usize = 0;
    pub const VERSION_AT: usize = 4; // u16 version, u16 thread count
    pub const PROCESS_AT: usize = 8;
    pub const STOP_EPOCH_AT: usize = 24;
    pub const PAUSE_REQUESTED_AT: usize = 32;
    pub const PARKED_COUNT_AT: usize = 36;
    pub const BUILD_ID_LENGTH_AT: usize = 40;
    pub const BUILD_ID_AT: usize = 44;
    pub const BUILD_ID_CEILING: usize = 20;
    pub const NAME_AT: usize = 64;
    pub const NAME_BYTES: usize = 32;
    pub const IMAGE_LOW_AT: usize = 96;
    pub const IMAGE_HIGH_AT: usize = 104;
    pub const THREADS_AT: usize = 128;
    pub const THREAD_CEILING: usize = 16;
    pub const THREAD_BYTES: usize = 128;
    pub const T_ID_AT: usize = 0;
    pub const T_STATE_AT: usize = 4;
    pub const T_SEQUENCE_AT: usize = 8;
    pub const T_CHECKPOINTS_AT: usize = 16;
    pub const T_NAME_AT: usize = 24;
    pub const T_NAME_BYTES: usize = 24;
    pub const T_STACK_LOW_AT: usize = 48;
    pub const T_STACK_HIGH_AT: usize = 56;
    pub const T_REGISTERS_AT: usize = 64; // RIP RSP RBP RBX R12 R13 R14 R15
    pub const THREAD_RUNNING: u32 = 0;
    pub const THREAD_PARKED: u32 = 1;
    pub const THREAD_ENDED: u32 = 2;
    const _: () = assert!(THREADS_AT + THREAD_CEILING * THREAD_BYTES <= PAGE_BYTES);
}
use protocol as p;

/// CuBit system calls (CuBit.Kernel_ABI, CuBit.Handles.Kernel, Wait_ABI,
/// Futex_ABI, CuBit.Messages): the numbers and forms the Ada stub uses.
mod sys {
    use core::arch::asm;
    pub const EXIT: u64 = 0;
    pub const REPLY_WAIT: u64 = 48;
    /// IPC revision 2 (Notification_ABI): Wait on one object and Wake it.
    pub const WAIT_ONE: u64 = 150;
    pub const WAKE: u64 = 151;
    pub const CREATE_GRANT_FOR_PROCESS: u64 = 102;
    pub const OWNED_GRANT_GENERATION: u64 = 108;
    pub const REVOKE_GRANT_REFERENCE: u64 = 110;
    pub const WAIT: u64 = 149;
    pub const REGISTER_DRIVER: u64 = 2000;
    pub const MESSAGES_SOURCES: u64 = 0b11; // Requests + Events
    pub const PRIVATE_WORD: u64 = 2; // Notification_ABI.Private_Word
    pub const FOREVER: u64 = u64::MAX;
    pub const GRANT_READ_ONLY: u64 = 0;
    pub const REFUSED: u64 = u64::MAX;
    pub const MAXIMUM_GLOBAL_SLOT: u64 = 256 * 4096 - 1;
    pub const MAXIMUM_GENERATION: u64 = u32::MAX as u64;
    pub const WIRE_FIELD_BASE: u64 = 1 << 32;
    /// The kernel's Stop notice (CuBit.Messages.Is_Kernel_Stop).
    pub const CONTROL_EVENT_LABEL: u32 = 0x010E;
    pub const CONTROL_EVENT_LENGTH: u8 = 2;
    pub const STOP_CONTROL_KIND: u64 = 1;
    pub const STOPPED_EXIT_CODE: u64 = 0;
    /// Readiness_Publication.Identity_Address: this process's identity word.
    pub const IDENTITY_ADDRESS: usize = 0x0000_5B00_0000_0000 + 6 * 4096 + 40;

    /// One system call (RAX number; RDI, RSI, RDX, R10, R8, R9 arguments).
    /// Safety: pointers passed must satisfy the call's contract.
    pub unsafe fn call(number: u64, a0: u64, a1: u64, a2: u64, a3: u64, a4: u64) -> u64 {
        let result: u64;
        unsafe {
            asm!("syscall",
                 inlateout("rax") number => result,
                 in("rdi") a0, in("rsi") a1, in("rdx") a2,
                 inlateout("r10") a3 => _, inlateout("r8") a4 => _,
                 lateout("r9") _, lateout("r12") _, lateout("rcx") _, lateout("r11") _,
                 options(nostack));
        }
        result
    }
}

/// The 48-byte IPC message (CuBit.Messages.Message).
#[repr(C)]
#[derive(Clone, Copy, Default)]
struct Message {
    tag: u64, // label (32), length (8), flags (8), reserved (16)
    authority: u64,
    words: [u64; 4],
}
const _: () = assert!(core::mem::size_of::<Message>() == 48);

impl Message {
    fn answer(label: u32, w0: u64, w1: u64) -> Self {
        const ANSWER_WORDS: u64 = 2;
        Message { tag: u64::from(label) | (ANSWER_WORDS << 32), authority: 0, words: [w0, w1, 0, 0] }
    }
    fn label(&self) -> u32 { self.tag as u32 }
    fn length(&self) -> u8 { (self.tag >> 32) as u8 }
}

#[repr(C, align(4096))]
struct StatePage(UnsafeCell<[u8; p::PAGE_BYTES]>);
// The page is written through atomics only.
unsafe impl Sync for StatePage {}
static STATE: StatePage = StatePage(UnsafeCell::new([0; p::PAGE_BYTES]));

fn word64(offset: usize) -> &'static AtomicU64 {
    debug_assert!(offset % 8 == 0 && offset + 8 <= p::PAGE_BYTES);
    // SAFETY: an aligned word inside the static page, accessed atomically.
    unsafe { &*(STATE.0.get().cast::<u8>().add(offset).cast::<AtomicU64>()) }
}
fn word32(offset: usize) -> &'static AtomicU32 {
    debug_assert!(offset % 4 == 0 && offset + 4 <= p::PAGE_BYTES);
    // SAFETY: as word64.
    unsafe { &*(STATE.0.get().cast::<u8>().add(offset).cast::<AtomicU32>()) }
}
fn put_bytes(offset: usize, width: usize, text: &[u8]) {
    // Byte writes through 32-bit words: read, merge, store.
    for i in 0..width {
        let b = if i < text.len() { text[i] } else { b' ' };
        let at = offset + i;
        let w = word32(at & !3);
        let shift = (at & 3) * 8;
        let old = w.load(Ordering::Relaxed);
        w.store((old & !(0xFF << shift)) | (u32::from(b) << shift), Ordering::Relaxed);
    }
}

/// Bytes of one observed thread's stack, and how far above the spawned
/// closure's frame the bounds reach (std's thread start frames).
const STACK_BYTES: usize = 256 * 1024;
const STACK_TOP_SLACK: u64 = 4096;
const PAUSE_POLL: Duration = Duration::from_millis(1);

unsafe extern "C" {
    /// The GNU build-id note (userspace/c/link.ld). LLD, which links Rust
    /// programs, loads no ELF header to find it by.
    static __cubit_build_id_note: u8;
}

static REGISTERED: AtomicUsize = AtomicUsize::new(0);
static RESUME_WORD: AtomicU32 = AtomicU32::new(0);
static STARTED: AtomicU32 = AtomicU32::new(0);

fn record_at(index: usize) -> usize { p::THREADS_AT + index * p::THREAD_BYTES }

/// The program's GNU build-id, from its note (16 bytes of header, "GNU",
/// then the 20-byte id).
fn publish_build_id() {
    const GNU: u32 = 0x0055_4E47;
    const NT_GNU_BUILD_ID: u32 = 3;
    const GNU_NAME_BYTES: u32 = 4;
    const NOTE_HEADER_BYTES: usize = 16;
    let note = &raw const __cubit_build_id_note;
    // SAFETY: the link keeps the note in the read-only segment, at least
    // its header and id long when the link passed --build-id=sha1; the
    // header is checked before the id is read.
    unsafe {
        let r32 = |at: usize| core::ptr::read_unaligned(note.add(at).cast::<u32>());
        if r32(0) != GNU_NAME_BYTES || r32(4) != p::BUILD_ID_CEILING as u32
            || r32(8) != NT_GNU_BUILD_ID || r32(12) != GNU
        {
            return;
        }
        let mut id = [0u8; p::BUILD_ID_CEILING];
        for (b, slot) in id.iter_mut().enumerate() {
            *slot = core::ptr::read_volatile(note.add(NOTE_HEADER_BYTES + b));
        }
        put_bytes(p::BUILD_ID_AT, p::BUILD_ID_CEILING, &id);
        word32(p::BUILD_ID_LENGTH_AT).store(p::BUILD_ID_CEILING as u32, Ordering::Release);
    }
}

/// Start the stub: register the debug-target role with the program's
/// (notification "debug-target" Notify_Access.Manage "registration")
/// startup handle, and run the stub's server thread.
pub fn start(program: &str) -> bool {
    if STARTED.swap(1, Ordering::AcqRel) != 0 {
        return false;
    }
    word32(p::MAGIC_AT).store(p::MAGIC, Ordering::Relaxed);
    word32(p::VERSION_AT).store(p::VERSION, Ordering::Relaxed);
    // SAFETY: the kernel maps this process's readiness page read-only.
    let identity = unsafe { core::ptr::read_volatile(sys::IDENTITY_ADDRESS as *const u64) };
    word64(p::PROCESS_AT).store(identity, Ordering::Relaxed);
    put_bytes(p::NAME_AT, p::NAME_BYTES, program.as_bytes());
    publish_build_id();
    let Some(registration) = cubit::startup::Table::from_start().and_then(|t| t.notification("registration")) else {
        return false;
    };
    // SAFETY: two words, no pointers.
    let registered = unsafe { sys::call(sys::REGISTER_DRIVER, p::SERVICE_ROLE, registration, 0, 0, 0) };
    if registered == sys::REFUSED {
        return false;
    }
    std::thread::Builder::new().name("debug-stub".into()).spawn(server).is_ok()
}

/// Run `body` on a new observed thread named `name`.
pub fn spawn<F: FnOnce() + Send + 'static>(name: &'static str, body: F) -> bool {
    let index = REGISTERED.load(Ordering::Acquire);
    if index >= p::THREAD_CEILING || STARTED.load(Ordering::Acquire) == 0 {
        return false;
    }
    REGISTERED.store(index + 1, Ordering::Release);
    let base = record_at(index);
    word32(base + p::T_ID_AT).store(index as u32 + 1, Ordering::Relaxed);
    put_bytes(base + p::T_NAME_AT, p::T_NAME_BYTES, name.as_bytes());
    let spawned = std::thread::Builder::new().name(name.into()).stack_size(STACK_BYTES).spawn(move || {
        // The stack's bounds, from this frame: std gives no others. The
        // engine stops unwinding at the upper one.
        let marker = 0u8;
        let here = core::ptr::addr_of!(marker) as u64;
        let page = p::PAGE_BYTES as u64;
        let high = (here + STACK_TOP_SLACK + page - 1) / page * page;
        word64(base + p::T_STACK_LOW_AT).store(high - STACK_BYTES as u64, Ordering::Relaxed);
        word64(base + p::T_STACK_HIGH_AT).store(high, Ordering::Release);
        body();
        word32(base + p::T_STATE_AT).store(p::THREAD_ENDED, Ordering::Release);
    });
    if spawned.is_err() {
        return false;
    }
    let count = (REGISTERED.load(Ordering::Acquire) as u32) << 16;
    word32(p::VERSION_AT).store(p::VERSION | count, Ordering::Release);
    true
}

/// Record this thread's registers for the debugger, and park while a pause
/// is asked. A thread `spawn` did not make returns at once.
#[inline(never)]
pub fn checkpoint() {
    let mut regs = [0u64; 8];
    // One capture point: the unwinder starts at this RIP with these
    // registers, and this function's CFI says where its caller's are.
    // SAFETY: stores eight words into `regs`, which lives across the asm.
    unsafe {
        asm!("lea rax, [rip]",
             "mov [{p}], rax",
             "mov [{p} + 8], rsp",
             "mov [{p} + 16], rbp",
             "mov [{p} + 24], rbx",
             "mov [{p} + 32], r12",
             "mov [{p} + 40], r13",
             "mov [{p} + 48], r14",
             "mov [{p} + 56], r15",
             p = in(reg) regs.as_mut_ptr(),
             out("rax") _,
             options(nostack, preserves_flags));
    }
    let rsp = regs[1];
    let found = (0..REGISTERED.load(Ordering::Acquire)).find(|&i| {
        let base = record_at(i);
        rsp >= word64(base + p::T_STACK_LOW_AT).load(Ordering::Acquire)
            && rsp < word64(base + p::T_STACK_HIGH_AT).load(Ordering::Acquire)
    });
    let Some(index) = found else { return };
    let base = record_at(index);
    // Odd while writing: a reader that sees the same even value before and
    // after its copy has a consistent record.
    let sequence = word64(base + p::T_SEQUENCE_AT);
    sequence.fetch_add(1, Ordering::AcqRel);
    for (i, value) in regs.iter().enumerate() {
        word64(base + p::T_REGISTERS_AT + 8 * i).store(*value, Ordering::Relaxed);
    }
    word64(base + p::T_CHECKPOINTS_AT).fetch_add(1, Ordering::Relaxed);
    sequence.fetch_add(1, Ordering::AcqRel);

    let state = word32(base + p::T_STATE_AT);
    while word32(p::PAUSE_REQUESTED_AT).load(Ordering::Acquire) != 0 {
        let seen = RESUME_WORD.load(Ordering::Acquire);
        state.store(p::THREAD_PARKED, Ordering::Release);
        if word32(p::PAUSE_REQUESTED_AT).load(Ordering::Acquire) == 0 {
            break;
        }
        // No lost wake: the kernel sleeps only while the word still holds
        // `seen`, and a resume changes it before it wakes.
        // SAFETY: a private futex word of this process.
        unsafe {
            sys::call(sys::WAIT_ONE, sys::PRIVATE_WORD, RESUME_WORD.as_ptr() as u64,
                      u64::from(seen), sys::FOREVER, 0);
        }
    }
    state.store(p::THREAD_RUNNING, Ordering::Release);
}

/// Grants made, per debugger, so a detach can revoke them.
struct Made {
    holder: u64,
    slot: u64,
    generation: u64,
}

fn grant_to(made: &mut Vec<Made>, holder: u64, base: u64, pages: u64) -> Option<u64> {
    if made.len() >= p::GRANTS_PER_DEBUGGER_CEILING {
        return None;
    }
    // READ-ONLY, to the caller being answered (the current-reply rule).
    // SAFETY: the kernel checks that the pages are this process's own.
    let slot = unsafe { sys::call(sys::CREATE_GRANT_FOR_PROCESS, holder, base, pages, sys::GRANT_READ_ONLY, 0) };
    if slot == sys::REFUSED || slot > sys::MAXIMUM_GLOBAL_SLOT {
        return None;
    }
    // SAFETY: one word.
    let generation = unsafe { sys::call(sys::OWNED_GRANT_GENERATION, slot, 0, 0, 0, 0) };
    if generation == 0 || generation > sys::MAXIMUM_GENERATION {
        // SAFETY: revoking this process's own new grant.
        unsafe { sys::call(sys::REVOKE_GRANT_REFERENCE, slot, generation, 0, 0, 0) };
        return None;
    }
    made.push(Made { holder, slot, generation });
    Some(generation * sys::WIRE_FIELD_BASE + slot)
}

fn revoke_all(made: &mut Vec<Made>, holder: u64) {
    made.retain(|m| {
        if m.holder == holder {
            // SAFETY: this process's own grant.
            unsafe { sys::call(sys::REVOKE_GRANT_REFERENCE, m.slot, m.generation, 0, 0, 0) };
            false
        } else {
            true
        }
    });
}

fn all_parked() -> bool {
    (0..REGISTERED.load(Ordering::Acquire))
        .all(|i| word32(record_at(i) + p::T_STATE_AT).load(Ordering::Acquire) != p::THREAD_RUNNING)
}

fn handle(made: &mut Vec<Made>, from: u64, message: &Message) -> Message {
    let refused = Message::answer(p::REPLY_REFUSED, 0, 0);
    match message.label() {
        p::OP_ATTACH => {
            // A debugger attaching again starts afresh.
            revoke_all(made, from);
            match grant_to(made, from, STATE.0.get() as u64, 1) {
                Some(reference) => Message::answer(p::REPLY_OK, reference,
                                                   word64(p::PROCESS_AT).load(Ordering::Relaxed)),
                None => refused,
            }
        }
        p::OP_SHARE => {
            let (base, pages) = (message.words[0], message.words[1]);
            if base % p::PAGE_BYTES as u64 != 0 || !(1..=p::SHARE_PAGES_CEILING).contains(&pages) {
                return refused;
            }
            match grant_to(made, from, base, pages) {
                Some(reference) => Message::answer(p::REPLY_OK, reference, pages),
                None => refused,
            }
        }
        p::OP_PAUSE => {
            let wait = Duration::from_millis(message.words[0].min(p::PAUSE_WAIT_CEILING_MS));
            let until = Instant::now() + wait;
            word32(p::PAUSE_REQUESTED_AT).store(1, Ordering::Release);
            loop {
                if all_parked() {
                    let epoch = word64(p::STOP_EPOCH_AT).fetch_add(1, Ordering::AcqRel) + 1;
                    word32(p::PARKED_COUNT_AT).store(REGISTERED.load(Ordering::Acquire) as u32, Ordering::Release);
                    return Message::answer(p::REPLY_OK, epoch, 0);
                }
                if Instant::now() >= until {
                    return Message::answer(p::REPLY_NOT_YET, 0, 0);
                }
                std::thread::sleep(PAUSE_POLL);
            }
        }
        p::OP_RESUME => {
            word32(p::PAUSE_REQUESTED_AT).store(0, Ordering::Release);
            word32(p::PARKED_COUNT_AT).store(0, Ordering::Release);
            RESUME_WORD.fetch_add(1, Ordering::AcqRel);
            // SAFETY: a private futex word of this process.
            unsafe {
                sys::call(sys::WAKE, sys::PRIVATE_WORD, RESUME_WORD.as_ptr() as u64,
                          p::THREAD_CEILING as u64, 0, 0);
            }
            Message::answer(p::REPLY_OK, 0, 0)
        }
        p::OP_DETACH => {
            revoke_all(made, from);
            Message::answer(p::REPLY_OK, 0, 0)
        }
        _ => refused,
    }
}

fn is_kernel_stop(from: u64, message: &Message) -> bool {
    from == 0 && message.label() == sys::CONTROL_EVENT_LABEL
        && message.length() == sys::CONTROL_EVENT_LENGTH && message.words[0] == sys::STOP_CONTROL_KIND
}

fn server() {
    let mut made: Vec<Made> = Vec::with_capacity(p::GRANTS_PER_DEBUGGER_CEILING);
    let mut message = Message::default();
    // SAFETY: the kernel writes one 48-byte message into `message`.
    let mut from = unsafe {
        sys::call(sys::WAIT, sys::MESSAGES_SOURCES, sys::FOREVER, &raw mut message as u64, 0, 0)
    };
    loop {
        if is_kernel_stop(from, &message) {
            // SAFETY: ends the process.
            unsafe { sys::call(sys::EXIT, sys::STOPPED_EXIT_CODE, 0, 0, 0, 0) };
        }
        message = if from == sys::REFUSED || from == 0 {
            Message::default()
        } else {
            handle(&mut made, from, &message)
        };
        let reply_to = if from == sys::REFUSED { 0 } else { from };
        // SAFETY: the kernel reads the answer from `message` and writes the
        // next request into it.
        from = unsafe { sys::call(sys::REPLY_WAIT, reply_to, &raw mut message as u64, 0, 0, 0) };
    }
}

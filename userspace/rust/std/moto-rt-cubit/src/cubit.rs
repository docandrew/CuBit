//! CuBit implementation of the moto-rt runtime table (docs/rust-std.md).
//!
//! Motor OS supplies this table from a VDSO; on CuBit the program carries it,
//! and `install` fills it before `main` over CuBit syscalls:
//! THREAD_CREATE/THREAD_EXIT, FUTEX_WAIT/FUTEX_WAKE, SBRK, GETTIME, WRITE
//! and EXIT (docs/threads.md). Operations CuBit does not offer yet (files,
//! sockets, processes, polling) return E_NOT_IMPLEMENTED.

use core::arch::{asm, global_asm};
use core::sync::atomic::{AtomicU32, AtomicU64, AtomicUsize, Ordering};

use crate::RtVdsoVtable;
use crate::error::{E_NOT_IMPLEMENTED, E_OK, ErrorCode};

/// The runtime table (Motor OS maps it at a fixed address; here it is data).
pub static VTABLE: RtVdsoVtable = unsafe { core::mem::zeroed() };

// --- optional hooks -----------------------------------------------------------
//
// A program may link these (the Turso probe and the Config storage worker
// do, in userspace/services/config-storage/std_hooks.rs) to route std's
// system allocator, clock and randomness through CuBit services; without
// them the runtime uses syscalls. Weak: absent symbols read as None.
unsafe extern "C" {
    #[linkage = "extern_weak"]
    static cubit_std_allocate: Option<unsafe extern "C" fn(usize, usize) -> *mut u8>;
    #[linkage = "extern_weak"]
    static cubit_std_release: Option<unsafe extern "C" fn(*mut u8, usize, usize)>;
    #[linkage = "extern_weak"]
    static cubit_std_time: Option<unsafe extern "C" fn(bool, *mut u64, *mut u32) -> bool>;
    #[linkage = "extern_weak"]
    static cubit_std_random: Option<unsafe extern "C" fn(*mut u8, usize) -> bool>;
}

fn hooked_allocator() -> Option<(unsafe extern "C" fn(usize, usize) -> *mut u8,
                                 unsafe extern "C" fn(*mut u8, usize, usize))> {
    match unsafe { (cubit_std_allocate, cubit_std_release) } {
        (Some(a), Some(r)) => Some((a, r)),
        _ => None,
    }
}

// --- lazy installation ----------------------------------------------------------

static INSTALL_STATE: AtomicU32 = AtomicU32::new(0); // 0 none, 1 installing, 2 done

/// Fill the table on first use. std calls it before main through moto-rt's
/// init; a Rust library linked into a program with another entry point
/// (Ada, C) reaches it on its first std call instead.
#[inline]
pub fn ensure_installed() {
    if INSTALL_STATE.load(Ordering::Acquire) == 2 {
        return;
    }
    if INSTALL_STATE.compare_exchange(0, 1, Ordering::Acquire, Ordering::Acquire).is_ok() {
        install();
        INSTALL_STATE.store(2, Ordering::Release);
    } else {
        while INSTALL_STATE.load(Ordering::Acquire) != 2 {
            core::hint::spin_loop();
        }
    }
}

// --- syscalls ---------------------------------------------------------------

const SYS_EXIT: u64 = 0;
const SYS_SBRK: u64 = 8;
const SYS_WRITE: u64 = 12;
const SYS_GETTIME: u64 = 27;
const SYS_THREAD_CREATE: u64 = 90;
const SYS_THREAD_EXIT: u64 = 91;
const SYS_FUTEX_WAIT: u64 = 92;
const SYS_FUTEX_WAKE: u64 = 93;

const FUTEX_TIMED_OUT: u64 = 2;
const FOREVER: u64 = u64::MAX;

#[inline(always)]
unsafe fn syscall(n: u64, a0: u64, a1: u64, a2: u64, a3: u64, a4: u64) -> u64 {
    let r;
    unsafe {
        asm!("syscall",
             inlateout("rax") n => r,
             in("rdi") a0, in("rsi") a1, in("rdx") a2, in("r10") a3, in("r8") a4,
             lateout("rcx") _, lateout("r11") _,
             options(nostack));
    }
    r
}

fn debug_write(bytes: &[u8]) {
    unsafe { syscall(SYS_WRITE, 1, bytes.as_ptr() as u64, bytes.len() as u64, 0, 0) };
}

fn now_ms() -> u64 {
    unsafe { syscall(SYS_GETTIME, 0, 0, 0, 0, 0) }
}

fn futex_wait_raw(word: *const AtomicU32, expected: u32, deadline_ms: u64) -> u64 {
    unsafe { syscall(SYS_FUTEX_WAIT, word as u64, expected as u64, deadline_ms, 0, 0) }
}

fn futex_wake_raw(word: *const AtomicU32, count: u64) -> u64 {
    unsafe { syscall(SYS_FUTEX_WAKE, word as u64, count, 0, 0, 0) }
}

// --- entry ------------------------------------------------------------------

// The loader enters at _start with RSP at the stack top and no return
// address. Align it for a SysV call into std's motor_start, which runs
// moto-rt's init (install below) and then main.
//
// Both symbols are weak defaults: a Rust library linked into a program with
// its own entry (the Ada-hosted Turso probe and Config storage worker) keeps
// that entry, and has no Rust `main` for motor_start to name.
global_asm!(
    ".weak _start",
    "_start:",
    "    and rsp, -16",
    "    xor ebp, ebp",
    "    call motor_start",
    "    ud2",
    ".weak main",
    "main:",
    "    ud2",
);

// --- a futex mutex for the runtime's own state ------------------------------

struct Lock(AtomicU32);

impl Lock {
    const fn new() -> Self {
        Lock(AtomicU32::new(0))
    }
    fn lock(&self) {
        if self.0.compare_exchange(0, 1, Ordering::Acquire, Ordering::Relaxed).is_ok() {
            return;
        }
        while self.0.swap(2, Ordering::Acquire) != 0 {
            futex_wait_raw(&self.0, 2, FOREVER);
        }
    }
    fn unlock(&self) {
        if self.0.swap(0, Ordering::Release) == 2 {
            futex_wake_raw(&self.0, 1);
        }
    }
}

// --- memory: dlmalloc over sbrk ---------------------------------------------

struct Sbrk;

unsafe impl dlmalloc::Allocator for Sbrk {
    fn alloc(&self, size: usize) -> (*mut u8, usize, u32) {
        let size = (size + 0xFFFF) & !0xFFFF;
        let base = unsafe { syscall(SYS_SBRK, size as u64, 0, 0, 0, 0) };
        if base == u64::MAX || base == 0 {
            (core::ptr::null_mut(), 0, 0)
        } else {
            (base as *mut u8, size, 0)
        }
    }
    fn remap(&self, _: *mut u8, _: usize, _: usize, _: bool) -> *mut u8 {
        core::ptr::null_mut()
    }
    fn free_part(&self, _: *mut u8, _: usize, _: usize) -> bool {
        false
    }
    fn free(&self, _: *mut u8, _: usize) -> bool {
        false
    }
    fn can_release_part(&self, _: u32) -> bool {
        false
    }
    fn allocates_zeros(&self) -> bool {
        true
    }
    fn page_size(&self) -> usize {
        4096
    }
}

static HEAP_LOCK: Lock = Lock::new();
static mut HEAP: dlmalloc::Dlmalloc<Sbrk> = dlmalloc::Dlmalloc::new_with_allocator(Sbrk);

#[allow(static_mut_refs)]
fn with_heap<R>(f: impl FnOnce(&mut dlmalloc::Dlmalloc<Sbrk>) -> R) -> R {
    HEAP_LOCK.lock();
    // SAFETY: serialized by HEAP_LOCK.
    let r = f(unsafe { &mut HEAP });
    HEAP_LOCK.unlock();
    r
}

extern "C" fn rt_alloc(size: u64, align: u64) -> u64 {
    if let Some((allocate, _)) = hooked_allocator() {
        return unsafe { allocate(size as usize, align as usize) } as u64;
    }
    with_heap(|h| unsafe { h.malloc(size as usize, align as usize) }) as u64
}

extern "C" fn rt_alloc_zeroed(size: u64, align: u64) -> u64 {
    if let Some((allocate, _)) = hooked_allocator() {
        let p = unsafe { allocate(size as usize, align as usize) };
        if !p.is_null() {
            unsafe { core::ptr::write_bytes(p, 0, size as usize) };
        }
        return p as u64;
    }
    with_heap(|h| unsafe { h.calloc(size as usize, align as usize) }) as u64
}

extern "C" fn rt_dealloc(ptr: u64, size: u64, align: u64) {
    if ptr == 0 {
        return;
    }
    if let Some((_, release)) = hooked_allocator() {
        return unsafe { release(ptr as *mut u8, size as usize, align as usize) };
    }
    with_heap(|h| unsafe { h.free(ptr as *mut u8, size as usize, align as usize) })
}

extern "C" fn rt_realloc(ptr: u64, size: u64, align: u64, new_size: u64) -> u64 {
    if let Some((allocate, release)) = hooked_allocator() {
        let p = unsafe { allocate(new_size as usize, align as usize) };
        if !p.is_null() {
            unsafe {
                core::ptr::copy_nonoverlapping(ptr as *const u8, p, size.min(new_size) as usize);
                release(ptr as *mut u8, size as usize, align as usize);
            }
        }
        return p as u64;
    }
    with_heap(|h| unsafe {
        h.realloc(ptr as *mut u8, size as usize, align as usize, new_size as usize)
    }) as u64
}

fn heap_alloc(size: usize, align: usize) -> *mut u8 {
    rt_alloc_zeroed(size as u64, align as u64) as *mut u8
}

fn heap_free(ptr: *mut u8, size: usize, align: usize) {
    rt_dealloc(ptr as u64, size as u64, align as u64)
}

// --- time: milliseconds as ticks (coarse; see docs/rust-std.md) -------------

const TICKS_PER_SEC: u64 = 1_000;

extern "C" fn rt_instant_now() -> u64 {
    // Never zero: moto-rt treats a zero Instant as "not a time".
    now_ms() + 1
}

extern "C" fn rt_ticks_to_nanos(ticks: u64, hi: *mut u64, lo: *mut u64) {
    let nanos = ticks as u128 * 1_000_000;
    unsafe {
        *hi = (nanos >> 64) as u64;
        *lo = nanos as u64;
    }
}

/// Wall-clock nanoseconds at a monotonic tick: CuBit's wall clock belongs to
/// the clock service, reached through the cubit_std_time hook. Without it
/// there is no honest answer, so this fails loudly (as the Turso port did)
/// rather than inventing a date.
extern "C" fn rt_abs_ticks_to_nanos(ticks: u64, hi: *mut u64, lo: *mut u64) {
    let Some(clock) = (unsafe { cubit_std_time }) else {
        debug_write(b"cubit-rt: SystemTime needs the clock service (cubit_std_time hook)\n");
        exit_process(102);
    };
    let (mut secs, mut nanos) = (0u64, 0u32);
    if !unsafe { clock(true, &mut secs, &mut nanos) } || nanos >= 1_000_000_000 {
        debug_write(b"cubit-rt: CuBit clock authority unavailable\n");
        exit_process(102);
    }
    let wall_now = secs as u128 * 1_000_000_000 + nanos as u128;
    let age_ms = rt_instant_now().saturating_sub(ticks) as u128;
    let wall = wall_now.saturating_sub(age_ms * 1_000_000);
    unsafe {
        *hi = (wall >> 64) as u64;
        *lo = wall as u64;
    }
}

extern "C" fn rt_nanos_to_ticks(nanos: u64) -> u64 {
    nanos.div_ceil(1_000_000)
}

/// Absolute monotonic ms deadline for a relative timeout in ns (MAX: none).
fn deadline_after_nanos(nanos: u64) -> u64 {
    if nanos == u64::MAX {
        FOREVER
    } else {
        now_ms().saturating_add(nanos.div_ceil(1_000_000))
    }
}

// --- futexes ------------------------------------------------------------------

extern "C" fn rt_futex_wait(futex: *const AtomicU32, expected: u32, timeout_nanos: u64) -> u32 {
    let deadline = deadline_after_nanos(timeout_nanos);
    // std's contract: false only on timeout; spurious returns are allowed.
    if futex_wait_raw(futex, expected, deadline) == FUTEX_TIMED_OUT { 0 } else { 1 }
}

extern "C" fn rt_futex_wake(futex: *const AtomicU32) -> u32 {
    (futex_wake_raw(futex, 1) > 0) as u32
}

extern "C" fn rt_futex_wake_all(futex: *const AtomicU32) {
    futex_wake_raw(futex, u64::MAX);
}

// --- thread-local storage: keys index a per-thread block at the FS base ----

const MAX_KEYS: usize = 256;

#[repr(C)]
struct TlsBlock {
    this: *mut TlsBlock,
    values: [*mut u8; MAX_KEYS],
}

static NEXT_KEY: AtomicUsize = AtomicUsize::new(1); // key 0 is never handed out
static DTORS: [AtomicU64; MAX_KEYS] = [const { AtomicU64::new(0) }; MAX_KEYS];
static mut MAIN_TLS: TlsBlock = TlsBlock { this: core::ptr::null_mut(), values: [core::ptr::null_mut(); MAX_KEYS] };

fn tls_block() -> *mut TlsBlock {
    let block: *mut TlsBlock;
    unsafe { asm!("mov {}, fs:[0]", out(reg) block, options(nostack, readonly, preserves_flags)) };
    block
}

fn new_tls_block() -> *mut TlsBlock {
    let block = heap_alloc(size_of::<TlsBlock>(), align_of::<TlsBlock>()) as *mut TlsBlock;
    if !block.is_null() {
        unsafe { (*block).this = block };
    }
    block
}

extern "C" fn rt_tls_create(dtor: u64) -> usize {
    let key = NEXT_KEY.fetch_add(1, Ordering::Relaxed);
    if key >= MAX_KEYS {
        debug_write(b"cubit-rt: out of thread-local keys\n");
        exit_process(101);
    }
    DTORS[key].store(dtor, Ordering::Release);
    key
}

extern "C" fn rt_tls_set(key: usize, value: *mut u8) {
    unsafe { (*tls_block()).values[key] = value };
}

extern "C" fn rt_tls_get(key: usize) -> *mut u8 {
    unsafe { (*tls_block()).values[key] }
}

extern "C" fn rt_tls_destroy(key: usize) {
    DTORS[key].store(0, Ordering::Release);
}

/// Run destructors for this thread's values, as POSIX does (a few passes,
/// since a destructor may set other keys).
fn run_tls_dtors() {
    let block = tls_block();
    for _ in 0..4 {
        let mut ran = false;
        for key in 1..NEXT_KEY.load(Ordering::Relaxed).min(MAX_KEYS) {
            let value = unsafe { (*block).values[key] };
            let dtor = DTORS[key].load(Ordering::Acquire);
            if !value.is_null() && dtor != 0 {
                unsafe { (*block).values[key] = core::ptr::null_mut() };
                let f: unsafe extern "C" fn(*mut u8) = unsafe { core::mem::transmute(dtor as usize) };
                unsafe { f(value) };
                ran = true;
            }
        }
        if !ran {
            break;
        }
    }
}

// --- threads ------------------------------------------------------------------

#[repr(C)]
struct ThreadRecord {
    /// Nonzero while the thread runs; the kernel clears and wakes it at exit.
    exit_word: AtomicU32,
    thread_fn: extern "C" fn(u64),
    arg: u64,
    stack: *mut u8,
    stack_size: usize,
    tls: *mut TlsBlock,
}

const MIN_STACK: usize = 256 * 1024;

extern "C" fn thread_entry(record: *mut ThreadRecord) -> ! {
    let (f, arg) = unsafe { ((*record).thread_fn, (*record).arg) };
    f(arg);
    run_tls_dtors();
    loop {
        unsafe { syscall(SYS_THREAD_EXIT, 0, 0, 0, 0, 0) };
    }
}

extern "C" fn rt_thread_spawn(thread_fn: extern "C" fn(u64), stack_size: usize, arg: u64) -> u64 {
    let stack_size = stack_size.max(MIN_STACK).next_multiple_of(4096);
    let stack = heap_alloc(stack_size, 4096);
    let tls = new_tls_block();
    let record = heap_alloc(size_of::<ThreadRecord>(), align_of::<ThreadRecord>()) as *mut ThreadRecord;
    if stack.is_null() || tls.is_null() || record.is_null() {
        return crate::error::E_OUT_OF_MEMORY as u64;
    }
    unsafe {
        record.write(ThreadRecord {
            exit_word: AtomicU32::new(1),
            thread_fn,
            arg,
            stack,
            stack_size,
            tls,
        });
    }
    // A SysV function expects RSP + 8 to be 16-byte aligned at entry.
    let rsp = (stack as u64 + stack_size as u64) - 8;
    let tid = unsafe {
        syscall(SYS_THREAD_CREATE, thread_entry as *const () as u64, rsp, record as u64,
                tls as u64, &(*record).exit_word as *const AtomicU32 as u64)
    };
    if tid == u64::MAX {
        heap_free(stack, stack_size, 4096);
        heap_free(tls as *mut u8, size_of::<TlsBlock>(), align_of::<TlsBlock>());
        heap_free(record as *mut u8, size_of::<ThreadRecord>(), align_of::<ThreadRecord>());
        return crate::error::E_OUT_OF_MEMORY as u64;
    }
    // The handle must be above u16::MAX (smaller values are error codes).
    record as u64
}

extern "C" fn rt_thread_join(handle: u64) -> ErrorCode {
    let record = handle as *mut ThreadRecord;
    let word = unsafe { &(*record).exit_word };
    loop {
        let v = word.load(Ordering::Acquire);
        if v == 0 {
            break;
        }
        futex_wait_raw(word, v, FOREVER);
    }
    // The thread is in the kernel for good: its stack and blocks are free.
    unsafe {
        heap_free((*record).stack, (*record).stack_size, 4096);
        heap_free((*record).tls as *mut u8, size_of::<TlsBlock>(), align_of::<TlsBlock>());
    }
    heap_free(record as *mut u8, size_of::<ThreadRecord>(), align_of::<ThreadRecord>());
    E_OK
}

extern "C" fn rt_thread_sleep(deadline_ticks: u64) {
    // A timed wait on a private word nobody wakes.
    let word = AtomicU32::new(0);
    let deadline_ms = deadline_ticks.saturating_sub(1);
    while now_ms() < deadline_ms {
        futex_wait_raw(&word, 0, deadline_ms);
    }
}

extern "C" fn rt_thread_yield() {
    let word = AtomicU32::new(0);
    futex_wait_raw(&word, 0, 0);
}

extern "C" fn rt_thread_set_name(_: *const u8, _: usize) -> ErrorCode {
    E_OK
}

// --- process ------------------------------------------------------------------

fn exit_process(code: i32) -> ! {
    loop {
        unsafe { syscall(SYS_EXIT, code as u64, 0, 0, 0, 0) };
    }
}

extern "C" fn rt_proc_exit(code: i32) -> ! {
    exit_process(code)
}

// --- stdio: stdout and stderr go to the debug output ------------------------

extern "C" fn rt_fs_write(fd: i32, buf: *const u8, len: usize) -> i64 {
    if fd == crate::FD_STDOUT || fd == crate::FD_STDERR {
        debug_write(unsafe { core::slice::from_raw_parts(buf, len) });
        len as i64
    } else {
        -(E_NOT_IMPLEMENTED as i64)
    }
}

extern "C" fn rt_fs_flush(_: i32) -> ErrorCode {
    E_OK
}

extern "C" fn rt_fs_is_terminal(_: i32) -> i32 {
    0
}

// --- utilities ------------------------------------------------------------------

extern "C" fn rt_fill_random_bytes(ptr: *mut u8, len: usize) {
    if let Some(random) = unsafe { cubit_std_random } {
        if unsafe { random(ptr, len) } {
            return;
        }
        debug_write(b"cubit-rt: CuBit random source unavailable\n");
        exit_process(103);
    }
    // RDRAND where available, else the TSC mixed through splitmix64. Enough
    // for HashMap keys; not a cryptographic source.
    let mut state: u64 = unsafe { core::arch::x86_64::_rdtsc() };
    for i in 0..len {
        if i % 8 == 0 {
            let mut r: u64 = 0;
            let ok: u8;
            unsafe { asm!("rdrand {r}", "setc {ok}", r = out(reg) r, ok = out(reg_byte) ok) };
            state = if ok != 0 { r } else {
                state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
                let mut z = state;
                z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
                z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
                z ^ (z >> 31)
            };
        }
        unsafe { *ptr.add(i) = (state >> ((i % 8) * 8)) as u8 };
    }
}

extern "C" fn rt_num_cpus() -> usize {
    4
}

extern "C" fn rt_log_to_kernel(ptr: *const u8, len: usize) {
    debug_write(unsafe { core::slice::from_raw_parts(ptr, len) });
    debug_write(b"\n");
}

/// Every operation CuBit does not provide yet. Signed results (byte counts,
/// descriptors) read a negative value as an error, and an ErrorCode reads
/// its truncation as a (non-OK) error. Entries whose result is a u64 handle
/// or address get explicit implementations instead.
extern "C" fn rt_not_implemented() -> i64 {
    -(E_NOT_IMPLEMENTED as i64)
}

/// No command-line arguments or environment yet (procmgr's launch message
/// will carry them).
extern "C" fn rt_proc_args() -> u64 {
    0
}

extern "C" fn rt_proc_getenv(_: *const u8, _: usize) -> u64 {
    u64::MAX
}

extern "C" fn rt_vdso_entry(_version: u64) {}

// --- installation -----------------------------------------------------------------

fn set(slot: &AtomicU64, f: *const ()) {
    slot.store(f as usize as u64, Ordering::Relaxed);
}

/// Fill the table and give the installing thread (the main thread) its
/// thread-local block. Called once, through ensure_installed.
#[allow(static_mut_refs)]
fn install() {
    let v = &VTABLE;
    // Every entry starts as "not implemented"; the table is a run of AtomicU64.
    let slots = unsafe {
        core::slice::from_raw_parts(v as *const RtVdsoVtable as *const AtomicU64,
                                    size_of::<RtVdsoVtable>() / 8)
    };
    for s in slots {
        set(s, rt_not_implemented as *const ());
    }
    set(&v.vdso_entry, rt_vdso_entry as *const ());
    v.vdso_bytes_sz.store(0, Ordering::Relaxed);

    set(&v.alloc, rt_alloc as *const ());
    set(&v.alloc_zeroed, rt_alloc_zeroed as *const ());
    set(&v.realloc, rt_realloc as *const ());
    set(&v.dealloc, rt_dealloc as *const ());

    set(&v.time_instant_now, rt_instant_now as *const ());
    set(&v.time_ticks_to_nanos, rt_ticks_to_nanos as *const ());
    set(&v.time_nanos_to_ticks, rt_nanos_to_ticks as *const ());
    v.time_ticks_in_sec.store(TICKS_PER_SEC, Ordering::Relaxed);
    set(&v.time_abs_ticks_to_nanos, rt_abs_ticks_to_nanos as *const ());

    set(&v.futex_wait, rt_futex_wait as *const ());
    set(&v.futex_wake, rt_futex_wake as *const ());
    set(&v.futex_wake_all, rt_futex_wake_all as *const ());

    set(&v.proc_exit, rt_proc_exit as *const ());
    set(&v.proc_args, rt_proc_args as *const ());
    set(&v.proc_get_full_env, rt_proc_args as *const ());
    set(&v.proc_getenv, rt_proc_getenv as *const ());

    set(&v.tls_create, rt_tls_create as *const ());
    set(&v.tls_set, rt_tls_set as *const ());
    set(&v.tls_get, rt_tls_get as *const ());
    set(&v.tls_destroy, rt_tls_destroy as *const ());

    set(&v.thread_spawn, rt_thread_spawn as *const ());
    set(&v.thread_sleep, rt_thread_sleep as *const ());
    set(&v.thread_yield, rt_thread_yield as *const ());
    set(&v.thread_set_name, rt_thread_set_name as *const ());
    set(&v.thread_join, rt_thread_join as *const ());

    set(&v.fs_write, rt_fs_write as *const ());
    set(&v.fs_flush, rt_fs_flush as *const ());
    set(&v.fs_is_terminal, rt_fs_is_terminal as *const ());

    set(&v.log_to_kernel, rt_log_to_kernel as *const ());
    set(&v.fill_random_bytes, rt_fill_random_bytes as *const ());
    set(&v.num_cpus, rt_num_cpus as *const ());

    // The main thread's thread-local block. Threads this runtime creates
    // get their own at THREAD_CREATE; nothing else on CuBit sets FS.
    unsafe {
        MAIN_TLS.this = &raw mut MAIN_TLS;
        let current: u64;
        asm!("rdfsbase {}", out(reg) current, options(nostack));
        if current == 0 {
            let base = &raw mut MAIN_TLS as u64;
            asm!("wrfsbase {}", in(reg) base, options(nostack));
        }
    }
}

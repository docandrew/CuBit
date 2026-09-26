//! Threads (docs/threads.md): THREAD_CREATE with a heap stack, joined through
//! the exit word the kernel clears and futex-wakes when the thread ends.

use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::cell::UnsafeCell;
use core::sync::atomic::{AtomicU32, Ordering};

use crate::sync::{futex_wait, FOREVER};

/// Default stack for spawned threads (Rust's `std` uses 2 MiB).
pub const DEFAULT_STACK_SIZE: usize = 2 * 1024 * 1024;

/// Why a thread could not start.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpawnError {
    /// The kernel refused: quota, thread IDs or kernel memory.
    Refused,
}

struct Packet<T> {
    /// Nonzero while the thread runs; the kernel stores 0 and wakes it at
    /// THREAD_EXIT, after the thread has stopped touching its user stack.
    exit_word: AtomicU32,
    result: UnsafeCell<Option<T>>,
}

// SAFETY: `result` is written once by the thread before its exit word is
// cleared, and read only by the joiner after observing the cleared word.
unsafe impl<T: Send> Sync for Packet<T> {}

struct Start<T> {
    main: Box<dyn FnOnce() -> T + Send>,
    packet: Arc<Packet<T>>,
}

/// A running thread. Dropping it without [`join`](JoinHandle::join) detaches
/// the thread and leaks its stack and result slot (the kernel still writes
/// the exit word when it ends).
pub struct JoinHandle<T> {
    packet: Option<Arc<Packet<T>>>,
    stack: Option<Vec<u8>>,
    id: u64,
}

impl<T> JoinHandle<T> {
    /// The kernel thread ID.
    pub fn id(&self) -> u64 {
        self.id
    }

    /// Wait for the thread to end and return its result.
    pub fn join(mut self) -> T {
        let packet = self.packet.take().expect("joined once");
        loop {
            let v = packet.exit_word.load(Ordering::Acquire);
            if v == 0 {
                break;
            }
            futex_wait(&packet.exit_word, v, FOREVER);
        }
        // The thread is in the kernel for good: its stack is free.
        drop(self.stack.take());
        // SAFETY: the thread stored the result before exiting (see Packet).
        unsafe { (*packet.result.get()).take().expect("thread produced a result") }
    }
}

impl<T> Drop for JoinHandle<T> {
    fn drop(&mut self) {
        // Detached: the thread may still run on the stack and the kernel will
        // still write the exit word, so neither may be freed.
        if let Some(stack) = self.stack.take() {
            core::mem::forget(stack);
        }
        if let Some(packet) = self.packet.take() {
            core::mem::forget(packet);
        }
    }
}

extern "C" fn thread_start<T>(start: *mut Start<T>) -> ! {
    // SAFETY: `spawn` leaked this box for exactly this thread.
    let start = unsafe { Box::from_raw(start) };
    let Start { main, packet } = *start;
    let value = main();
    // SAFETY: only this thread writes the result, before the exit word clears.
    unsafe { *packet.result.get() = Some(value) };
    // The kernel clears and wakes the exit word; the joiner's Arc keeps it
    // alive, and this thread's reference is deliberately not dropped after
    // the syscall (there is no after).
    drop(packet);
    exit_thread()
}

/// End the calling thread. The main thread ending ends the process.
pub fn exit_thread() -> ! {
    loop {
        // SAFETY: no arguments.
        unsafe { crate::syscall3(crate::Syscall::ThreadExit, 0, 0, 0) };
    }
}

/// Start `f` on a new thread with the default stack.
pub fn spawn<F, T>(f: F) -> Result<JoinHandle<T>, SpawnError>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    spawn_with_stack(DEFAULT_STACK_SIZE, f)
}

/// Start `f` on a new thread with a `stack_size`-byte stack.
pub fn spawn_with_stack<F, T>(stack_size: usize, f: F) -> Result<JoinHandle<T>, SpawnError>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    let packet = Arc::new(Packet { exit_word: AtomicU32::new(1), result: UnsafeCell::new(None) });
    let mut stack = Vec::<u8>::with_capacity(stack_size.max(16 * 1024));
    // A SysV function expects RSP + 8 to be 16-byte aligned at entry.
    let top = (stack.as_mut_ptr() as usize + stack.capacity()) & !15;
    let rsp = top - 8;
    let start = Box::into_raw(Box::new(Start { main: Box::new(f), packet: packet.clone() }));
    let exit_word = packet.exit_word.as_ptr() as u64;
    // SAFETY: the entry, stack and start block stay valid for the thread's
    // life (the handle owns the stack; the start block is the thread's).
    let id = unsafe {
        crate::syscall5(
            crate::Syscall::ThreadCreate,
            thread_start::<T> as *const () as usize as u64,
            rsp as u64,
            start as u64,
            0,
            exit_word,
        )
    };
    if id == u64::MAX {
        // SAFETY: the kernel did not start the thread; reclaim the block.
        drop(unsafe { Box::from_raw(start) });
        return Err(SpawnError::Refused);
    }
    Ok(JoinHandle { packet: Some(packet), stack: Some(stack), id })
}

//! Futexes and futex-based locks (docs/threads.md).
//!
//! `Mutex` and `Condvar` use the protocols Rust's `std` uses on futex
//! platforms; `tests/futex-queues/explore.py` model-checks the mutex protocol
//! against the kernel's FUTEX_WAIT/FUTEX_WAKE steps. Under `cfg(test)` (hosted)
//! the futex calls degrade to yielding and waking nobody, which the protocols
//! tolerate as spurious returns, so the lock logic runs on real host threads.

use core::cell::UnsafeCell;
use core::ops::{Deref, DerefMut};
use core::sync::atomic::{AtomicU32, Ordering};

/// Result of [`futex_wait`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WaitResult {
    /// Woken by [`futex_wake`] (or, hosted, spuriously).
    Woken,
    /// The word did not hold the expected value.
    Retry,
    /// The deadline passed.
    TimedOut,
    /// Unaligned, not user memory, or unmapped.
    Fault,
}

/// No deadline.
pub const FOREVER: u64 = u64::MAX;

/// Sleep while `word` holds `expected`, until woken or the absolute
/// monotonic millisecond `deadline_ms` ([`FOREVER`] for none).
pub fn futex_wait(word: &AtomicU32, expected: u32, deadline_ms: u64) -> WaitResult {
    #[cfg(not(test))]
    {
        let result = unsafe {
            // SAFETY: the kernel reads the aligned word through the process's
            // page tables; no pointer outlives the call.
            crate::syscall3(crate::Syscall::FutexWait, word.as_ptr() as u64,
                            expected as u64, deadline_ms)
        };
        match result {
            0 => WaitResult::Woken,
            1 => WaitResult::Retry,
            2 => WaitResult::TimedOut,
            _ => WaitResult::Fault,
        }
    }
    #[cfg(test)]
    {
        let _ = deadline_ms;
        if word.load(Ordering::Relaxed) != expected {
            return WaitResult::Retry;
        }
        std::thread::yield_now();
        WaitResult::Woken
    }
}

/// Wake up to `count` waiters on `word`, oldest first. Returns how many.
pub fn futex_wake(word: &AtomicU32, count: u32) -> u32 {
    #[cfg(not(test))]
    {
        // SAFETY: the address is only a key; the kernel does not dereference it.
        unsafe {
            crate::syscall3(crate::Syscall::FutexWake, word.as_ptr() as u64, count as u64, 0)
                as u32
        }
    }
    #[cfg(test)]
    {
        let _ = (word, count);
        0
    }
}

const UNLOCKED: u32 = 0;
const LOCKED: u32 = 1;
const CONTENDED: u32 = 2;

/// A futex mutex without data: 0 unlocked, 1 locked, 2 locked with waiters.
pub struct RawMutex {
    state: AtomicU32,
}

impl RawMutex {
    pub const fn new() -> Self {
        Self { state: AtomicU32::new(UNLOCKED) }
    }

    pub fn try_lock(&self) -> bool {
        self.state
            .compare_exchange(UNLOCKED, LOCKED, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
    }

    pub fn lock(&self) {
        if !self.try_lock() {
            self.lock_contended();
        }
    }

    #[cold]
    fn lock_contended(&self) {
        // Spin briefly while the lock is merely held, then mark it contended
        // and sleep.
        let mut spins = 100;
        while spins > 0 && self.state.load(Ordering::Relaxed) == LOCKED {
            core::hint::spin_loop();
            spins -= 1;
        }
        if self.try_lock() {
            return;
        }
        while self.state.swap(CONTENDED, Ordering::Acquire) != UNLOCKED {
            futex_wait(&self.state, CONTENDED, FOREVER);
        }
    }

    /// # Safety
    /// The caller holds the lock.
    pub unsafe fn unlock(&self) {
        if self.state.swap(UNLOCKED, Ordering::Release) == CONTENDED {
            futex_wake(&self.state, 1);
        }
    }
}

impl Default for RawMutex {
    fn default() -> Self {
        Self::new()
    }
}

/// A mutual-exclusion lock protecting a `T`.
pub struct Mutex<T: ?Sized> {
    raw: RawMutex,
    data: UnsafeCell<T>,
}

// SAFETY: access to `data` is serialized by `raw`.
unsafe impl<T: ?Sized + Send> Send for Mutex<T> {}
unsafe impl<T: ?Sized + Send> Sync for Mutex<T> {}

impl<T> Mutex<T> {
    pub const fn new(value: T) -> Self {
        Self { raw: RawMutex::new(), data: UnsafeCell::new(value) }
    }

    pub fn into_inner(self) -> T {
        self.data.into_inner()
    }
}

impl<T: ?Sized> Mutex<T> {
    pub fn lock(&self) -> MutexGuard<'_, T> {
        self.raw.lock();
        MutexGuard { mutex: self }
    }

    pub fn try_lock(&self) -> Option<MutexGuard<'_, T>> {
        self.raw.try_lock().then(|| MutexGuard { mutex: self })
    }
}

pub struct MutexGuard<'a, T: ?Sized> {
    mutex: &'a Mutex<T>,
}

impl<T: ?Sized> Deref for MutexGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        // SAFETY: the guard holds the lock.
        unsafe { &*self.mutex.data.get() }
    }
}

impl<T: ?Sized> DerefMut for MutexGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        // SAFETY: the guard holds the lock exclusively.
        unsafe { &mut *self.mutex.data.get() }
    }
}

impl<T: ?Sized> Drop for MutexGuard<'_, T> {
    fn drop(&mut self) {
        // SAFETY: the guard holds the lock.
        unsafe { self.mutex.raw.unlock() }
    }
}

/// A condition variable: a sequence number that notifiers bump before
/// waking, so a waiter that sampled it before unlocking never misses a
/// notification (it either sees the new value and returns, or sleeps and is
/// woken).
pub struct Condvar {
    seq: AtomicU32,
}

impl Condvar {
    pub const fn new() -> Self {
        Self { seq: AtomicU32::new(0) }
    }

    /// Release the guard's lock, wait for a notification (or a spurious
    /// wakeup), and reacquire it.
    pub fn wait<'a, T: ?Sized>(&self, guard: MutexGuard<'a, T>) -> MutexGuard<'a, T> {
        let seq = self.seq.load(Ordering::Relaxed);
        let mutex = guard.mutex;
        drop(guard);
        futex_wait(&self.seq, seq, FOREVER);
        mutex.lock()
    }

    /// As [`wait`](Self::wait), until the absolute monotonic millisecond
    /// deadline. Returns whether it timed out.
    pub fn wait_until<'a, T: ?Sized>(
        &self,
        guard: MutexGuard<'a, T>,
        deadline_ms: u64,
    ) -> (MutexGuard<'a, T>, bool) {
        let seq = self.seq.load(Ordering::Relaxed);
        let mutex = guard.mutex;
        drop(guard);
        let result = futex_wait(&self.seq, seq, deadline_ms);
        (mutex.lock(), result == WaitResult::TimedOut)
    }

    pub fn notify_one(&self) {
        self.seq.fetch_add(1, Ordering::Relaxed);
        futex_wake(&self.seq, 1);
    }

    pub fn notify_all(&self) {
        self.seq.fetch_add(1, Ordering::Relaxed);
        futex_wake(&self.seq, u32::MAX);
    }
}

impl Default for Condvar {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate std;
    use std::sync::Arc;
    use std::vec::Vec;

    #[test]
    fn mutex_excludes_across_host_threads() {
        let m = Arc::new(Mutex::new(0u64));
        let threads: Vec<_> = (0..8)
            .map(|_| {
                let m = m.clone();
                std::thread::spawn(move || {
                    for _ in 0..20_000 {
                        *m.lock() += 1;
                    }
                })
            })
            .collect();
        for t in threads {
            t.join().unwrap();
        }
        assert_eq!(*m.lock(), 160_000);
    }

    #[test]
    fn condvar_hands_off() {
        let pair = Arc::new((Mutex::new(false), Condvar::new()));
        let other = pair.clone();
        let t = std::thread::spawn(move || {
            let (m, c) = &*other;
            *m.lock() = true;
            c.notify_one();
        });
        let (m, c) = &*pair;
        let mut ready = m.lock();
        while !*ready {
            ready = c.wait(ready);
        }
        drop(ready);
        t.join().unwrap();
    }

    #[test]
    fn wait_on_changed_value_retries() {
        let w = AtomicU32::new(5);
        assert_eq!(futex_wait(&w, 4, FOREVER), WaitResult::Retry);
    }
}

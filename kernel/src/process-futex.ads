-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Futexes: FUTEX_WAIT and FUTEX_WAKE (docs/threads.md)
--
-- Private futexes: a key is (process, user word address). FUTEX_WAIT sleeps
-- only if the word still holds the expected value, checked and enqueued
-- under the key's bucket lock; FUTEX_WAKE takes the same lock. The bucket
-- data structure is Futex_Queues (proved: FIFO per key, key isolation, exact
-- removal); the lock discipline is the model proved in tests/futex-queues
-- (no lost wakeup when every changing store is followed by a wake).
--
-- Lock order: bucket lock, then Process.lock. The reaper cancels a dying
-- thread's wait before freeing it, outside Process.lock.
-------------------------------------------------------------------------------
package Process.Futex is

    -- FUTEX_WAIT results.
    FUTEX_WOKEN     : constant Unsigned_64 := 0;
    -- The word did not hold the expected value: recheck it and retry.
    FUTEX_RETRY     : constant Unsigned_64 := 1;
    FUTEX_TIMED_OUT : constant Unsigned_64 := 2;
    -- Unaligned, not user RAM, or not mapped (readable, for a wait).
    FUTEX_FAULT     : constant Unsigned_64 := Unsigned_64'Last;

    -- No deadline.
    FOREVER : constant Unsigned_64 := Unsigned_64'Last;

    -- Sleep while the calling process's 32-bit word at address holds
    -- expected, until woken or the absolute monotonic-ms deadline passes.
    function wait (address, expected, deadlineMs : Unsigned_64)
      return Unsigned_64;

    -- Wake up to count of the calling process's waiters on address, oldest
    -- first. Returns the number woken.
    function wake (address, count : Unsigned_64) return Unsigned_64;

    -- Wake up to count waiters of pid on address (thread exit).
    function wakeFor (pid : ProcessID; address, count : Unsigned_64)
      return Unsigned_64;

    -- Timer tick (BSP): time out waits whose deadline has passed.
    procedure expireDeadlines (nowMs : Unsigned_64);

    -- The reaper: withdraw a dying thread's wait, if any, before freeing it.
    -- Not called with Process.lock held.
    procedure cancelWait (tid : ThreadID);
end Process.Futex;

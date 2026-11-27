-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Futexes: FUTEX_WAIT and FUTEX_WAKE (docs/threads.md, IPC-010)
--
-- Private futexes live in their process's own table (Process.Private_Futex,
-- IPC revision 2, S1); this unit's private forms only route there, so old
-- and new callers share one table. Shared futexes
-- (IPC-010): a key is (grant slot, grant life, offset), resolved from a
-- Futex_ABI shared word by Process.IPC.resolveSharedWord, so both parties
-- of a grant reach the same key, and nobody else reaches it at all.
--
-- FUTEX_WAIT sleeps only if the word still holds the expected value,
-- checked and enqueued under the key's bucket lock (for a shared key the
-- grant is checked live under that lock too); FUTEX_WAKE takes the same
-- lock. The bucket data structure is Futex_Queues (proved: FIFO per key,
-- key isolation, exact removal, a life's removal touches no other key);
-- the lock discipline is the model proved in tests/futex-queues (no lost
-- wakeup when every changing store is followed by a wake; no sleeper left
-- on an ended grant life after its sweep).
--
-- A grant life's end is swept by Process.IPC once grantLock is free
-- (endSharedLife): every waiter of that life returns FUTEX_REVOKED.
--
-- Lock order: bucket lock, then grantLock (resolving a shared word), then
-- Process.lock; overflowLock is taken only after a bucket lock and never
-- held into Process.lock. The reaper cancels a dying thread's wait before
-- freeing it, outside Process.lock.
-------------------------------------------------------------------------------
package Process.Futex is

    -- FUTEX_WAIT results.
    FUTEX_WOKEN     : constant Unsigned_64 := 0;
    -- The word did not hold the expected value: recheck it and retry.
    FUTEX_RETRY     : constant Unsigned_64 := 1;
    FUTEX_TIMED_OUT : constant Unsigned_64 := 2;
    -- A shared futex's grant life ended while waiting.
    FUTEX_REVOKED   : constant Unsigned_64 := 3;
    -- Unaligned, not user RAM, or not mapped (readable, for a wait); for
    -- a shared word, also: not a party of a live grant.
    FUTEX_FAULT     : constant Unsigned_64 := Unsigned_64'Last;
    -- FUTEX_WAKE of a shared word by a non-party, or a malformed word.
    WAKE_REFUSED    : constant Unsigned_64 := Unsigned_64'Last;

    -- No deadline.
    FOREVER : constant Unsigned_64 := Unsigned_64'Last;

    -- Sleep while the calling process's 32-bit word at address holds
    -- expected, until woken or the absolute monotonic-ms deadline passes.
    function wait (address, expected, deadlineMs : Unsigned_64)
      return Unsigned_64;

    -- Wait's futex form (IPC-010): the word is a private address or, with
    -- shared, a Futex_ABI shared word. With wakeFirst, first wake one
    -- waiter of wakeWord (private, or shared with wakeShared) placed on
    -- this CPU (readySync), then wait, switching straight to that waiter
    -- if it is still ready here when this thread sleeps (yieldToSyncWakee).
    -- A malformed or refused wake operand returns FUTEX_FAULT and waits
    -- for nothing.
    function waitForm
      (shared     : Boolean;
       word       : Unsigned_64;
       expected   : Unsigned_64;
       deadlineMs : Unsigned_64;
       wakeFirst  : Boolean;
       wakeShared : Boolean;
       wakeWord   : Unsigned_64) return Unsigned_64;

    -- Wake up to count of the calling process's waiters on address, oldest
    -- first. Returns the number woken.
    function wake (address, count : Unsigned_64) return Unsigned_64;

    -- FUTEX_WAKE (arg2 = Futex_ABI.Private_Space or Shared_Space): a
    -- private address, or a shared word of a grant the caller is a party
    -- of (else WAKE_REFUSED).
    --
    -- Peer_Space (IPC-010 option B): word is a grant reference; the caller,
    -- a party of that live notified grant, wakes the other party's wake word
    -- (Process.IPC.wakePeer); sync places the woken thread on this CPU;
    -- urgent marks it an input wake (Futex_ABI.Urgent_Hint: the caller
    -- may lend urgency, Latency_Contract_ABI.Input_Wakes_Only).
    function wakeSpace (word, count, space : Unsigned_64; sync : Boolean;
                        urgent : Boolean := False) return Unsigned_64;

    -- Wake up to count waiters of pid on address (thread exit).
    function wakeFor (pid : ProcessID; address, count : Unsigned_64)
      return Unsigned_64;

    -- A grant life ended (Process.IPC, without grantLock): its waiters
    -- return FUTEX_REVOKED.
    procedure endSharedLife (slot, generation : Unsigned_32);

    -- After the queue of ended lives overflowed: every shared waiter whose
    -- grant life is no longer live returns FUTEX_REVOKED.
    procedure endDeadSharedLives;

    -- Timer tick (BSP): time out waits whose deadline has passed.
    procedure expireDeadlines (nowMs : Unsigned_64);

    -- The reaper: withdraw a dying thread's wait, if any, before freeing it.
    -- Not called with Process.lock held.
    procedure cancelWait (tid : ThreadID);
end Process.Futex;

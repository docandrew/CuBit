-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Private futexes (IPC revision 2, S1): words in a process's own
-- memory, waited on only by its own threads, in its own table.
--
-- @description
-- The table is Private_Futexes (proved): one entry per thread slot, a
-- lookup bounded by the process's 128 slots, wakes in batches of at most
-- Private_Futexes.Chunk under one hold of the process's privateFutexLock.
-- Wait checks the word and enqueues under that lock and Wake takes it, the
-- discipline of the proved protocol (tests/private-futexes Word_Protocol,
-- tests/futex-queues Futex_Protocol): no wake is lost when every store that
-- changes the word is followed by a wake. A process cannot reach another's
-- table at all, so the only thread it can slow is its own.
--
-- Serves the Wait and Wake system calls (Notification_ABI.Private_Word)
-- and the old WAIT futex form and FUTEX_WAKE for private words
-- (Process.Futex), so old and new callers in one process share one table.
-- A private wake also wakes the process's wake word (IPC-010 option B,
-- Process.IPC.noteWakeWord) when it woke fewer than it was asked to.
--
-- Lock order: a deadline heap lock (Process.Wait_Timers), then
-- privateFutexLock, then Process.lock.
-------------------------------------------------------------------------------
package Process.Private_Futex is

    -- Notification_ABI's Private_Word results (Woken, Retry, Timed_Out,
    -- Bad_Argument).

    -- The calling thread sleeps while its process's word at address holds
    -- expected. timed: until the TSC deadline tsc (passed: already gone, so
    -- the word is only checked); else until woken. handOff: switch to this
    -- thread's sync wakee on sleeping (the old WAIT's wake-plus-wait).
    function wait (address, expected : Unsigned_64; timed, passed : Boolean;
                   tsc : Unsigned_64; handOff : Boolean := False) return Unsigned_64;

    -- Wake up to count of pid's waiters on address, oldest first (every
    -- one, in batches, when count covers them all); the first placed on
    -- this CPU when sync. How many, or Notification_ABI.Bad_Argument.
    function wake (pid : ProcessID; address, count : Unsigned_64; sync : Boolean := False)
      return Unsigned_64;

    -- Process.Wait_Timers: t's deadline came (its heap lock held).
    procedure Time_Out (t : ThreadID);

    -- Process.Wait_Timers.Cancel: t will not run again.
    procedure Cancel (t : ThreadID);

end Process.Private_Futex;

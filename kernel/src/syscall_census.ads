-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary System call census (ABI-002): which processes call which calls
--
-- Diagnostic, built only with SYSCALL_CENSUS=1 (Build.Syscall_Census). The
-- dispatcher counts each process's calls by number. A pinned kernel thread
-- prints, every Report_Interval, one line per process whose counts changed
-- (cumulative, final=0), and a process's last line comes when it is
-- reclaimed (final=1), before its PID can be reused:
--   CENSUS: id=<identity word> name=<16 chars> final=<0|1> <number>=<count>...
-- tests/syscall-census/census.py merges the lines of many runs.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Syscall_Census is
    Slots : constant := 256;
    -- Call indexes are Syscall.SyscallNumber'Pos (Syscall checks they fit).
    Calls : constant := 128;
    subtype Slot is Natural range 0 .. Slots - 1;
    subtype Call is Natural range 0 .. Calls - 1;

    -- The dispatcher, for each valid call. Lock-free.
    procedure Note (S : Slot; C : Call; Number : Unsigned_64) with Inline;

    -- Process.reclaimProcess, before the PID can be reused.
    procedure Report_Exit (S : Slot);

    -- kmain, once, after the reaper: start the reporting thread.
    procedure Start;
end Syscall_Census;

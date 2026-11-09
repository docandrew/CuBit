-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel log ring's writer (shared/publications/kernel_log_publication.ads,
-- LOG-004): typed records of the kernel's console lines and of the
-- processes it stopped (Fault_Reports), for logstore to drain. It never
-- holds a program's text.
--
-- @description
-- The ring is a page-aligned static area in the kernel image (zeroed bss),
-- so it records from the first boot line on, before any allocator exists,
-- and is never charged to or freed with a process. Kernel_Log_Mapping maps
-- it read-only into the registered log service only.
--
-- Locking. One writer at a time, by Writer_Owner: a word set by
-- compare-and-swap to the writing CPU's kernel GS base (its per-CPU data
-- address; Early_Owner on a CPU without per-CPU data: the boot CPU before
-- Enable_Locking, a CPU being brought up), held with interrupts masked, for
-- the few hundred bytes one record copies. The holder takes no other lock,
-- prints nothing and never waits, so another CPU spins for at most one
-- record. It is a leaf: TextIO calls Append_Kernel_Line under its output
-- lock (output lock, then this); the fault and kill paths call Append_Stop
-- holding at most IRQ-masking locks that never wait on this one. A CPU that finds itself already the writer (an NMI or a
-- fault that logs, interrupting it mid-record) gives its record up and
-- counts it in the header's Reentered, so no path can deadlock on the
-- ring, interrupt and fault paths included. After a panic nothing is
-- appended (TextIO checks x86.panicked), so a CPU stopped while holding the
-- lock cannot stall the panicking one.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with Fault_Reports;
with Kernel_Log_Publication;

package Kernel_Log with
    SPARK_Mode => Off -- volatile ring stores, intrinsic locking
is
    package KLP renames Kernel_Log_Publication;

    -- The ring's capacity: the build setting (KERNEL_LOG_RECORDS, src/build.pre).
    function Capacity return KLP.Record_Capacity;

    -- Time.msTicks, by address: Kernel_Log sits below Time in the unit
    -- graph (TextIO calls it), so it cannot name it. Records written
    -- before this carry time 0 (they are before the first timer tick).
    procedure Use_Clock (Milliseconds : System.Address);

    -- Per-CPU data exists on every CPU that will write (TextIO's
    -- enableOutputLocking point): writers use the lock from now on.
    procedure Enable_Locking;

    -- One kernel console line (no line feed), already cut to what the
    -- caller kept; Truncated: the caller dropped the rest of the line.
    procedure Append_Kernel_Line (Text : String; Truncated : Boolean);

    -- The kernel stopped a process: its identity word (KERN-003), its name
    -- and the typed report of why, as two records: the summary for every
    -- reader of the process's logs, then the detail for diagnostics
    -- readers only (Kernel_Log_Publication.Audience); a stop by another
    -- process has no detail. Never blocks (interrupt and fault paths).
    procedure Append_Stop
      (Source : Unsigned_64;
       Name   : KLP.Name_Text;
       Fault  : Fault_Reports.Report);

    -- The area's first byte (page-aligned), for Kernel_Log_Mapping.
    function Area_Address return System.Address;
end Kernel_Log;

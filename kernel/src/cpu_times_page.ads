-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel's side of the per-CPU times publication (KERN-002,
-- shared/publications/cpu_times_publication.ads).
--
-- @description
-- One frame, written only here, mapped read-only and non-executable into a
-- process only when its launcher grants it (SYSCALL_INSTALL_LAUNCH_ARGUMENTS
-- arg4, System_Publications.Granted_Publication). Like the other
-- publication frames it is the kernel's: not tracked, charged or freed with
-- any process. Each CPU writes only its own record, from
-- Process.accountBoundary, under its seqlock.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Config;
with CPU_Times_Publication;
with Virtmem;

package Cpu_Times_Page with
    SPARK_Mode => Off -- boot frame allocation and the kernel's page writes
is
    package CTP renames CPU_Times_Publication;

    pragma Compile_Time_Error
      (Config.MAX_SMP_CPUS > CTP.Maximum_Processors,
       "the kernel starts more CPUs than the CPU-times page holds");

    ---------------------------------------------------------------------------
    -- Publish
    -- BSP boot only, before the first process: allocates the frame and
    -- writes the header (CPUs the kernel starts, the counter rate, RAM).
    -- Records written before this are kept by the callers' own copies and
    -- appear at their CPU's next boundary.
    ---------------------------------------------------------------------------
    procedure Publish (Processors     : CTP.Processor_Count;
                       Tick_Frequency : Unsigned_64;
                       Memory_Bytes   : Unsigned_64);

    -- CPU's record: one seqlock write of Values. Only CPU itself calls it.
    procedure Write (CPU : Natural; Values : CTP.Fields) with Inline;

    -- Free physical memory (one store).
    procedure Set_Free_Memory (Bytes : Unsigned_64) with Inline;

    -- Map the page into an address space (the caller holds its lock).
    -- Mapping it again is harmless. Fails before Publish.
    procedure Map_Into (Root : in out Virtmem.P4; Success : out Boolean);
end Cpu_Times_Page;

-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel's side of the clock publication (KERN-002,
-- shared/time/clock_publication.ads, docs/fast-clock.md).
--
-- @description
-- One frame, written only here, mapped read-only and non-executable into
-- every process at Clock_Publication.Page_Address. The kernel keeps its own
-- copy of the conversion: Time.msTicks is that conversion sampled at CPU 0's
-- timer ticks, and READ_MONOTONIC_MICROSECONDS reads it directly, so the
-- system calls and the page agree on one epoch.
--
-- User space gets the parameters only when the TSC is invariant (CPUID
-- 8000_0007h EDX bit 8): only then does every CPU's counter advance at one
-- constant rate. Otherwise the page stays mapped but unpublished, user space
-- uses the system calls, and READ_MONOTONIC_MICROSECONDS keeps the HPET.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Virtmem;

package Clock_Page with
    SPARK_Mode => Off -- boot frame allocation and the kernel's page writes
is
    ---------------------------------------------------------------------------
    -- Publish
    -- BSP boot only: interrupts disabled, before APs and the first process.
    -- The conversion continues from Base_Milliseconds at counter Base_Ticks.
    -- A frequency outside Clock_Publication.Counter_Frequency leaves no
    -- conversion (Converting False); the page is still allocated.
    ---------------------------------------------------------------------------
    procedure Publish (Frequency         : Unsigned_64;
                       Base_Ticks        : Unsigned_64;
                       Base_Milliseconds : Unsigned_64;
                       Invariant         : Boolean);

    -- The kernel follows the conversion (msTicks).
    function Converting return Boolean;

    -- User space has the parameters; READ_MONOTONIC_MICROSECONDS uses them.
    function Shared return Boolean;

    -- Nanoseconds at TSC value Counter. Requires Converting.
    procedure Read (Counter     : Unsigned_64;
                    Nanoseconds : out Unsigned_64;
                    Success     : out Boolean);

    -- Map the page into a new address space. The frame is the kernel's:
    -- not tracked, charged or freed with the process, and a grant of it
    -- fails (its owner is no process).
    procedure Map (Root : in out Virtmem.P4; Success : out Boolean);
end Clock_Page;

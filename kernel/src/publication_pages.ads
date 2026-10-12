-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel's side of the system publications (KERN-002,
-- shared/publications/system_publications.ads, docs/syscall-retirement.md):
-- system facts, the service registry and the wall clock.
--
-- @description
-- Three frames, written only here, mapped read-only and non-executable into
-- every process beside the clock page. Like the clock page, the frames are
-- the kernel's: not tracked, charged or freed with any process, and a grant
-- of them fails. Every value is one aligned word written with one store.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System_Publications;
with Virtmem;

package Publication_Pages with
    SPARK_Mode => Off -- boot frame allocation and the kernel's page writes
is
    ---------------------------------------------------------------------------
    -- Publish_Facts
    -- BSP boot only, before the first process: allocates the pages and
    -- writes the system facts (CPU count, memory, boot framebuffer, clock
    -- diagnostic). The registry and wall clock start empty.
    ---------------------------------------------------------------------------
    procedure Publish_Facts;

    -- Map the pages into a new address space (fails before Publish_Facts).
    procedure Map (Root : in out Virtmem.P4; Success : out Boolean);

    -- The registry entry for Role: a process identity word, or
    -- System_Publications.No_Identity.
    procedure Set_Registered (Role     : System_Publications.Service_Role;
                              Identity : Unsigned_64);

    -- UTC milliseconds at monotonic zero, or 0 for unknown.
    procedure Set_Wall_Clock (Offset_Ms : Unsigned_64);

    -- Device-memory requests refused so far (Device_Memory_Diagnostics).
    procedure Set_Device_Memory_Denials (Count : Unsigned_64);

    -- User processes killed for a fault so far (Device_Memory_Diagnostics).
    procedure Set_Process_Kills (Count : Unsigned_64);

    -- The boot display adapter is driven natively (1) or not (0).
    procedure Set_Boot_Display_Native (Native : Boolean);
end Publication_Pages;

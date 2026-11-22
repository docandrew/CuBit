-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Address-space tags on x86 (PCIDs): the trusted CR3/TLB adapter
--
-- Every CR3 load and every whole-TLB flush goes through here. When the CPU
-- has both PCID and INVPCID (and Build.Tagged_TLB), each CPU sets
-- CR4.PCIDE: the kernel's page tables load as Address_Space_Tags.Kernel_Tag
-- and each process slot as its own tag, and a load keeps the tag's cached
-- translations when Address_Space_Tags.Keep says this CPU last loaded the
-- tag for its current life. Otherwise every load drops the old translations
-- as before.
--
-- Flush_All drops every translation of every tag (INVPCID all contexts):
-- unmapping or downgrading any mapping, kernel or user, must reach other
-- address spaces' cached translations too, never only the current tag's.
--
-- Trusted (inline assembly, per-CPU tables). The rules it applies are
-- Address_Space_Tags (proved); the model of the whole protocol is
-- tests/address-space-tags. Required of the callers, not checked here:
--  * Begin_Life runs before the slot's new address space is first loaded,
--    and after no CPU has the slot's previous address space in CR3 (its
--    contexts have stopped and switched away);
--  * every removal or downgrade of a present mapping is followed by
--    Flush_All on every CPU that may hold it (TLB_Shootdown.Invalidate_All)
--    before the page or page table is reused.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Address_Space_Tags;
with Config;

package TLB_Context is
    package Tags renames Address_Space_Tags;

    subtype CPU_Index is Natural range 0 .. Config.MAX_SMP_CPUS - 1;
    -- Tags of process slots: PID + 1 (Process checks PIDs fit).
    Slot_Tags : constant := 256;
    subtype Slot_Tag is Tags.Process_Tag
        range Tags.Process_Tag'First .. Tags.Process_Tag'First + Slot_Tags - 1;

    -- Each CPU once, after it runs on the kernel's page tables (tag 0, so
    -- CR4.PCIDE may be set) and before it loads any process. The boot CPU
    -- first: it decides for all.
    procedure Setup_This_CPU (Boot_CPU : Boolean);

    -- Whether CPUs tag translations (decided on the boot CPU).
    function Enabled return Boolean with Inline;

    -- The slot gets a new address space: no CPU may keep translations
    -- cached for any earlier one.
    procedure Begin_Life (T : Slot_Tag);

    -- Load a process's page tables (Root: physical, 4 KiB aligned) on this
    -- CPU, keeping the tag's translations only when they are its life's.
    procedure Load_Process (Root : Unsigned_64; T : Slot_Tag);

    -- Load the kernel's page tables, keeping their translations.
    procedure Load_Kernel (Root : Unsigned_64);

    -- Load a CR3 value as it was read (no tag bookkeeping): the boot path
    -- and CPUs without tags. Drops the loaded tag's translations.
    procedure Load_Raw (CR3 : Unsigned_64);

    -- Drop every translation of every tag on this CPU.
    procedure Flush_All;

    -- CR3 as it reads now.
    function Current return Unsigned_64;
end TLB_Context;

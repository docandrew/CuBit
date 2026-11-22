-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Message-signaled device interrupts (docs/msi-interrupts.md).
--
-- @description
-- The one live Device_Vectors table, its lock, and the interrupt path:
--  * allocate / release: the device manager, through its vector authority
--    capability (Syscall.Admin checks it), binds a block of vectors to a
--    driver and later frees it, after masking the device;
--  * retireProcess: a dying owner's vectors stop delivering at once but
--    stay reserved until released;
--  * deliver: an interrupt on a device vector sets the owner's pending
--    vector bit and IRQ doorbell (Process.IPC.notifyDeviceVector). The
--    doorbell is level-like: it stays set until the driver takes it, and the
--    driver then drains its device, so an edge-triggered message that
--    arrives at any point is never lost.
-- Per-CPU, per-vector interrupt counts make delivery visible.
--
-- Lock order: a mailbox lock (Process.mailtab) may be held when taking the
-- vector lock, never the reverse. deliver takes no vector lock: it reads one
-- entry (bound and retired transitions happen only while the device is
-- masked or its owner is gone).
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Device_Vector_ABI;
with Process;

package Device_Interrupts with
    SPARK_Mode => Off  -- glue: the live table, its lock, per-CPU counters
is
    subtype Vector is Device_Vector_ABI.Device_Vector;
    subtype Block_Size is Device_Vector_ABI.Block_Size;

    type Allocation_Outcome is
      (Allocated, Vectors_Exhausted, Invalid_CPU);

    ---------------------------------------------------------------------------
    -- allocate
    -- Bind Count contiguous vectors to owner, delivered to the local APIC
    -- of CPU (the CPU the driver runs on). The caller holds owner's mailbox
    -- lock and has checked authority and that owner is live.
    ---------------------------------------------------------------------------
    procedure allocate (owner       : Process.ProcessID;
                        count       : Block_Size;
                        cpu         : Natural;
                        head        : out Vector;
                        destination : out Device_Vector_ABI.Destination_ID;
                        outcome     : out Allocation_Outcome);

    ---------------------------------------------------------------------------
    -- release
    -- Free the block that starts at head. Returns its count and the owner it
    -- was bound or retired from, so the caller can withdraw the owner's
    -- interrupt capability. False when head starts no block.
    ---------------------------------------------------------------------------
    procedure release (head     : Vector;
                       count    : out Block_Size;
                       owner    : out Unsigned_64;
                       released : out Boolean);

    -- A process is being reclaimed: no more interrupts reach it.
    procedure retireProcess (pid : Process.ProcessID);

    -- Interrupt context, after EOI.
    procedure deliver (v : Vector);

    -- Visibility (READ_DEVICE_VECTOR).
    function interruptCount (v : Vector) return Unsigned_64;
    function ownerIdentity (v : Vector) return Unsigned_64;
    function binding (v : Vector) return Device_Vector_ABI.Vector_Binding;

    -- Interrupts on vectors with no bound owner (released, retired, or a
    -- device programmed wrongly): counted, never delivered.
    function strayCount return Unsigned_64;
end Device_Interrupts;

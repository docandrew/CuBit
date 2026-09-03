-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- IRQ Capability Owner Table
--
-- Simple owner table mapping interrupt vectors to the PID that holds the
-- CAP_IRQ for that vector. Interrupt handlers consult this table instead
-- of using hardcoded PIDs.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Capabilities.IRQ with
    SPARK_Mode => On
is

    subtype IRQVector is Natural range 0 .. 255;

    ---------------------------------------------------------------------------
    --  Legacy PCI INTx lines may be shared by several devices.  Keep the
    --  fan-out deliberately small and statically bounded: registration still
    --  requires delegated CAP_IRQ authority, and every subscriber must
    --  inspect and acknowledge only its own device.
    MAX_OWNERS_PER_IRQ : constant := 8;
    subtype IRQOwnerIndex is Natural range 0 .. MAX_OWNERS_PER_IRQ - 1;

    -- registerIRQ
    -- Register a process as an owner of a given interrupt vector. Repeated
    -- registration by the same process is idempotent. Fails only when the
    -- bounded owner set is full.
    ---------------------------------------------------------------------------
    procedure registerIRQ (vector : IRQVector;
                           pid    : Unsigned_64;
                           status : out Boolean);

    ---------------------------------------------------------------------------
    -- unregisterIRQ
    -- Remove ownership of a given interrupt vector. Only the current owner
    -- can unregister.
    ---------------------------------------------------------------------------
    procedure unregisterIRQ (vector : IRQVector;
                             pid    : Unsigned_64);

    ---------------------------------------------------------------------------
    -- getOwner
    -- Return one authorized recipient for a vector. 0 = unused slot.
    ---------------------------------------------------------------------------
    function getOwner (vector : IRQVector;
                       index  : IRQOwnerIndex) return Unsigned_64;

    ---------------------------------------------------------------------------
    -- unregisterAllByPID
    -- Remove all IRQ registrations belonging to a given process.
    -- Called during process kill().
    ---------------------------------------------------------------------------
    procedure unregisterAllByPID (pid : Unsigned_64);

private

    type OwnerSlots is array (IRQOwnerIndex) of Unsigned_64;
    type IRQOwnerMatrix is array (IRQVector) of OwnerSlots;

    irqOwners : IRQOwnerMatrix := (others => (others => 0));

end Capabilities.IRQ;

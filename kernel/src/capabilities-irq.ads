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
    -- registerIRQ
    -- Register a process as the owner of a given interrupt vector.
    -- Fails if the vector already has an owner.
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
    -- Return the PID that owns a given interrupt vector. 0 = no owner.
    ---------------------------------------------------------------------------
    function getOwner (vector : IRQVector) return Unsigned_64;

    ---------------------------------------------------------------------------
    -- unregisterAllByPID
    -- Remove all IRQ registrations belonging to a given process.
    -- Called during process kill().
    ---------------------------------------------------------------------------
    procedure unregisterAllByPID (pid : Unsigned_64);

private

    type IRQOwnerTable is array (IRQVector) of Unsigned_64;

    irqOwners : IRQOwnerTable := (others => 0);

end Capabilities.IRQ;

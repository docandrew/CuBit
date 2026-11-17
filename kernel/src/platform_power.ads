-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The platform power kernel object (KERN-004): Enter_State on a
-- CAP_PLATFORM_POWER handle, reached through Call (ABI-002).
--
-- @description
-- The adapter around Platform_Power_Policy (proved): against the registers
-- the kernel decoded from the FADT at boot (acpi.powerRegisters) it performs
-- an admitted plan:
-- writes the console line, drains the console ring (TextIO.stopAsynchronous,
-- as a fatal stop does), parks the other CPUs (CPU_Parking), then the
-- register reads and writes the plan names, in its order, on this CPU with
-- interrupts off. S5 and Reset do not return when they take effect; if the
-- machine still runs after Settle_Microseconds, the CPUs are released, the
-- console goes back to asynchronous and the caller gets
-- Did_Not_Take_Effect.
--
-- Tested, not proved: the port I/O, the console drain, the parking.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Process;

package Platform_Power is

    -- A Call on a platform power handle. Writable: the handle carries Write.
    -- Reply is the whole reply message (label echoed, one outcome word).
    procedure Call (Writable : Boolean;
                    Request  : Process.Message;
                    Reply    : out Process.Message);

    -- How long a written S5 or Reset may take before the caller hears
    -- Did_Not_Take_Effect (as CuBit.Power_Event_Exchange.Settle_Milliseconds).
    Settle_Microseconds : constant := 2_000_000;
    -- The bound when no monotonic clock is available: polls of it.
    Settle_Polls_Without_Clock : constant := 400_000_000;
end Platform_Power;

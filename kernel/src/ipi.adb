-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2024 Jon Andrew
--
-- Inter-Processor Interrupt (IPI) support
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

use type System.Address;

-- SPARK_Mode Off: volatile MMIO register access via overlay
package body IPI with
    SPARK_Mode => Off
is
    lapicAddr : System.Address := System.Null_Address;

    RESCHEDULE_VECTOR : constant := 249;
    ICR0_OFFSET       : constant := 16#300#;
    ICR1_OFFSET       : constant := 16#310#;
    ICR_SEND_PENDING  : constant Unsigned_32 := 16#0000_1000#;

    ---------------------------------------------------------------------------
    -- init
    ---------------------------------------------------------------------------
    procedure init (lapicBase : in System.Address) is
    begin
        lapicAddr := lapicBase;
    end init;

    ---------------------------------------------------------------------------
    -- sendReschedule
    ---------------------------------------------------------------------------
    procedure sendReschedule (targetCPU : in Natural) is
        icr0 : Unsigned_32 with
            Import, Volatile, Address => lapicAddr + ICR0_OFFSET;
        icr1 : Unsigned_32 with
            Import, Volatile, Address => lapicAddr + ICR1_OFFSET;
    begin
        if lapicAddr = System.Null_Address then
            return;
        end if;

        -- Wait for any pending IPI to complete
        while (icr0 and ICR_SEND_PENDING) /= 0 loop
            null;
        end loop;

        -- Set destination APIC ID in ICR high word
        icr1 := Shift_Left (Unsigned_32 (targetCPU), 24);

        -- Send fixed IPI with reschedule vector (edge-triggered, physical)
        icr0 := Unsigned_32 (RESCHEDULE_VECTOR);
    end sendReschedule;

end IPI;

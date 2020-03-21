-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- I/O APIC
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

with InterruptNumbers;
with Virtmem;

generic
    BaseAddr : System.Address;
package ioapic 
    with SPARK_Mode => On
is

    -- Various offsets or "addresses" of the I/O APIC registers. Write this
    -- value to the IOREGSEL
    subtype IOAPICAddress is Unsigned_32;

    IOAPICID            : constant IOAPICAddress := 16#00#;
    IOAPICVER           : constant IOAPICAddress := 16#01#;
    IOAPICARB           : constant IOAPICAddress := 16#02#;
    
    --------------------------------------------------------------------------
    -- Base index of the I/O redirection table registers
    -- Each redirect entry needs to have the low and high 32-bit entries
    -- written separately.
    --------------------------------------------------------------------------
    IOREDTBL_BASE       : constant IOAPICAddress := 16#10#;

    -- low word of redirect table - see APIC section of Intel manual for details.
    INT_MASKED          : constant Unsigned_32 := 16#0001_0000#;
    INT_LEVEL_TRIGGER   : constant Unsigned_32 := 16#0000_8000#;
    INT_ACTIVE_LOW      : constant Unsigned_32 := 16#0000_2000#;
    INT_LOGICAL         : constant Unsigned_32 := 16#0000_0800#;

    ---------------------------------------------------------------------------
    -- Writes to the I/O APIC require specifying the "address" of the register
    -- in IOREGSEL, and then accessing the data itself from IOWIN.
    -- @field IOREGSEL - the I/O Register select (index)
    -- @field IOWIN - the I/O Window (data)
    ---------------------------------------------------------------------------
    type IOAPICStruct is
    record
        IOREGSEL        : IOAPICAddress;
        res1            : Unsigned_32;
        res2            : Unsigned_32;
        res3            : Unsigned_32;
        IOWIN           : Unsigned_32;
    end record;

    for IOAPICStruct use
    record
        IOREGSEL        at 16#00# range 0..31;
        res1            at 16#04# range 0..31;
        res2            at 16#08# range 0..31;
        res3            at 16#0C# range 0..31;
        IOWIN           at 16#10# range 0..31;
    end record;

    ioapic : IOAPICStruct with Import, Address => baseAddr;

    --function read(index : in IOAPICAddress) return Unsigned_32;    
    --procedure write(index : in IOAPICAddress, val : in Unsigned_32);

    ---------------------------------------------------------------------------
    -- setupIOAPIC
    -- Marks all interrupts as edge-triggered, active high, disabled, and not
    -- routed to any CPUs. This function will check the I/O APIC ID for the
    -- APIC whose base address is specified in the package instantiation
    -- against the expectedID parameter.
    --
    -- @param expectedID - I/O APIC ID from ACPI or MP tables.
    ---------------------------------------------------------------------------
    procedure setupIOAPIC(expectedID : in Unsigned_32);

    ---------------------------------------------------------------------------
    -- enableIRQ
    -- Enables the IRQ and routes it to the CPU specified.
    -- @param irq - Use the interrupt/vector numbers specified in
    --  interrupt_numbers.ads, i.e. the timer interrupt would be 32, NOT 0.
    -- @param cpu - The value used here is the CPU's LAPIC ID.
    ---------------------------------------------------------------------------
    procedure enableIRQ(irq : in InterruptNumbers.x86interrupt;
                        cpu : in Unsigned_32);
end ioapic;
    
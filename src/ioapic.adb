-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- I/O APIC
-------------------------------------------------------------------------------
with textmode; use textmode;

package body ioapic
    with SPARK_Mode => On
is
    -- specify register, read the value
    function read(index : in IOAPICAddress) return Unsigned_32 with
        SPARK_Mode => On
    is
    begin
        ioapic.IOREGSEL := index;
        return ioapic.IOWIN;
    end read;

    -- specify register, write the value
    procedure write(index : in IOAPICAddress; val : in Unsigned_32) with
        SPARK_Mode => On
    is
    begin
        ioapic.IOREGSEL := index;
        ioapic.IOWIN := val;
    end write;

    -- Get the maximum number of entries in the I/O redirection table
    -- bits 16:23 of IOAPICVER are the max redirection entry
    -- bits 0:7 of IOAPICVER is the APIC version, should be 0x11
    -- bits 24:27 of IOAPICID is 
    procedure setupIOAPIC(expectedID : in Unsigned_32) with
        SPARK_Mode => On
    is
        version         : Unsigned_32;
        maxRedirect     : Unsigned_32;
        id              : Unsigned_32;
    begin
        -- assign the expected ID we got from the ACPI tables.
        --write(IOAPICID, expectedID);

        version := read(IOAPICVER) and 16#0000_00FF#;
        maxRedirect := Shift_Right(read(IOAPICVER) and 16#00FF_0000#, 16);
        id          := Shift_Right(read(IOAPICID) and 16#0F00_0000#, 24);

        if id /= expectedID then
            println(" WARNING: I/O APIC ID doesn't match the ID from ACPI tables.");
            print(" id: "); printd(id); print(" expected: "); printdln(expectedID);
        end if;
        --print(" Max Redirect: "); printdln(maxRedirect);

        -- set interrupts as edge-triggered, active-high, disabled.
        for i in 0..maxRedirect loop
            write(IOREDTBL_BASE + IOAPICAddress(2*i), 
                INT_MASKED or (Unsigned_32(InterruptNumbers.TIMER) + i));
            write(IOREDTBL_BASE + IOAPICAddress(2*i + 1), 0);
        end loop;
    end setupIOAPIC;

    procedure enableIRQ(irq : in InterruptNumbers.x86Interrupt; cpu : in Unsigned_32) with
        SPARK_Mode => On
    is
        tableEntry : Unsigned_32 := Unsigned_32(2 * (irq - 32));
    begin
        write(IOREDTBL_BASE + tableEntry, Unsigned_32(irq));
        write(IOREDTBL_BASE + tableEntry+1, Shift_Left(cpu, 24));
    end enableIRQ;

end ioapic;
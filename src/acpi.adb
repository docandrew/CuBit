-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- ACPI - Advanced Configuration and Power Interface
-------------------------------------------------------------------------------
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;

with BootAllocator;
with lapic;
--with pagetable;
with textmode; use textmode;
with virtmem;

package body acpi
    with SPARK_Mode => On
is
    -- As we go through each of the tables, stash a copy here.

    rsdp : RSDPRecord;

    rsdt : RSDTRecord;
    rsdtValid : Boolean := False;

    xsdt : XSDTRecord;
    xsdtValid : Boolean := False;

    --------------------------------------------------------------------------
    -- checksumACPI - ensure all bytes of the ACPI table sum to 0 mod x100.
    --------------------------------------------------------------------------
    function checksumACPI(tableAddr : in System.Address; len : in Unsigned_32)
        return Boolean with SPARK_Mode => Off  -- Storage_Array
    is
        tableBytes : Storage_Array(1..Storage_Offset(len))
            with Import, Address => tableAddr;
        sum : Unsigned_32 := 0;
    begin
        for i in 1..len loop
            sum := sum + Unsigned_32(tableBytes(Storage_Offset(i)));
        end loop;
        return (sum mod 16#100#) = 0;
    end checksumACPI;

    ---------------------------------------------------------------------------
    -- Convenience function for getting a 64-bit pointer from either 32-bit or
    -- 64-bit pointers as contained in an ACPI table. Some of the older ACPI
    -- tables contain 32-bit pointers that we need to zero-extend into the
    -- higher-half.
    -- @param acpiAddr - pointer val from the ACPI table, either a single
    --  64-bit address or a 32-bit address in the low dword followed by junk.
    -- @param ptrSize - size of the pointer. Depends on whether the table was
    --  an RSDT or an XSDT
    ---------------------------------------------------------------------------
    function makeTableAddress(acpiAddr : in Integer_Address; ptrSize : Unsigned_32)
        return Integer_Address with SPARK_Mode => On is 
    begin
        if ptrSize = 4 then
            return virtmem.P2V(16#0000_0000_FFFF_FFFF# and acpiAddr);
        else
            return virtmem.P2V(acpiAddr);
        end if;
    end makeTableAddress;

    ---------------------------------------------------------------------------
    -- getRSDP - convenience function for getting a RSDPRecord 
    -- @return True if successful, False if address does not point to an RSDP
    ---------------------------------------------------------------------------
    function getRSDP(rsdpAddr : in System.Address; rsdp : in out RSDPRecord)
        return Boolean with SPARK_Mode => On
    is    
        retRSDP : RSDPRecord with Import, Address => rsdpAddr;
    begin
        if retRSDP.signature = "RSD PTR " then
            rsdp := retRSDP;
            return True;
        else
            return False;
        end if;
    end getRSDP;

    ---------------------------------------------------------------------------
    -- getXSDT - convenience function for getting a XSDTRecord
    ---------------------------------------------------------------------------
    function getXSDT(sdtAddr : in System.Address; xsdt : in out XSDTRecord) 
        return Boolean with SPARK_Mode => On
    is
        retXSDT : XSDTRecord with Import, Address => sdtAddr;
    begin
        if retXSDT.header.signature = "XSDT" then
            xsdt := retXSDT;
            return True;
        else
            return False;
        end if;
    end getXSDT;

    ---------------------------------------------------------------------------
    -- getRSDT - convenience function for getting a RSDTRecord
    ---------------------------------------------------------------------------
    function getRSDT(sdtAddr : in System.Address; rsdt : in out RSDTRecord) 
        return Boolean with SPARK_Mode => On
    is
        retRSDT : RSDTRecord with Import, Address => sdtAddr;
    begin
        if retRSDT.header.signature = "RSDT" then
            rsdt := retRSDT;
            return True;
        else
            return False;
        end if;
    end getRSDT;
    
    ---------------------------------------------------------------------------
    -- getLAPIC - given an APIC table entry describing a local APIC, get it.
    ---------------------------------------------------------------------------
    function getLAPIC(lapicAddr : in System.Address; lapic : in out LAPICRecord)
        return Boolean with SPARK_Mode => On
    is
        retLAPIC : LAPICRecord with Import, Address => lapicAddr;
    begin
        if retLAPIC.header.length < (retLAPIC'Size / 8) then
            return False;
        else
            lapic := retLAPIC;
            return True;
        end if;
    end getLAPIC;

    ---------------------------------------------------------------------------
    -- getIOAPIC - given an APIC table entry describing an I/O APIC, get it.
    ---------------------------------------------------------------------------
    function getIOAPIC(ioapicAddr : in System.Address; ioapic : in out IOAPICRecord)
        return Boolean with SPARK_Mode => On
    is
        retIOAPIC : IOAPICRecord with Import, Address => ioapicAddr;
    begin
        if retIOAPIC.header.length < (retIOAPIC'Size / 8) then
            return False;
        else
            ioapic := retIOAPIC;
            return True;
        end if;
    end getIOAPIC;

    ---------------------------------------------------------------------------
    -- parseMADT - get information from the Multiple APIC Description Table
    ---------------------------------------------------------------------------
    procedure parseMADT(madtAddr : in System.Address)
        with SPARK_Mode => On
    is
        madt : MADTRecord
            with Import, Address => madtAddr;

        madtAddrInt : Integer_Address := To_Integer(madtAddr);
        entries_0 : constant Integer_Address := To_Integer(madt.entries'Address);
        --offset : Integer_Address := 0;
        entries_i : Integer_Address := entries_0;
        entryHeader : APICRecordHeader;

        endMADT : Integer_Address := madtAddrInt + Integer_Address(madt.header.length);
    begin

        APICLoop : loop
            ThisEntry : declare
                entryHeader : APICRecordHeader
                    with Import, Address => To_Address(entries_i);
                lapic : LAPICRecord;
                ioapic : IOAPICRecord;
            begin
                -- print("Checking APIC entry, type: ");
                -- print(entryHeader.apicType);
                -- print(" length: ");
                -- println(entryHeader.length);

                case entryHeader.apicType is
                    when LOCAL_APIC =>
                        print(" Found Local APIC:");
                        if getLAPIC(To_Address(entries_i), lapic) then
                            numCPUs := numCPUs + 1;
                            print(" LAPIC ID: ");
                            print(lapic.apicID);
                            print(", CPU ID: ");
                            println(lapic.processorID);
                        else
                            println(" WARNING: Bad LAPIC record");
                        end if;
                    when IO_APIC =>
                        print(" Found I/O APIC:");
                        if getIOAPIC(To_Address(entries_i), ioapic) then
                            numIOAPICs := numIOAPICs + 1;
                            print(" ID: ");
                            print(ioapic.apicID);
                            print(", Address: ");
                            print(ioapic.apicAddress);
                            print(", Base Interrupt Num: ");
                            println(ioapic.globalBase);

                            -- if, for some wild reason there are more than
                            -- one I/O APIC, just use the first one we find
                            -- for now.
                            if ioapicAddr = 0 then
                                ioapicAddr := virtmem.PhysAddress(ioapic.apicAddress);
                                ioapicID := Unsigned_32(ioapic.apicID);
                            end if;
                        else
                            println(" WARNING: Bad IO APIC Record");
                        end if;
                    when INT_SRC_OVERRIDE =>
                        println(" Found APIC Interrupt Source Override");
                    when NMI =>
                        println(" Found APIC NMI");
                    when LAPIC_NMI =>
                        println(" Found Local APIC NMI");
                    when LAPIC_ADDR_OVERRIDE =>
                        println(" Found Local APIC Address Override");
                    when LOCAL_SAPIC =>
                        println(" Found Local S-APIC");
                    when PLATFORM_INTERRUPT =>
                        println(" Found APIC Platform Interrupt");
                    when others =>
                        println(" WARNING: Found unrecognized APIC record");
                end case;
                -- advance to next entry.
                entries_i := entries_i + Integer_Address(entryHeader.length);
            end ThisEntry;
            
            -- print(" MADT entries(0): "); print(madt.entries.apicType);
            -- print(" length: "); println(madt.entries.length);

            exit APICLoop when entries_i >= endMADT or entryHeader.length = 0;
        end loop APICLoop;

        lapicAddr := virtmem.PhysAddress(madt.lapicAddress);
        print(" LAPIC Physical Address:   "); println(lapicAddr);
        print(" Number of CPUs: ");
        println(numCPUs);
        print(" Number of I/O APICs: ");
        println(numIOAPICs);
    end parseMADT;


    -- parse ACPI tables, get information we need out of them.
    function setup return Boolean
    is
        use System;
        rsdpAddr    : constant System.Address := findRSDP;
        
        -- use XSDT if available.
        useXSDT     : Boolean := False;

        -- sdtAddr refers to either the RSDT or the XSDT.
        sdtAddr     : Unsigned_64;
        sdtPtrSize  : Unsigned_32 := 4;
        sdtHeader   : SDTRecordHeader;
        
        numEntries  : Unsigned_32;      -- number of entries in the SDT
        entries_0   : Integer_Address;  -- address of first SDT entry
        offset      : Integer_Address;  -- offset to i-th SDT entry
        entries_i   : Integer_Address;  -- entries_0 + offset
    begin

        if rsdpAddr = Null_Address then
            println("ACPI RSDP not found.");
            return False;
        end if;

        if not getRSDP(rsdpAddr, rsdp) then
            println("Invalid ACPI Tables (No RSDP)");
            return False;
        end if;

        println("Found ACPI Tables. ");
        -- print("RSDP at:      "); println(rsdpAddr);
        -- print(" checksum:    "); println(rsdp.checksum);
        -- print(" OEMID:       "); println(rsdp.OEMID);
        -- print(" revision:    "); println(rsdp.revision);
        -- print(" RSDT addr:   "); println(rsdp.RSDTAddress);

        if rsdp.revision = 2 then
            -- print(" RSDP length: "); println(rsdp.length);
            -- print(" XSDT addr:   "); println(rsdp.XSDTAddress);
            -- print(" exChecksum:  "); println(rsdp.exChecksum);
            
            useXSDT := True;
        end if;

        if useXSDT then
            -- Per the spec, if XSDT is available, we must use it.
            sdtAddr := rsdp.XSDTAddress;
            print("ACPI XSDT at:  "); println(sdtAddr);
            
            if not getXSDT(To_Address(virtmem.P2V(Integer_Address(sdtAddr))), xsdt) then
                println("Error reading XSDT, defaulting to RSDT");
                useXSDT := False;   -- try and fall back on RSDT
            else
                sdtPtrSize := 8;
                sdtHeader := xsdt.header;              
                entries_0 := Integer_Address(sdtAddr + (SDTRecordHeader'Size / 8));
            end if;
        end if;

        -- fall back to RSDT if XSDT not available or broken.
        if not useXSDT then
            sdtAddr := Unsigned_64(rsdp.RSDTAddress);
            print("ACPI RSDT at:  "); println(sdtAddr);

            if not getRSDT(To_Address(virtmem.P2V(Integer_Address(sdtAddr))), rsdt) then
                println("Error reading RSDT");
                return False;
            else
                sdtPtrSize := 4;
                sdtHeader := rsdt.header;
                entries_0 := Integer_Address(sdtAddr + (SDTRecordHeader'Size / 8));
            end if;
        end if;

        numEntries := (sdtHeader.length - (SDTRecordHeader'Size / 8)) / sdtPtrSize;

        print(" signature:   "); println(sdtHeader.signature);
        print(" length:      "); println(sdtHeader.length);
        print(" revision:    "); println(sdtHeader.revision);
        print(" checksum:    "); println(sdtHeader.checksum);
        print(" OEMID:       "); println(sdtHeader.OEMID);
        print(" OEMTableID:  "); println(sdtHeader.OEMTableID);
        print(" OEMRevision: "); println(sdtHeader.OEMRevision);
        print(" creatorID:   "); println(sdtHeader.creatorID);
        print(" creatorRev:  "); println(sdtHeader.creatorRevision);
        print(" # entries: ");
        println(numEntries);

        if not checksumACPI(To_Address(virtmem.P2V(Integer_Address(sdtAddr))),
                            sdtHeader.length) then
            println(" WARNING: ACPI Checksum error.");
            return False;
        else
            println(" ACPI Checksum OK.");
        end if;

        -- Iterate over each of the entries after the SDT header.
        for i in 0 .. numEntries-1 loop

            offset := Integer_Address(i * sdtPtrSize);
            entries_i := makeTableAddress(entries_0 + offset, sdtPtrSize);

            --print("Reading Table at ");
            --println(entries_i);
            printRecordHeader : declare

                entries_i_val : constant Integer_Address
                    with Import, Address => To_Address(entries_i);

                descHdr : DescriptionHeader
                    with Import, Address => To_Address(makeTableAddress(entries_i_val, sdtPtrSize));
            begin
                -- print("entries_0: "); println(entries_0);
                -- print("offset: "); println(offset);
                -- --print("newint: "); println(newint);
                -- print("entries("); print(i); print("): "); println(entries_i);
                -- --print("entries_i_val: "); println(entries_i_val);
                -- println;
                -- print(" Found ACPI Record:  "); print(descHdr.signature);
                -- print(" length:  "); println(descHdr.length);

                if descHdr.signature = "FACP" then
                    parseFADT : declare
                    fadt : FADTRecord
                        with Import, Address => descHdr'Address;
                    begin
                        print(" DSDT Address:          "); println(fadt.dsdt);
                        print(" DSDT extended Address: "); println(fadt.exDsdt);
                    end parseFADT;
                elsif descHdr.signature = "APIC" then
                    parseMADT(descHdr'Address);
                elsif descHdr.signature = "MCFG" then
                    println(" MCFG present, not supported.");
                    -- parseMCFG : declare
                    -- mcfg : MCFGRecord
                    --     with Import, Address => descHdr'Address;
                    -- begin
                    --     println(" MCFG Address:          ");
                    -- end parseMCFG;
                elsif descHdr.signature = "HPET" then
                    println(" HPET present, not supported.");
                    -- parseHPET : declare
                    -- hpet : HPETRecord
                    --     with Import, Address => descHdr'Address;
                    -- begin
                    --     println(" HPET Address:          "); println(hpet.);
                    -- end parseHPET;
                elsif descHdr.signature = "SSDT" then
                    println(" SSDT present, not supported.");
                else
                    print("Unsupported ACPI table "); print(descHdr.signature);
                    print(" with length "); println(descHdr.length);
                end if;
            end printRecordHeader;
        end loop;

        return True;
    end setup;

    function findRSDP return System.Address
    is
        --use System.Storage_Elements;
        package ToRSDP is new System.Address_To_Access_Conversions(RSDPRecord);

        type RSDPAccess is access all RSDPRecord;
        rsdpAddr : Integer_Address;
        rsdp : RSDPAccess;
    begin
        -- Check BIOS area E0000 to FFFFF
        rsdpAddr := Integer_Address(virtmem.P2V(16#E0000#));

        -- a bit ugly, we just cast addresses to an rsdp and see if the
        -- signature matches what we'd expect.
        search : loop
            rsdp := RSDPAccess(ToRSDP.To_Pointer(To_Address(rsdpAddr)));

            if rsdp.signature = "RSD PTR " then
                return To_Address(rsdpAddr);
            end if;

            rsdpAddr := rsdpAddr + 16;

            exit search when rsdpAddr >= virtmem.P2V(16#FFFFF#);
        end loop search;

        return System.Null_Address;
    end findRSDP;

end acpi;
-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- ACPI - Advanced Configuration and Power Interface
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with Multiboot;
with CPU_Topology.Boot;
with Multiboot2_Info;
with Firmware_Tables;
with Firmware_Tables.HPET;
with Firmware_Tables.Provisioning;
with BuddyAllocator;

with TextIO; use TextIO;

-- Ada implementation: custom storage, address overlays or live context state.
-- Only separately annotated SPARK policy/state routines carry proof obligations.
package body acpi is
    use type System.Address;
    use type Firmware_Tables.Admission;
    use type Firmware_Tables.Root_Kind;

    -- Raw adapter: firmware tables are immutable during boot. Check retained
    -- backing before every overlay; shared SPARK code admits byte contents.
    function Read_Root_At (Physical : Unsigned_64) return Firmware_Tables.Root_Result is
        Length : Unsigned_32 := 20;
    begin
        if not Multiboot.Firmware_Readable (Physical, 20) then
            return (Status => Firmware_Tables.Truncated);
        end if;
        declare
            Prefix : Firmware_Tables.Bytes (1 .. 20) with Import,
              Address => Virtmem.P2Va (Integer_Address (Physical));
        begin
            if Prefix (16) >= 2 then
                if not Multiboot.Firmware_Readable (Physical, 36) then
                    return (Status => Firmware_Tables.Truncated);
                end if;
                declare
                    Raw : Multiboot2_Info.Bytes (0 .. 35) with Import,
                      Address => Prefix'Address;
                begin
                    Length := Multiboot2_Info.Read_32 (Raw, 20);
                end;
            end if;
        end;
        if Length < 20 or else Length > 4096 or else
          not Multiboot.Firmware_Readable (Physical, Unsigned_64 (Length))
        then return (Status => Firmware_Tables.Invalid_Length); end if;
        declare
            Raw : Firmware_Tables.Bytes (1 .. Natural (Length)) with Import,
              Address => Virtmem.P2Va (Integer_Address (Physical));
        begin
            return Firmware_Tables.Read_Root (Raw);
        end;
    end Read_Root_At;

    function Admit_Table (Physical : Unsigned_64; Expected : String := "") return Boolean is
        Length : Unsigned_32;
        Name : Firmware_Tables.Signature;
    begin
        if not Multiboot.Firmware_Readable (Physical, 36) then return False; end if;
        declare
            Prefix : Multiboot2_Info.Bytes (0 .. 35) with Import,
              Address => Virtmem.P2Va (Integer_Address (Physical));
        begin
            Length := Multiboot2_Info.Read_32 (Prefix, 4);
            for I in Name'Range loop Name (I) := Character'Val (Prefix (I - 1)); end loop;
        end;
        if (Expected /= "" and then Name /= Expected) or else
          Length < 36 or else Length > Unsigned_32 (Positive'Last) or else
          not Multiboot.Firmware_Readable (Physical, Unsigned_64 (Length))
        then return False; end if;
        declare
            Raw : Firmware_Tables.Bytes (1 .. Natural (Length)) with Import,
              Address => Virtmem.P2Va (Integer_Address (Physical));
        begin
            return Firmware_Tables.Read_Table (Raw, Name).Status = Firmware_Tables.Accepted;
        end;
    end Admit_Table;
    Source_Inventory : Firmware_Tables.Catalog.State;
    -- One checked allocator reservation backs one typed snapshot construction.
    -- No allocator call is hidden in the pool callback, so ordinary exhaustion
    -- can fail closed before GNAT initializes the allocated object.
    package Capture_Storage is
    type Capture_Pool is limited record
        Base : System.Address := System.Null_Address;
        Capacity : Storage_Count := 0;
        Consumed : Boolean := False;
    end record;
    pragma Simple_Storage_Pool_Type (Capture_Pool);
    procedure Allocate
      (Pool : in out Capture_Pool; Address : out System.Address;
       Size, Alignment : Storage_Count);
    procedure Deallocate
      (Pool : in out Capture_Pool; Address : System.Address;
       Size, Alignment : Storage_Count);
    function Storage_Size (Pool : Capture_Pool) return Storage_Count;
    end Capture_Storage;
    package body Capture_Storage is
    procedure Allocate
      (Pool : in out Capture_Pool; Address : out System.Address;
       Size, Alignment : Storage_Count) is
    begin
        if Pool.Consumed or else Pool.Base = System.Null_Address
          or else Size > Pool.Capacity or else Alignment <= 0
          or else To_Integer (Pool.Base) mod Integer_Address (Alignment) /= 0
        then
            raise Storage_Error with "invalid ACPI snapshot reservation";
        end if;
        Address := Pool.Base;
        Pool.Consumed := True;
    end Allocate;
    procedure Deallocate
      (Pool : in out Capture_Pool; Address : System.Address;
       Size, Alignment : Storage_Count) is
    begin
        -- Boot-lifetime ownership: no recapture or deallocation is supported.
        raise Program_Error with "ACPI snapshot lifetime violation";
    end Deallocate;
    function Storage_Size (Pool : Capture_Pool) return Storage_Count is
      (Pool.Capacity);
    end Capture_Storage;
    use Capture_Storage;
    Snapshot_Pool : Capture_Pool;
    type Snapshot_Access is access Firmware_Tables.Snapshots.State;
    for Snapshot_Access'Simple_Storage_Pool use Snapshot_Pool;
    Boot_Snapshot : Snapshot_Access := null;
    Capture_Attempted : Boolean := False;
    use type Firmware_Tables.Snapshots.Phase;
    function Table_Snapshot_State return Firmware_Tables.Snapshots.Phase is
      (if Boot_Snapshot /= null then Firmware_Tables.Snapshots.Current (Boot_Snapshot.all)
       elsif Capture_Attempted then Firmware_Tables.Snapshots.Failed
       else Firmware_Tables.Snapshots.Empty);
    function Snapshot_Table_Count return Natural is
      (if Boot_Snapshot = null then 0
       else Firmware_Tables.Snapshots.Count (Boot_Snapshot.all));

    function Table_Inventory_State return Firmware_Tables.Catalog.Phase is
      (Firmware_Tables.Catalog.Current (Source_Inventory));
    function Table_Count return Natural is
      (Firmware_Tables.Catalog.Count (Source_Inventory));
    function Table_Source (Index : Positive)
      return Firmware_Tables.Catalog.Descriptor is
      (Firmware_Tables.Catalog.Item (Source_Inventory, Index));
    procedure Capture_Tables is
        use Firmware_Tables.Provisioning;
        -- Allocator policy, not a firmware format limit. Object metadata must
        -- also fit this block; check its actual representation before reserving.
        Block_Limit : constant Storage_Count :=
          BuddyAllocator.blockSize (BuddyAllocator.Order'Last);
        Budget : constant Positive := Positive
          (Storage_Count'Min (Block_Limit, Storage_Count (Positive'Last)));
        Layout : constant Plan := Select_Capacity
          (Source_Inventory, Firmware_Tables.Catalog.Max_Tables, Budget, Budget);
        Success : Boolean;
    begin
        if Capture_Attempted then return; end if;
        Capture_Attempted := True;
        if Layout.Status /= Approved then
            println ("ACPI: table inventory exceeds snapshot allocation policy");
            return;
        end if;
        declare
            subtype Sized_Snapshot is Firmware_Tables.Snapshots.State
              (Layout.Needed.Tables, Positive (Layout.Needed.Bytes), Layout.Needed.Largest);
            Required : constant Storage_Count :=
              Storage_Count ((Sized_Snapshot'Object_Size + 7) / 8);
            Order : BuddyAllocator.Order;
            Block : System.Address;
        begin
            if Required > Block_Limit then
                println ("ACPI: snapshot metadata exceeds allocator block limit");
                return;
            end if;
            Order := BuddyAllocator.getOrder (Required);
            BuddyAllocator.alloc (Order, Block);
            if Block = BuddyAllocator.NO_BLOCK_AVAILABLE then
                println ("ACPI: snapshot allocation failed");
                return;
            end if;
            Snapshot_Pool.Base := Block;
            Snapshot_Pool.Capacity := BuddyAllocator.blockSize (Order);
            -- Initialize the entire reserved block, including allocator rounding
            -- and record padding. This kernel object itself is never granted.
            declare
                Raw : Firmware_Tables.Bytes (1 .. Natural (Snapshot_Pool.Capacity))
                  with Import, Address => Block;
            begin
                Raw := (others => 0);
            end;
            Boot_Snapshot := new Sized_Snapshot;
        end;
        Firmware_Tables.Snapshots.Begin_Snapshot (Boot_Snapshot.all, Table_Count);
        if Table_Snapshot_State /= Firmware_Tables.Snapshots.Building then return; end if;
        for I in 1 .. Table_Count loop
            declare
                D : constant Firmware_Tables.Catalog.Descriptor := Table_Source (I);
            begin
                if not Multiboot.Firmware_Readable (D.Physical, Unsigned_64 (D.Extent)) then
                    Firmware_Tables.Snapshots.Reject (Boot_Snapshot.all);
                    exit;
                end if;
                declare
                    Source : Firmware_Tables.Bytes (1 .. D.Extent) with Import,
                      Address => Virtmem.P2Va (Integer_Address (D.Physical));
                begin
                    Firmware_Tables.Snapshots.Append
                      (Boot_Snapshot.all, D, Source, Success);
                end;
            end;
            if not Success then exit; end if;
        end loop;
        Firmware_Tables.Snapshots.Seal (Boot_Snapshot.all);
    end Capture_Tables;

    procedure Copy_Table
      (Index : Positive; Destination : out Firmware_Tables.Bytes;
       Result : out Table_Copy_Status) is
        Success : Boolean;
    begin
        Destination := (others => 0);
        Result := Snapshot_Unavailable;
        if Table_Snapshot_State /= Firmware_Tables.Snapshots.Ready then return; end if;
        Result := No_Table;
        if Index > Snapshot_Table_Count then return; end if;
        Firmware_Tables.Snapshots.Copy (Boot_Snapshot.all, Index, Destination, Success);
        Result := (if Success then Copied else Buffer_Too_Small);
    end Copy_Table;

    function Table_Page_Exposure (Index : Positive)
      return Firmware_Tables.Exposure.Plan is
      (Firmware_Tables.Exposure.Describe (Source_Inventory, Index));
    function Table_Backing_Is_Reclaim_RAM (Index : Positive) return Boolean is
        Span : constant Firmware_Tables.Exposure.Window :=
          Firmware_Tables.Exposure.Page_Window (Table_Source (Index));
    begin
        return Multiboot.Firmware_Reclaim_Pages
          (Span.First, Span.Pages * Firmware_Tables.Exposure.Page_Size);
    end Table_Backing_Is_Reclaim_RAM;

    -- Raw adapter: call only after Admit_Table/getRSDT/getXSDT admitted the
    -- complete immutable extent. Firmware backing remains reserved by the
    -- allocator; a future export adapter must enforce page exposure/lifetime.
    procedure Retain_Source (Physical : Unsigned_64) is
    begin
        if Physical = 0 then
            Firmware_Tables.Catalog.Reject (Source_Inventory);
            return;
        end if;
        declare
            Header : Multiboot2_Info.Bytes (0 .. 35) with Import,
              Address => Virtmem.P2Va (Integer_Address (Physical));
            Name : Firmware_Tables.Signature;
        begin
            for I in Name'Range loop
                Name (I) := Character'Val (Header (I - 1));
            end loop;
            Firmware_Tables.Catalog.Include (Source_Inventory,
              (Physical => Physical,
               Extent => Natural (Multiboot2_Info.Read_32 (Header, 4)),
               Name => Name, Revision => Header (8)));
        end;
    end Retain_Source;
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
        return Boolean   -- Storage_Array
    is
        tableBytes : Storage_Array(1..Storage_Offset(len))
            with Import, Volatile, Address => tableAddr;
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
        return Integer_Address  is
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
    procedure getRSDP(rsdpAddr : in System.Address; rsdp : in out RSDPRecord;
        success : out Boolean)
    is
        Root : constant Firmware_Tables.Root_Result :=
          Read_Root_At (Unsigned_64 (Virtmem.V2P (rsdpAddr)));
    begin
        if Root.Status = Firmware_Tables.Accepted then
            rsdp.revision := (if Root.Kind = Firmware_Tables.XSDT then 2 else 0);
            rsdp.XSDTAddress := Root.Address;
            rsdp.RSDTAddress := (if Root.Kind = Firmware_Tables.RSDT then Unsigned_32 (Root.Address) else 0);
            success := True;
        else
            success := False;
        end if;
    end getRSDP;

    ---------------------------------------------------------------------------
    -- getXSDT - convenience function for getting a XSDTRecord
    ---------------------------------------------------------------------------
    procedure getXSDT(sdtAddr : in System.Address; xsdt : in out XSDTRecord;
        success : out Boolean)
    is
        retXSDT : XSDTRecord with Import, Volatile, Address => sdtAddr;
    begin
        if Admit_Table (Unsigned_64 (Virtmem.V2P (sdtAddr)), "XSDT") and then
          retXSDT.header.length >= 44 and then (retXSDT.header.length - 36) mod 8 = 0
        then
            xsdt := retXSDT;
            success := True;
        else
            success := False;
        end if;
    end getXSDT;

    ---------------------------------------------------------------------------
    -- getRSDT - convenience function for getting a RSDTRecord
    ---------------------------------------------------------------------------
    procedure getRSDT(sdtAddr : in System.Address; rsdt : in out RSDTRecord;
        success : out Boolean)
    is
        retRSDT : RSDTRecord with Import, Volatile, Address => sdtAddr;
    begin
        if Admit_Table (Unsigned_64 (Virtmem.V2P (sdtAddr)), "RSDT") and then
          retRSDT.header.length >= 40 and then (retRSDT.header.length - 36) mod 4 = 0
        then
            rsdt := retRSDT;
            success := True;
        else
            success := False;
        end if;
    end getRSDT;

    ---------------------------------------------------------------------------
    -- getLAPIC - given an APIC table entry describing a local APIC, get it.
    ---------------------------------------------------------------------------
    procedure getLAPIC(lapicAddr : in System.Address; lapic : in out LAPICRecord;
        success : out Boolean)
    is
        retLAPIC : LAPICRecord with Import, Volatile, Address => lapicAddr;
    begin
        if retLAPIC.header.length < (retLAPIC'Size / 8) then
            success := False;
        else
            lapic := retLAPIC;
            success := True;
        end if;
    end getLAPIC;

    ---------------------------------------------------------------------------
    -- getIOAPIC - given an APIC table entry describing an I/O APIC, get it.
    ---------------------------------------------------------------------------
    procedure getIOAPIC(ioapicAddr : in System.Address; ioapic : in out IOAPICRecord;
        success : out Boolean)
    is
        retIOAPIC : IOAPICRecord with Import, Volatile, Address => ioapicAddr;
    begin
        if retIOAPIC.header.length < (retIOAPIC'Size / 8) then
            success := False;
        else
            ioapic := retIOAPIC;
            success := True;
        end if;
    end getIOAPIC;

    ---------------------------------------------------------------------------
    -- parseMADT - get information from the Multiple APIC Description Table
    ---------------------------------------------------------------------------
    procedure parseMADT (madtAddr : in System.Address)

    is
        madt : MADTRecord
            with Import, Volatile, Address => madtAddr;

        madtAddrInt : Integer_Address := To_Integer(madtAddr);
        entries_0 : constant Integer_Address := To_Integer(madt.entries'Address);
        entries_i : Integer_Address := entries_0;

        endMADT : Integer_Address := madtAddrInt + Integer_Address(madt.header.length);
    begin

        while entries_i < endMADT loop
            if endMADT - entries_i < 2 then
                raise Constraint_Error with "ACPI: truncated MADT entry header";
            end if;
            ThisEntry : declare
                entryHeader : APICRecordHeader
                    with Import, Volatile, Address => To_Address(entries_i);
                lapic : LAPICRecord;
                ioapic : IOAPICRecord;
                ok : Boolean;
            begin
                if entryHeader.length < 2 or else
                  Integer_Address (entryHeader.length) > endMADT - entries_i or else
                  (entryHeader.apicType = LOCAL_APIC and entryHeader.length < 8) or else
                  (entryHeader.apicType = IO_APIC and entryHeader.length < 12)
                then
                    raise Constraint_Error with "ACPI: invalid MADT entry extent";
                end if;
                -- print("Checking APIC entry, type: ");
                -- print(entryHeader.apicType);
                -- print(" length: ");
                -- println(entryHeader.length);

                case entryHeader.apicType is
                    when LOCAL_APIC =>
                        print(" Found Local APIC:");
                        getLAPIC(To_Address(entries_i), lapic, ok);
                        if ok then
                            if (lapic.flags and LAPIC_ENABLED) /= 0 then
                                if lapic.apicID = 255 then
                                    raise Constraint_Error with "ACPI: broadcast APIC ID is not a CPU";
                                end if;
                                CPU_Topology.Include_CPU
                                  (CPU_Topology.Boot.CPUs, lapic.apicID);
                                numCPUs := Natural (CPU_Topology.Count (CPU_Topology.Boot.CPUs));
                            end if;
                            print(" LAPIC ID: ");
                            print(lapic.apicID);
                            print(", CPU ID: ");
                            println(lapic.processorID);
                        else
                            println(" WARNING: Bad LAPIC record");
                        end if;
                    when IO_APIC =>
                        print(" Found I/O APIC:");
                        getIOAPIC(To_Address(entries_i), ioapic, ok);
                        if ok then
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
        end loop;

        lapicAddr := Virtmem.PhysAddress(madt.lapicAddress);
        print (" LAPIC Physical Address:   "); println (lapicAddr);
        print (" Number of CPUs: ");
        println (numCPUs);
        print (" Number of I/O APICs: ");
        println (numIOAPICs);
    end parseMADT;

    ---------------------------------------------------------------------------
    -- parseDSDT
    ---------------------------------------------------------------------------
    procedure parseDSDT (dsdtAddr : System.Address) is
        dsdt : DSDTRecord with Import, Volatile, Address => dsdtAddr;
    begin
        if dsdt.header.signature /= "DSDT" then
            println ("ACPI: Error parsing DSDT, bad address or corrupted table.");
            return;
        end if;

        -- @TODO AML bytecode...
    end parseDSDT;

    ---------------------------------------------------------------------------
    -- parseMCFG
    -- Need to get the PCIe configuration addresses here
    ---------------------------------------------------------------------------
    procedure parseMCFG (mcfgAddr : System.Address) is

        mcfg     : MCFGRecord with Import, Volatile, Address => mcfgAddr;
        tableLen : Unsigned_32;
        curTable : System.Address;
        idx      : Unsigned_32 := 0;
    begin
        if mcfg.header.signature /= "MCFG" then
            println ("ACPI: Error parsing MCFG, bad address or corrupted table.");
        end if;

        println ("ACPI: PCIe Configuration Space (MCFG)");

        tableLen := 1 + (mcfg.header.length - (mcfg'Size / 8)) / (MMAPConfigurationSpace'Size / 8);

        print (" Length: "); println (mcfg.header.length);
        print (" Number of Configuration Spaces: "); println (tableLen);

        println;
        print (" Config space ");
        print ("  Base Address:  "); println (mcfg.entries.baseAddr);
        print ("  Segment Group: "); println (mcfg.entries.pciSegGroupNum);
        print ("  Start Bus:     "); println (mcfg.entries.startBusNum);
        print ("  End Bus:       "); println (mcfg.entries.endBusNum);

        -- Save this for later when we're enumerating PCI devices.
        pcieConfig := mcfg.entries;

        -- @TODO this code works if there are multiple configuration spaces, for
        -- now just use the first one we find.
        -- if tableLen = 0 then
        --     return;
        -- end if;
        --
        -- declare
        --     mmapConfigs : MMAPConfigSpaces(1..tableLen) with
        --         Import, Address => mcfg.entries'Address;
        -- begin
        --     for m of mmapConfigs loop
        --         println;
        --         print (" Config space ");    printdln (idx);
        --         print ("  Base Address:  "); println (m.baseAddr);
        --         print ("  Segment Group: "); println (m.pciSegGroupNum);
        --         print ("  Start Bus:     "); println (m.startBusNum);
        --         print ("  End Bus:       "); println (m.endBusNum);
        --         idx := idx + 1;
        --     end loop;
        -- end;
    end parseMCFG;

    ---------------------------------------------------------------------------
    -- setup
    -- parse ACPI tables, get information we need out of them.
    ---------------------------------------------------------------------------
    function setup return Boolean

    is
        use System;
        rsdpAddr    : System.Address;
        handedRoot : constant Firmware_Tables.Root_Result := Multiboot.Firmware_Root;

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
        ok          : Boolean;
    begin

        if Table_Snapshot_State /= Firmware_Tables.Snapshots.Empty then
            println ("ACPI: repeated discovery after snapshot construction refused");
            return False;
        end if;
        Firmware_Tables.Catalog.Reset (Source_Inventory);

        if Multiboot.Tagged_Boot then
            if handedRoot.Status /= Firmware_Tables.Accepted then
                println ("ACPI: Multiboot2 root missing");
                return False;
            end if;
            rsdp.revision := (if handedRoot.Kind = Firmware_Tables.XSDT then 2 else 0);
            rsdp.XSDTAddress := handedRoot.Address;
            rsdp.RSDTAddress := (if handedRoot.Kind = Firmware_Tables.RSDT then
              Unsigned_32 (handedRoot.Address) else 0);
            println ("ACPI: validated Multiboot2 root handoff");
        else
        rsdpAddr := findRSDP;
        if rsdpAddr = Null_Address then
            println("ACPI RSDP not found.");
            return False;
        end if;

        getRSDP(rsdpAddr, rsdp, ok);
        if not ok then
            println("Invalid ACPI Tables (No RSDP)");
            return False;
        end if;
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

            getXSDT(To_Address(virtmem.P2V(Integer_Address(sdtAddr))), xsdt, ok);
            if not ok then
                println("ACPI: invalid XSDT; refusing fallback");
                return False;
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

            getRSDT(To_Address(virtmem.P2V(Integer_Address(sdtAddr))), rsdt, ok);
            if not ok then
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

        Retain_Source (sdtAddr);

        -- Iterate over each of the entries after the SDT header.
        for i in 0 .. numEntries-1 loop

            offset := Integer_Address(i * sdtPtrSize);
            entries_i := makeTableAddress(entries_0 + offset, sdtPtrSize);

            --print("Reading Table at ");
            --println(entries_i);
            printRecordHeader : declare

                entries_i_val : Integer_Address;
            begin
                if sdtPtrSize = 4 then
                    declare
                        Raw : Unsigned_32 with Import, Address => To_Address (entries_i);
                    begin entries_i_val := Integer_Address (Raw); end;
                else
                    declare
                        Raw : Unsigned_64 with Import, Address => To_Address (entries_i);
                    begin entries_i_val := Integer_Address (Raw); end;
                end if;
                if not Admit_Table (Unsigned_64 (entries_i_val)) then
                    println ("ACPI: child table failed admission"); return False;
                end if;
                Retain_Source (Unsigned_64 (entries_i_val));
                declare
                descHdr : DescriptionHeader with Import, Volatile,
                  Address => Virtmem.P2Va (entries_i_val);
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
                        with Import, Volatile, Address => descHdr'Address;
                    tableLength : constant Unsigned_32 := descHdr.length;
                    -- End offsets derive from the wire representation. ACPI
                    -- 1.0 stops after Flags; X_DSDT is an optional later field.
                    legacyBytes : constant Unsigned_32 :=
                      fadt.flags'Position + fadt.flags'Size / 8;
                    extendedDSDTEnd : constant Unsigned_32 :=
                      fadt.exDsdt'Position + fadt.exDsdt'Size / 8;
                    dsdtPhysical : Unsigned_64;
                    begin
                        if tableLength < legacyBytes then
                            println ("ACPI: truncated FADT");
                            return False;
                        end if;
                        dsdtPhysical := Unsigned_64 (fadt.dsdt);
                        if tableLength >= extendedDSDTEnd then
                            declare
                                extended : constant Unsigned_64 := fadt.exDsdt;
                            begin
                                if extended /= 0 then
                                    dsdtPhysical := extended;
                                end if;
                            end;
                        end if;
                        print (" DSDT selected address: "); println (dsdtPhysical);

                        -- @TODO this will be how we can shutdown/sleep the machine
                        print (" PM1A Control Block:    "); println (fadt.PM1AEventBlock);

                        if dsdtPhysical = 0 or else
                          dsdtPhysical > Unsigned_64 (virtmem.PhysAddress'Last) -
                            (SDTRecordHeader'Size / 8 - 1)
                        then
                            println ("ACPI: invalid DSDT physical extent");
                            return False;
                        end if;
                        if not Admit_Table (dsdtPhysical, "DSDT") then
                            println ("ACPI: invalid DSDT"); return False;
                        end if;
                        Retain_Source (dsdtPhysical);
                        parseDSDT (To_Address(virtmem.P2V(Integer_Address(dsdtPhysical))));
                    end parseFADT;

                elsif descHdr.signature = "APIC" then
                    if descHdr.length < 44 then
                        println ("ACPI: truncated MADT"); return False;
                    end if;
                    parseMADT(descHdr'Address);

                elsif descHdr.signature = "MCFG" then
                    if descHdr.length < 60 or else (descHdr.length - 44) mod 16 /= 0 then
                        println ("ACPI: invalid MCFG extent"); return False;
                    end if;
                    parseMCFG (descHdr'Address);

                elsif descHdr.signature = "HPET" then
                    declare
                        Data : Firmware_Tables.Bytes (1 .. Natural (descHdr.length))
                          with Import, Address => descHdr'Address;
                        Item : constant Firmware_Tables.HPET.Descriptor :=
                          Firmware_Tables.HPET.Decode
                            (Data, Unsigned_64 (Integer_Address'Min
                              (Virtmem.PhysAddress'Last,
                               Integer_Address'Last - Virtmem.LINEAR_BASE)));
                    begin
                        if not Item.Valid or else hpetAddr /= 0 then
                            println ("ACPI: unsupported or duplicate HPET descriptor");
                            return False;
                        end if;
                        hpetAddr := Virtmem.PhysAddress (Item.Base);
                        print ("HPET register base: "); println (hpetAddr);
                    end;
                elsif descHdr.signature = "SSDT" then
                    println(" SSDT present, not supported.");
                else
                    print("Unsupported ACPI table "); print(descHdr.signature);
                    print(" with length "); println(descHdr.length);
                end if;
                end;
            end printRecordHeader;
        end loop;

        Firmware_Tables.Catalog.Seal (Source_Inventory);
        Capture_Tables;
        if Table_Snapshot_State /= Firmware_Tables.Snapshots.Ready then
            println ("ACPI: immutable snapshot unavailable; userspace handoff disabled");
        end if;
        return True;
    end setup;

    ---------------------------------------------------------------------------
    -- findRSDP
    ---------------------------------------------------------------------------
    function findRSDP return System.Address
    is
        function Scan (First, Last : Unsigned_64) return System.Address is
            Cursor : Unsigned_64 := First;
        begin
            while Cursor <= Last and then Last - Cursor >= 19 loop
                if Read_Root_At (Cursor).Status = Firmware_Tables.Accepted then
                    return Virtmem.P2Va (Integer_Address (Cursor));
                end if;
                Cursor := Cursor + 16;
            end loop;
            return System.Null_Address;
        end Scan;
        EBDA_Segment : Unsigned_16 with Import, Address => Virtmem.P2Va (16#40E#);
        EBDA : constant Unsigned_64 := Unsigned_64 (EBDA_Segment) * 16;
        Found : System.Address;
    begin
        -- Defined BIOS discovery windows only; UEFI never scans RAM.
        if EBDA >= 16#80000# and EBDA <= 16#9FC00# then
            Found := Scan (EBDA, EBDA + 1023);
            if Found /= System.Null_Address then return Found; end if;
        end if;
        return Scan (16#E0000#, 16#FFFFF#);
    end findRSDP;

end acpi;

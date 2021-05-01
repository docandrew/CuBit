-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- ACPI - Advanced Configuration and Power Interface
--
-- Covers up to ACPI Version 3.0
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Interfaces.C;
with System;

with virtmem;

package acpi 
    with SPARK_Mode => On
is

    -- Base address of the mmap-ed LAPIC registers we find when parsing the
    -- ACPI tables.
    lapicAddr   : Virtmem.PhysAddress := 0;
    ioapicAddr  : Virtmem.PhysAddress := 0;
    ioapicID    : Unsigned_32 := 0;

    type ACPIVersion is (AcpiVersion1, AcpiVersion2);

    -- The number of CPUs found in the call to setupACPI
    numCPUs : Natural := 0;

    -- The number of IOAPICs found in the call to setupACPI
    numIOAPICs : Natural := 0;

    ---------------------------------------------------------------------------
    -- ACPI Root System Description Pointer (RSDP)
    -- Gives us the address of the Root System Description Table (RSDT)
    -- @field signature - always "RSD PTR " (note trailing space)
    -- @field checksum - checksum of fields defined in ACPI 1.0 spec
    -- @field OEMID - OEM-supplied string
    -- @field revision - Revision of this structure. Larger revision numbers
    --   backwards compatible to lower revision numbers.
    -- @field RSDTAddress - 32-bit physical address of the RSDT
    --
    -- If revision is 2, then this record has additional ACPI 2.0 fields:
    --
    -- @field length - Length of the ACPI table, in bytes, including header.
    -- @field XSDTAddress - 64-bit physical address of the XSDT
    -- @field exChecksum - checksum of the entire ACPI table, including both
    --  checksum fields.
    -- @field reserved1 - not used
    -- @field reserved2 - not used
    -- @field reserved3 - not used
    --
    -- Note: We just assume this is going to be an ACPI version 2 record. 
    --  This may come back to bite us later, but ACPI 2 was released almost 20
    --  years ago...
    ---------------------------------------------------------------------------
    type RSDPRecord is
    record
        signature       : String (1..8);
        checksum        : Unsigned_8;
        OEMID           : String (1..6);
        revision        : Unsigned_8;
        RSDTAddress     : Unsigned_32;
        length          : Unsigned_32;
        XSDTAddress     : Unsigned_64;
        exChecksum      : Unsigned_8;
        reserved1       : Unsigned_8 := 0;
        reserved2       : Unsigned_8 := 0;
        reserved3       : Unsigned_8 := 0;
    end record;
    pragma Pack (RSDPRecord);

    ---------------------------------------------------------------------------
    -- ACPI 2.0 Root System Description Pointer (RSDP)
    -- @field length - Length of the ACPI table, in bytes, including header.
    -- @field XSDTAddress - 64-bit physical address of the XSDT
    -- @field exChecksum - checksum of the entire ACPI table, including both
    --  checksum fields.
    -- @field reserved1 - not used
    -- @field reserved2 - not used
    -- @field reserved3 - not used
    ---------------------------------------------------------------------------
    --type RSDPRecord2 is
    --record
    --    length          : Unsigned_32;
    --    XSDTAddress     : Unsigned_64;
    --    exChecksum      : Unsigned_8;
    --    reserved1       : Unsigned_8;
    --    reserved2       : Unsigned_8;
    --    reserved3       : Unsigned_8;
    --end record;
    --pragma Pack (RSDPRecord2);

    ---------------------------------------------------------------------------
    -- DescriptionHeader - for ACPI records that have an array of values
    --  after them, we need to get the length ahead of time so we can add the
    --  discriminant containing the array length later. We use this incomplete
    --  record to get the type and length so we know what to instantiate.
    ---------------------------------------------------------------------------
    type DescriptionHeader is
    record
        signature       : String (1..4);
        length          : Unsigned_32;
    end record with Pack, Size => 8*8;

    ---------------------------------------------------------------------------
    -- ACPI System Description Table (RSDT/XSDT). This is called the RSDT in 
    --  ACPI 1.0, but has been superseded by the XSDT. The only difference is
    --  the width of the pointers in the entries array at the end.
    --
    -- @field signature - Always "RSDT"
    -- @field length - length of array of physical memory pointers to other 
    --  ACPI system description tables
    -- @field revision - Always 1
    -- @field checksum - entire table must sum to 0.
    -- @field OEMID - OEM-supplied string
    -- @field OEMTableID - The table ID is the manufacturer model ID. This
    --  field must match the OEM Table ID in the FADT.
    -- @field recordPointers - Array of pointers (length addresses long) to
    --  additional ACPI records.
    -- @field OEMRevision - OEM revision of RSDT table for supplied OEM 
    --  Table ID.
    -- @field creatorID - Vendor ID of utility that created the table. For
    --  tables containing Definition Blocks, this is the ID for the ASL
    --  compiler.
    -- @field creatorRevision - Revision of utility that created the table.
    -- @field entries - array of 32-bit pointers (for RSDT) or 64-bit pointers
    --  (for XSDT) to other ACPI tables.
    ---------------------------------------------------------------------------
    type entryArray32 is array (Interfaces.C.int range <>) of Unsigned_32 with
        Convention => C;
    --pragma Pack (entryArray32);

    type entryArray64 is array (Interfaces.C.int range <>) of Unsigned_64 with
        Convention => C;
    --pragma Pack (entryArray64);

--    type RSDTRecord (numEntries : Interfaces.C.int := 0) is
--    record
--        signature       : String (1..4);
--        length          : Unsigned_32;
--        revision        : Unsigned_8;
--        checksum        : Unsigned_8;
--        OEMID           : String (1..6);
--        OEMTableID      : String (1..8);
--        OEMRevision     : Unsigned_32;
--        creatorID       : Unsigned_32;
--        creatorRevision : Unsigned_32;
--        entries         : entryArray32 (0 .. numEntries);
--    end record;
--    pragma Pack (RSDTRecord);

--    type RSDTRecordPtr is access all RSDTRecord;

--    type String4 is new String(1..4) with Pack, Size => 4*8;
--    type String6 is new String(1..6) with Pack, Size => 6*8;
--    type String8 is new String(1..8) with Pack, Size => 8*8;

    ---------------------------------------------------------------------------
    -- SDT Record Header is used for both the RSDT and XSDT records.
    -- Refer to ACPI Specification for information about fields.
    ---------------------------------------------------------------------------
    type SDTRecordHeader is
    record
        signature       : String (1..4);
        length          : Unsigned_32;
        revision        : Unsigned_8;
        checksum        : Unsigned_8;
        OEMID           : String (1..6);
        OEMTableID      : String (1..8);
        OEMRevision     : Unsigned_32;
        creatorID       : Unsigned_32;
        creatorRevision : Unsigned_32;
    end record with Size => 36*8;
    
    for SDTRecordHeader use
    record
        signature       at 0  range 0..31;
        length          at 4  range 0..31;
        revision        at 8  range 0..7;
        checksum        at 9  range 0..7;
        OEMID           at 10 range 0..47;
        OEMTableID      at 16 range 0..63;
        OEMRevision     at 24 range 0..31;
        creatorID       at 28 range 0..31;
        creatorRevision at 32 range 0..31;
    end record;

    ---------------------------------------------------------------------------
    -- RSDT Record Header
    -- @field header - See SDTRecordHeader
    -- @field entries - This is just the _first_ entry in an array whose length
    --  is given in the record header. These are 32-bit addresses, so if CuBit
    --  is being run on a 64-bit system that uses an RSDT, they will need to
    --  be converted to 64-bit addresses.
    ---------------------------------------------------------------------------
    type RSDTRecord is
    record
        header          : SDTRecordHeader;
        entries         : Unsigned_32;
    end record with Size => 40*8;

    for RSDTRecord use
    record
        header          at 0  range 0..287;
        entries         at 36 range 0..31;
    end record;

    ---------------------------------------------------------------------------
    -- XSDT Record Header
    -- @field header - See SDTRecordHeader
    -- @field entries - This is just the _first_ entry in an array whose length
    --  is given in the record header.
    ---------------------------------------------------------------------------
    type XSDTRecord is
    record
        header          : SDTRecordHeader;
        entries         : Unsigned_64;
    end record with Size => 44*8;

    for XSDTRecord use
    record
        header          at 0  range 0..287;
        entries         at 36 range 0..63;
    end record;

    ---------------------------------------------------------------------------
    -- ACPI Generic Address Structure and Friends
    ---------------------------------------------------------------------------
    -- ACPI address space ID
    subtype AddressSpaceID is Unsigned_8;
    ADDR_SYS_MEM             : constant AddressSpaceID := 0;
    ADDR_SYS_IO              : constant AddressSpaceID := 1;
    ADDR_PCI_CFG             : constant AddressSpaceID := 2;
    ADDR_EMBED_CTRLR         : constant AddressSpaceID := 3;
    ADDR_SMBUS               : constant AddressSpaceID := 4;
    ADDR_PCC                 : constant AddressSpaceID := 16#0A#;
    ADDR_FF_HARDWARE         : constant AddressSpaceID := 16#7F#;
    -- OEM-Defined values from 0xC0 to 0xFF

    subtype AccessSizeType is Unsigned_8;
    ACC_SIZE_UNDEFINED      : constant AccessSizeType := 0; -- legacy
    ACC_SIZE_BYTE           : constant AccessSizeType := 1;
    ACC_SIZE_WORD           : constant AccessSizeType := 2;
    ACC_SIZE_DWORD          : constant AccessSizeType := 3;
    ACC_SIZE_QWORD          : constant AccessSizeType := 4;

    type GenericAddressStructure is
    record
        addressSpace        : AddressSpaceID;
        regBitWidth         : Unsigned_8;       -- 0 when addressing structs
        regBitOffset        : Unsigned_8;       -- 0 when addressing structs
        accessSize          : AccessSizeType;
        address             : Unsigned_64;      -- processor-relative
    end record with Size => 12*8;

    for GenericAddressStructure use
    record
        addressSpace        at 0 range 0..7;
        regBitWidth         at 1 range 0..7;
        regBitOffset        at 2 range 0..7;
        accessSize          at 3 range 0..7;
        address             at 4 range 0..63;
    end record;

    ---------------------------------------------------------------------------
    -- Power management profiles used in the FADT
    ---------------------------------------------------------------------------
    subtype PowerManagementProfile is Unsigned_8 range 0..8;

    PM_PROFILE_UNSPECIFIED  : constant PowerManagementProfile := 0;
    PM_PROFILE_DESKTOP      : constant PowerManagementProfile := 1;
    PM_PROFILE_MOBILE       : constant PowerManagementProfile := 2;
    PM_PROFILE_WORKSTATION  : constant PowerManagementProfile := 3;
    PM_PROFILE_ENTERPRISE   : constant PowerManagementProfile := 4;
    PM_PROFILE_SOHO         : constant PowerManagementProfile := 5;
    PM_PROFILE_APPLIANCE    : constant PowerManagementProfile := 6;
    PM_PROFILE_PERFORMANCE  : constant PowerManagementProfile := 7;
    PM_PROFILE_TABLET       : constant PowerManagementProfile := 8;

    ---------------------------------------------------------------------------
    -- FADT - Fixed ACPI Description Table.
    ---------------------------------------------------------------------------
    type FADTRecord is
    record
        header              : SDTRecordHeader;
        firmwareControl     : Unsigned_32;  -- ignored if exFirmwareControl present
        dsdt                : Unsigned_32;  -- ignored if exDsdt present
        reserved1           : Unsigned_8;
        powerMgmtProfile    : PowerManagementProfile;
        sciInterrupt        : Unsigned_16;
        smiCommand          : Unsigned_32;
        acpiEnable          : Unsigned_8;
        acpiDisable         : Unsigned_8;
        S4BIOSReq           : Unsigned_8;
        pStateControl       : Unsigned_8;
        PM1AEventBlock      : Unsigned_32;
        PM1BEventBlock      : Unsigned_32;
        PM1AControlBlock    : Unsigned_32;
        PM1BControlBlock    : Unsigned_32;
        PM2ControlBlock     : Unsigned_32;
        PMTimerBlock        : Unsigned_32;
        GPE0Block           : Unsigned_32;
        GPE1Block           : Unsigned_32;
        PM1EventLength      : Unsigned_8;
        PM1ControlLength    : Unsigned_8;
        PM2ControlLength    : Unsigned_8;
        PMTimerLength       : Unsigned_8;
        GPE0BlockLength     : Unsigned_8;
        GPE1BlockLength     : Unsigned_8;
        GPE1Base            : Unsigned_8;
        cStateControl       : Unsigned_8;
        pLevel2Latency      : Unsigned_16;
        pLevel3Latency      : Unsigned_16;
        flushSize           : Unsigned_16;
        flushStride         : Unsigned_16;
        dutyOffset          : Unsigned_8;
        dutyWidth           : Unsigned_8;
        dayAlarm            : Unsigned_8;
        monthAlarm          : Unsigned_8;
        century             : Unsigned_8;   -- RTC index into RTC RAM if not 0
        intelBootArch       : Unsigned_16;  -- IA-PC boot architecture flags
        reserved2           : Unsigned_8;   -- always 0
        flags               : Unsigned_32;  -- fixed feature flags
        resetRegister       : GenericAddressStructure;
        resetValue          : Unsigned_8;
        armBootArch         : Unsigned_16;
        fadtMinorVersion    : Unsigned_8;
        exFirmwareControl   : Unsigned_64;
        exDsdt              : Unsigned_64;
        exPM1AEventBlock    : GenericAddressStructure;
        exPM1BEventBlock    : GenericAddressStructure;
        exPM1AControlBlock  : GenericAddressStructure;
        exPM1BControlBlock  : GenericAddressStructure;
        exPM2ControlBlock   : GenericAddressStructure;
        exPMTimerBlock      : GenericAddressStructure;
        exGPE0Block         : GenericAddressStructure;
        exGPE1Block         : GenericAddressStructure;

        -- ACPI 6 fields (not supported yet)
        --sleepControlReg     : GenericAddressStructure;
        --sleepStatusReg      : GenericAddressStructure;
        --hypervisorVendor    : Unsigned_64;
    end record with Size => 244*8;

    for FADTRecord use
    record
        header              at 0   range 0..287;
        firmwareControl     at 36  range 0..31;
        dsdt                at 40  range 0..31;
        reserved1           at 44  range 0..7;
        powerMgmtProfile    at 45  range 0..7;
        sciInterrupt        at 46  range 0..15;
        smiCommand          at 48  range 0..31;
        acpiEnable          at 52  range 0..7;
        acpiDisable         at 53  range 0..7;
        S4BIOSReq           at 54  range 0..7;
        pStateControl       at 55  range 0..7;
        PM1AEventBlock      at 56  range 0..31;
        PM1BEventBlock      at 60  range 0..31;
        PM1AControlBlock    at 64  range 0..31;
        PM1BControlBlock    at 68  range 0..31;
        PM2ControlBlock     at 72  range 0..31;
        PMTimerBlock        at 76  range 0..31;
        GPE0Block           at 80  range 0..31;
        GPE1Block           at 84  range 0..31;
        PM1EventLength      at 88  range 0..7;
        PM1ControlLength    at 89  range 0..7;
        PM2ControlLength    at 90  range 0..7;
        PMTimerLength       at 91  range 0..7;
        GPE0BlockLength     at 92  range 0..7;
        GPE1BlockLength     at 93  range 0..7;
        GPE1Base            at 94  range 0..7;
        cStateControl       at 95  range 0..7;
        pLevel2Latency      at 96  range 0..15;
        pLevel3Latency      at 98  range 0..15;
        flushSize           at 100 range 0..15;
        flushStride         at 102 range 0..15;
        dutyOffset          at 104 range 0..7;
        dutyWidth           at 105 range 0..7;
        dayAlarm            at 106 range 0..7;
        monthAlarm          at 107 range 0..7;
        century             at 108 range 0..7;
        intelBootArch       at 109 range 0..15;
        reserved2           at 111 range 0..7;
        flags               at 112 range 0..31;
        resetRegister       at 116 range 0..95;
        resetValue          at 128 range 0..7;
        armBootArch         at 129 range 0..15;
        fadtMinorVersion    at 131 range 0..7;
        exFirmwareControl   at 132 range 0..63;
        exDsdt              at 140 range 0..63;
        exPM1AEventBlock    at 148 range 0..95;
        exPM1BEventBlock    at 160 range 0..95;
        exPM1AControlBlock  at 172 range 0..95;
        exPM1BControlBlock  at 184 range 0..95;
        exPM2ControlBlock   at 196 range 0..95;
        exPMTimerBlock      at 208 range 0..95;
        exGPE0Block         at 220 range 0..95;
        exGPE1Block         at 232 range 0..95;

        -- ACPI 6 fields
        --sleepControlReg     at 244 range 0..95;
        --sleepStatusReg      at 256 range 0..95;
        --hypervisorVendor    at 268 range 0..63;
    end record;

    ---------------------------------------------------------------------------
    -- The MADT contains a header and then sub-structures, for each type of
    -- APIC in the system. The type of structure is enumerated here with the
    -- APICStructureType.
    ---------------------------------------------------------------------------
    subtype APICStructureType is Unsigned_8 range 0..8;
    
    LOCAL_APIC              : constant APICStructureType := 0;
    IO_APIC                 : constant APICStructureType := 1;
    INT_SRC_OVERRIDE        : constant APICStructureType := 2;
    NMI                     : constant APICStructureType := 3;
    LAPIC_NMI               : constant APICStructureType := 4;
    LAPIC_ADDR_OVERRIDE     : constant APICStructureType := 5;
    IO_SAPIC                : constant APICStructureType := 6;
    LOCAL_SAPIC             : constant APICStructureType := 7;
    PLATFORM_INTERRUPT      : constant APICStructureType := 8;

    -- Note: the ACPI spec says that LAPIC_DISABLED means the _processor_ is 
    -- unusable.
    subtype LocalAPICFlags is Unsigned_32 range 0..1;
    
    LAPIC_ENABLED           : constant LocalAPICFlags := 1;
    LAPIC_DISABLED          : constant LocalAPICFlags := 0;

    ---------------------------------------------------------------------------
    -- APICRecordHeader is common to each of the MADT's APIC entries. This is
    -- used when parsing the MADT entries to determine which type of APIC is
    -- being described and to use the appropriate record type for it.
    -- @field apicType - Describes the type of APIC described. 
    --  See APICStructureType.
    -- @field length - Length of the APIC record.
    ---------------------------------------------------------------------------
    type APICRecordHeader is
    record
        apicType            : APICStructureType;
        length              : Unsigned_8;
    end record with Size => 2*8;

    for APICRecordHeader use
    record
        apicType            at 0 range 0..7;
        length              at 1 range 0..7;
    end record;

    ---------------------------------------------------------------------------
    -- Processor Local APIC Structure
    -- @field header - see APICRecordHeader. apicType = 0, length = 8.
    -- @field processorID - "ProcessorID as listed in the ACPI processor
    --  declaration operator." (ACPI 3.0 specification)
    -- @field apicID - the processor's local APIC ID
    -- @field flags - (see LocalAPICFlags subtype.)
    ---------------------------------------------------------------------------
    type LAPICRecord is
    record
        header              : APICRecordHeader;
        processorID         : Unsigned_8;
        apicID              : Unsigned_8;
        flags               : LocalAPICFlags;
    end record with Size => 8*8;

    for LAPICRecord use
    record
        header              at 0 range 0..15;
        processorID         at 2 range 0..7;
        apicID              at 3 range 0..7;
        flags               at 4 range 0..31;
    end record;

    ---------------------------------------------------------------------------
    -- System I/O APIC Structure
    -- @field header - see APICRecordHeader. apicType = 1, length = 12
    -- @field apicID - the I/O APIC's ID
    -- @field reserved - always 0
    -- @field apicAddress - 32-bit physical address to access the I/O APIC.
    -- @field globalBase - global system interrupt number where this I/O APIC's
    --  interrupt inputs start.
    ---------------------------------------------------------------------------
    type IOAPICRecord is
    record
        header              : APICRecordHeader;
        apicID              : Unsigned_8;
        reserved            : Unsigned_8;
        apicAddress         : Unsigned_32;
        globalBase          : Unsigned_32;
    end record with Size => 12*8;

    for IOAPICRecord use
    record
        header              at 0 range 0..15;
        apicID              at 2 range 0..7;
        reserved            at 3 range 0..7;
        apicAddress         at 4 range 0..31;
        globalBase          at 8 range 0..31;
    end record;

    --------------------------------------------------------------------------
    -- MPS INTI flags, aka Intel Multiprocessor Specification 1.4 INTI flags.
    -- Define polarity and trigger mode of interrupts.
    --------------------------------------------------------------------------
    subtype MPSInterruptPolarity is Natural range 0..3;
    MPS_INTI_POLARITY_BUS           : constant MPSInterruptPolarity := 0;
    MPS_INTI_POLARITY_ACTIVE_HIGH   : constant MPSInterruptPolarity := 1;
    MPS_INTI_POLARITY_RESERVED      : constant MPSInterruptPolarity := 2;
    MPS_INTI_POLARITY_ACTIVE_LOW    : constant MPSInterruptPolarity := 3;

    subtype MPSInterruptTrigger is Natural range 0..3;
    MPS_INTI_TRIGGER_BUS            : constant MPSInterruptTrigger := 0;
    MPS_INTI_TRIGGER_EDGE           : constant MPSInterruptTrigger := 1;
    MPS_INTI_TRIGGER_RESERVED       : constant MPSInterruptTrigger := 2;
    MPS_INTI_TRIGGER_LEVEL          : constant MPSInterruptTrigger := 3;

    type MPSInterruptFlags is record
        polarity            : MPSInterruptPolarity;
        triggerMode         : MPSInterruptTrigger;
        reserved1           : Natural range 0..15;
        reserved2           : Unsigned_8;
    end record with Size => 16;

    for MPSInterruptFlags use
    record
        polarity            at 0 range 0..1;
        triggerMode         at 0 range 2..3;
        reserved1           at 0 range 4..7;
        reserved2           at 1 range 0..7;
    end record;

    ---------------------------------------------------------------------------
    -- Interrupt Source Overrides - describes differences between the 8259 PIC
    --  interrupt controllers and the APICs, with regards to numbering of
    --  interrupts. Used for ISA interrupt sources, which I don't think are
    --  going to be much of a factor for modern hardware.
    -- @field header - see APICRecordHeader. apicType = 2, length = 10.
    -- @field bus - always 0 (ISA)
    -- @field source - bus-relative interrupt source (IRQ)
    -- @field globalInterrupt - global system interrupt that the bus-relative
    --  IRQ will signal.
    -- @field flags - see MPSInterruptFlags.
    ---------------------------------------------------------------------------
    type InterruptOverrideRecord is
    record
        header              : APICRecordHeader;
        bus                 : Unsigned_8;
        source              : Unsigned_8;
        globalInterrupt     : Unsigned_32;
        flags               : MPSInterruptFlags;
    end record with Size => 10*8;

    for InterruptOverrideRecord use
    record
        header              at 0 range 0..15;
        bus                 at 2 range 0..7;
        source              at 3 range 0..7;
        globalInterrupt     at 4 range 0..31;
        flags               at 8 range 0..15;
    end record;

    ---------------------------------------------------------------------------
    -- NMIRecord
    -- Non-Maskable Interrupt (NMI) sources. Specify which I/O APIC interrupt
    --  inputs should be enabled as non-maskable. These are unusable for
    --  devices.
    -- @field header - see APICRecordHeader. apicType = 3, length = 8.
    -- @field flags - see MPSInterruptFlags.
    -- @field globalInterrupt - global system interrupt that this NMI will
    --  signal.
    ---------------------------------------------------------------------------
    type NMIRecord is
    record
        header              : APICRecordHeader;
        flags               : MPSInterruptFlags;
        globalInterrupt     : Unsigned_32;
    end record with Size => 8*8;

    for NMIRecord use
    record
        header              at 0 range 0..15;
        flags               at 2 range 0..15;
        globalInterrupt     at 4 range 0..31;
    end record;

    -- For LAPIC NMIs, this value means that the LAPIC NMI applies to all
    -- processors in the system.
    LAPIC_NMI_ALL_PROCESSORS    : constant Unsigned_8 := 16#FF#;

    --------------------------------------------------------------------------
    -- LAPICNMIRecord
    -- Local APIC Non-Maskable Interrupt Structure. This structure describes
    --  which LAPIC interrupt input that the NMI is connected to for each of
    --  the processors in the system if they exist. Each Local APIC NMI
    --  connection requires a separate LAPICNMI record.
    -- @field header - see APICRecordHeader. apicType = 4, length = 6.
    -- @field processorID - processor ID corresponding to the processor object
    -- @field flags - see MPSInterruptFlags.
    -- @field lapicLINT - Local APIC interrupt input LINTn to which NMI is
    --  connected.
    --------------------------------------------------------------------------
    type LAPICNMIRecord is
    record
        header              : APICRecordHeader;
        processorID         : Unsigned_8;
        flags               : MPSInterruptFlags;
        lapicLINT           : Unsigned_8;
    end record with Size => 6*8;

    for LAPICNMIRecord use
    record
        header              at 0 range 0..15;
        processorID         at 2 range 0..7;
        flags               at 3 range 0..15;
        lapicLINT           at 5 range 0..7;
    end record;

    ---------------------------------------------------------------------------
    -- LAPICAddressOverrideRecord
    -- Local APIC Address Override Structure. This optional structure supports
    -- 64-bit systems by providing an override of the physical address of the
    -- local APIC in the MADT table header. If defined, the OS will use this
    -- address for all local APICs and SAPICs, rather than the one in the
    -- MADT table header.
    -- @field header - see APICRecordHeader. apicType = 5, length = 12.
    ---------------------------------------------------------------------------
    type LAPICAddressOverrideRecord is
    record
        header              : APICRecordHeader;
        reserved            : Unsigned_16;
        lapicAddress        : Unsigned_64;
    end record with Size => 12*8;

    for LAPICAddressOverrideRecord use
    record
        header              at 0 range 0..15;
        reserved            at 2 range 0..15;
        lapicAddress        at 4 range 0..63;
    end record;

    -- Indicates whether the system uses 8259 PIC interrupt controllers in
    -- addition to the APICs. If so, then the PIC must be disabled prior to
    -- APIC use.
    subtype PC_ATCompatibility is Unsigned_32 range 0..1;
    NO_8259     : constant PC_ATCompatibility := 0;
    YES_8259    : constant PC_ATCompatibility := 1;

    ---------------------------------------------------------------------------
    -- Multiple APIC Description Table (MADT) and associated sub-structures.
    -- Signature is "APIC".
    -- @field header - see SDTRecordHeader. Signature = "APIC".
    -- @field flags - see PC_ATCompatibility.
    -- @field entries - the _first_ APICRecordHeader after the MADT record.
    --  we will use pointer arithmetic to find the rest, based on the length
    --  of this record.
    ---------------------------------------------------------------------------
    type MADTRecord is
    record
        header              : SDTRecordHeader;
        lapicAddress        : Unsigned_32;          -- each processor's LAPIC
        flags               : PC_ATCompatibility;
        entries             : APICRecordHeader;
    end record with Size => 46*8;

    for MADTRecord use
    record
        header              at 0 range 0..287;
        lapicAddress        at 36 range 0..31;
        flags               at 40 range 0..31;
        entries             at 44 range 0..15;
    end record;

    ---------------------------------------------------------------------------
    -- DSDT - Differentiated System Description Table
    --
    -- @field AMLCode - the _first_ byte of AML bytecode in this table.
    ---------------------------------------------------------------------------
    type DSDTRecord is
    record
        header              : SDTRecordHeader;
        AMLCode             : Unsigned_8;
    end record with Size => 37*8;

    for DSDTRecord use
    record
        header              at 0 range 0..287;
        AMLCode             at 36 range 0..7;
    end record;

    ---------------------------------------------------------------------------
    -- SSDT - Secondary System Description Table
    --
    -- @field AMLCode - the _first_ byte of AML bytecode in this table.
    ---------------------------------------------------------------------------
    type SSDTRecord is
    record
        header              : SDTRecordHeader;
        AMLCode             : Unsigned_8;
    end record with Size => 37*8;

    for SSDTRecord use
    record
        header              at 0 range 0..287;
        AMLCode             at 36 range 0..7;
    end record;

    ---------------------------------------------------------------------------
    -- HPETRecord - High-Precision Event Timer
    -- Intel HPET Specification 1.0, 2004.
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- Memory-Mapped Enhanced Configuration Space Base Address Allocation
    -- Structure. Defined by PCI Express specification.
    -- @field baseAddr - physical base address for the memory mapped config
    --  space associated with the PCI Segment Group
    -- @field PCISegmentGroup - Denotes the PCI Segment Group, if there are
    --  multiple, this field should match the _SEG object in the ACPI namespace
    -- @field startBus - Start PCI bus number decoded by the host bridge
    -- @field endBus - End PCI bus number decoded by the host bridge
    ---------------------------------------------------------------------------
    type MMAPConfigurationSpace is
    record
        baseAddr            : Unsigned_64;
        pciSegGroupNum      : Unsigned_16;
        startBusNum         : Unsigned_8;
        endBusNum           : Unsigned_8;
        reserved            : Unsigned_32;
    end record with Size => 16*8;

    for MMAPConfigurationSpace use
    record
        baseAddr            at 0 range 0..63;
        pciSegGroupNum      at 8 range 0..15;
        startBusNum         at 10 range 0..7;
        endBusNum           at 11 range 0..7;
        reserved            at 12 range 0..31;
    end record;

    -- Packed array of the above record
    type MMAPConfigSpaces is array (Unsigned_32 range <>) of MMAPConfigurationSpace
        with Convention => C;

    -- Base address of (the first, hopefully only) PCIe configuration space
    pcieConfig : MMAPConfigurationSpace;

    ---------------------------------------------------------------------------
    -- MCFG - PCI Express
    -- Specification at pcisig.com
    -- @field entries - configuration space base address allocation structures
    ---------------------------------------------------------------------------
    type MCFGRecord is
    record
        header              : SDTRecordHeader;
        reserved            : Unsigned_64;
        entries             : MMAPConfigurationSpace;
    end record with Size => (36+8+16)*8;

    for MCFGRecord use
    record
        header              at 0 range 0..287;
        reserved            at 36 range 0..63;
        entries             at 44 range 0..127;
    end record;

    ---------------------------------------------------------------------------
    -- SSDT - 
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- setup - read the ACPI tables and get machine information.
    --
    -- @return True if parsing ACPI tables was successful, False otherwise.
    ---------------------------------------------------------------------------
    function setup return Boolean;

    ---------------------------------------------------------------------------
    -- findRSDP - search through BIOS memory area for a pointer to the ACPI 
    --  System Description Table. Note that the ACPI specification says that it
    --  may be in the lower 1k of memory, pointed to by the value at address
    --  0x40E. Currently this function _does not_ search there.
    ---------------------------------------------------------------------------
    function findRSDP return System.Address;

    --procedure getRSDP(ret : out RSDPRecord; addr : out System.Address);

    --procedure findSDT(ret : out XSDTRecord);
end acpi;

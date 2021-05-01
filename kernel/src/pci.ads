-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary PCI Bus
--
-- @TODO: Would be nice to have a generalized iterator interface for finding
--  devices.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;

with x86;

package pci
    with SPARK_Mode => On
is
    PCI_CONFIG_ADDRESS      : constant x86.IOPort := 16#CF8#;
    PCI_CONFIG_DATA         : constant x86.IOPort := 16#CFC#;

    subtype PCIFunctionNum  is Unsigned_8 range 0..7;
    subtype PCISlotNum      is Unsigned_8 range 0..31;
    subtype PCIBusNum       is Unsigned_8;
    subtype PCIOffset       is Unsigned_8;

    ---------------------------------------------------------------------------
    -- PCI offsets within the configuration space for a particular value
    ---------------------------------------------------------------------------
    PCI_VENDOR_ID       : constant PCIOffset := 0;
    PCI_DEVICE_ID       : constant PCIOffset := 2;
    PCI_COMMAND         : constant PCIOffset := 4;
    PCI_STATUS          : constant PCIOffset := 6;
    PCI_REVISION_ID     : constant PCIOffset := 8;
    PCI_PROG_IF         : constant PCIOffset := 9;
    PCI_CLASSCODE       : constant PCIOffset := 10; -- class+subclass
    PCI_SUBCLASS        : constant PCIOffset := 10;
    PCI_CLASS           : constant PCIOffset := 11;
    PCI_CACHELINE_SIZE  : constant PCIOffset := 12;
    PCI_LATENCY_TIMER   : constant PCIOffset := 13;
    PCI_HEADER_TYPE     : constant PCIOffset := 14;
    PCI_BIST            : constant PCIOffset := 15;
    PCI_BASEADDR_0      : constant PCIOffset := 16;
    PCI_BASEADDR_1      : constant PCIOffset := 20;
    PCI_BASEADDR_2      : constant PCIOffset := 24;
    PCI_BASEADDR_3      : constant PCIOffset := 28;
    PCI_BASEADDR_4      : constant PCIOffset := 32;
    PCI_BASEADDR_5      : constant PCIOffset := 36;
    PCI_CARDBUS_PTR     : constant PCIOffset := 40;
    PCI_SUB_VENDOR_ID   : constant PCIOffset := 44;
    PCI_SUBSYSTEM_ID    : constant PCIOffset := 46;
    PCI_EXPROM_BASEADDR : constant PCIOffset := 48;
    PCI_CAPES_PTR       : constant PCIOffset := 52;
    PCI_INTERRUPT_LINE  : constant PCIOffset := 60;
    PCI_INTERRUPT_PIN   : constant PCIOffset := 61;
    PCI_MIN_GRANT       : constant PCIOffset := 62;
    PCI_MAX_LATENCY     : constant PCIOffset := 63;

    ---------------------------------------------------------------------------
    -- If bit 7 of the header type is 1, this device has multiple functions.
    ---------------------------------------------------------------------------
    subtype PCIHeaderType is Unsigned_8;
    PCI_MULTIPLE_FUNCS  : constant PCIHeaderType := 2#1000_0000#;
    GENERAL_DEVICE      : constant PCIHeaderType := 0;
    PCI_BRIDGE          : constant PCIHeaderType := 1;
    CARDBUS_BRIDGE      : constant PCIHeaderType := 2;

    ---------------------------------------------------------------------------
    -- PCI Subclass+Class
    ---------------------------------------------------------------------------
    subtype PCIClassCode is Unsigned_16;
    CLASS_UNKNOWN               : constant PCIClassCode := 16#0000#;
    CLASS_UNKNOWN_VGA           : constant PCIClassCode := 16#0001#;

    CLASS_STORAGE_SCSI          : constant PCIClassCode := 16#0100#;
    CLASS_STORAGE_IDE           : constant PCIClassCode := 16#0101#;
    CLASS_STORAGE_FLOPPY        : constant PCIClassCode := 16#0102#;
    CLASS_STORAGE_IPI           : constant PCIClassCode := 16#0103#;
    CLASS_STORAGE_RAID          : constant PCIClassCode := 16#0104#;
    CLASS_STORAGE_ATA           : constant PCIClassCode := 16#0105#;
    CLASS_STORAGE_SATA          : constant PCIClassCode := 16#0106#;
    CLASS_STORAGE_SAS           : constant PCIClassCode := 16#0107#;
    CLASS_STORAGE_NVME          : constant PCIClassCode := 16#0108#;
    CLASS_STORAGE_UNKNOWN       : constant PCIClassCode := 16#0180#;

    CLASS_NETWORK_ETHERNET      : constant PCIClassCode := 16#0200#;
    CLASS_NETWORK_UNKNOWN       : constant PCIClassCode := 16#0280#;

    CLASS_DISPLAY_VGA           : constant PCIClassCode := 16#0300#;
    CLASS_DISPLAY_XGA           : constant PCIClassCode := 16#0301#;
    CLASS_DISPLAY_3D            : constant PCIClassCode := 16#0302#;
    CLASS_DISPLAY_UNKNOWN       : constant PCIClassCode := 16#0380#;

    CLASS_MULTIMEDIA_VIDEO      : constant PCIClassCode := 16#0400#;
    CLASS_MULTIMEDIA_AUDIO      : constant PCIClassCode := 16#0401#;
    CLASS_MULTIMEDIA_PHONE      : constant PCIClassCode := 16#0402#;
    CLASS_MULTIMEDIA_AUDIODEV   : constant PCIClassCode := 16#0403#;
    CLASS_MULTIMEDIA_UNKNOWN    : constant PCIClassCode := 16#0480#;

    CLASS_MEMORY_RAM            : constant PCIClassCode := 16#0500#;
    CLASS_MEMORY_FLASH          : constant PCIClassCode := 16#0501#;
    CLASS_MEMORY_UNKNOWN        : constant PCIClassCode := 16#0580#;

    CLASS_BRIDGE_HOST           : constant PCIClassCode := 16#0600#;
    CLASS_BRIDGE_ISA            : constant PCIClassCode := 16#0601#;
    CLASS_BRIDGE_EISA           : constant PCIClassCode := 16#0602#;
    CLASS_BRIDGE_MICROCHANNEL   : constant PCIClassCode := 16#0603#;
    CLASS_BRIDGE_PCI            : constant PCIClassCode := 16#0604#;
    CLASS_BRIDGE_PCMCIA         : constant PCIClassCode := 16#0605#;
    CLASS_BRIDGE_NUBUS          : constant PCIClassCode := 16#0606#;
    CLASS_BRIDGE_CARDBUS        : constant PCIClassCode := 16#0607#;
    CLASS_BRIDGE_RACEWAY        : constant PCIClassCode := 16#0608#;
    CLASS_BRIDGE_TRANS_PCI2PCI  : constant PCIClassCode := 16#0609#;
    CLASS_BRIDGE_INFINIBAND2PCI : constant PCIClassCode := 16#060a#;
    CLASS_BRIDGE_UNKNOWN        : constant PCIClassCode := 16#0680#;

    CLASS_COMM_SERIAL           : constant PCIClassCode := 16#0700#;
    CLASS_COMM_PARALLEL         : constant PCIClassCode := 16#0701#;
    CLASS_COMM_MULTIPORT        : constant PCIClassCode := 16#0702#;
    CLASS_COMM_MODEM            : constant PCIClassCode := 16#0703#;
    CLASS_COMM_GPIB             : constant PCIClassCode := 16#0704#;
    CLASS_COMM_SMARTCARD        : constant PCIClassCode := 16#0705#;
    CLASS_COMM_UNKNOWN          : constant PCIClassCode := 16#0780#;

    CLASS_GENERAL_PIC           : constant PCIClassCode := 16#0800#;
    CLASS_GENERAL_DMA           : constant PCIClassCode := 16#0801#;
    CLASS_GENERAL_TIMER         : constant PCIClassCode := 16#0802#;
    CLASS_GENERAL_RTC           : constant PCIClassCode := 16#0803#;
    CLASS_GENERAL_PCI_HOTPLUG   : constant PCIClassCode := 16#0804#;
    CLASS_GENERAL_SDHOST        : constant PCIClassCode := 16#0805#;
    CLASS_GENERAL_IOMMU         : constant PCIClassCode := 16#0806#;
    CLASS_GENERAL_OTHER         : constant PCIClassCode := 16#0880#;

    CLASS_INPUT_KEYBOARD        : constant PCIClassCode := 16#0900#;
    CLASS_INPUT_PEN             : constant PCIClassCode := 16#0901#;
    CLASS_INPUT_MOUSE           : constant PCIClassCode := 16#0902#;
    CLASS_INPUT_SCANNER         : constant PCIClassCode := 16#0903#;
    CLASS_INPUT_GAMEPORT        : constant PCIClassCode := 16#0904#;
    CLASS_INPUT_UNKNOWN         : constant PCIClassCode := 16#0980#;

    CLASS_SERIAL_FIREWIRE       : constant PCIClassCode := 16#0C00#;
    CLASS_SERIAL_ACCESS         : constant PCIClassCode := 16#0C01#;
    CLASS_SERIAL_SSA            : constant PCIClassCode := 16#0C02#;
    CLASS_SERIAL_USB            : constant PCIClassCode := 16#0C03#;
    CLASS_SERIAL_FIBRECHANNEL   : constant PCIClassCode := 16#0C04#;
    CLASS_SERIAL_SMBUS          : constant PCIClassCode := 16#0C05#;
    CLASS_SERIAL_INFINIBAND     : constant PCIClassCode := 16#0C06#;
    CLASS_SERIAL_IPMI           : constant PCIClassCode := 16#0C07#;
    CLASS_SERIAL_SERCOS         : constant PCIClassCode := 16#0C08#;
    CLASS_SERIAL_CANBUS         : constant PCIClassCode := 16#0C09#;

    ---------------------------------------------------------------------------
    -- PCI Configuration Space
    ---------------------------------------------------------------------------
    type PCIDeviceHeader is
    record
        vendorID        : Unsigned_16;
        deviceID        : Unsigned_16;
        command         : Unsigned_16;
        status          : Unsigned_16;
        revisionID      : Unsigned_8;
        progInterface   : Unsigned_8;
        classCode       : PCIClassCode;      -- two fields, but we treat as one
        cacheLineSize   : Unsigned_8;
        latencyTimer    : Unsigned_8;
        headerType      : PCIHeaderType;
        builtInSelfTest : Unsigned_8;
        baseAddr0       : Unsigned_32;
        baseAddr1       : Unsigned_32;
        baseAddr2       : Unsigned_32;
        baseAddr3       : Unsigned_32;
        baseAddr4       : Unsigned_32;
        baseAddr5       : Unsigned_32;
        cardbusPtr      : Unsigned_32;
        subVendorID     : Unsigned_16;
        subSystemID     : Unsigned_16;
        romBaseAddr     : Unsigned_32;
        capesPtr        : Unsigned_8;
        reserved1       : Unsigned_24;
        reserved2       : Unsigned_32;
        interruptLine   : Unsigned_8;
        interruptPIN    : Unsigned_8;
        minGrant        : Unsigned_8;
        maxLatency      : Unsigned_8;
    end record with Size => 64*8;

    for PCIDeviceHeader use
    record
        vendorID        at 0  range 0..15;
        deviceID        at 0  range 16..31;
        command         at 4  range 0..15;
        status          at 4  range 16..31;
        revisionID      at 8  range 0..7;
        progInterface   at 8  range 8..15;
        classCode       at 8  range 16..31;
        cacheLineSize   at 12 range 0..7;
        latencyTimer    at 12 range 8..15;
        headerType      at 12 range 16..23;
        builtInSelfTest at 12 range 24..31;
        baseAddr0       at 16 range 0..31;
        baseAddr1       at 20 range 0..31;
        baseAddr2       at 24 range 0..31;
        baseAddr3       at 28 range 0..31;
        baseAddr4       at 32 range 0..31;
        baseAddr5       at 36 range 0..31;
        cardbusPtr      at 40 range 0..31;
        subVendorID     at 44 range 0..15;
        subSystemID     at 44 range 16..31;
        romBaseAddr     at 48 range 0..31;
        capesPtr        at 52 range 0..7;
        reserved1       at 52 range 8..31;
        reserved2       at 56 range 0..31;
        interruptLine   at 60 range 0..7;
        interruptPIN    at 60 range 8..15;
        minGrant        at 60 range 16..23;
        maxLatency      at 60 range 24..31;
    end record;

    ---------------------------------------------------------------------------
    -- PCIe Configuration Space
    ---------------------------------------------------------------------------
    -- type PCIeConfiguration is
    -- record
    --     pciConfig : PCIDeviceHeader;
    --     PM Cap
    --     NxtCap
    --     PM Capability
    --     PMCSR
    --     MSI Cap
    --     NxtCap
    --     MSI Control
    --     Message Address : 64
    --     Message Data
    --     Reserved
    --     PE Cap
    --     NxtCap
    --     PE Capability
    --     PCIe Device Capabilities
    --     Device Control
    --     Device Status
    --     PCIe Link Capabilities
    --     Link Control
    --     Link Status
    --     Reserved Legacy Config Space
    --     PCI Express Extended Capability - DSN
    --     Capability Version
    --     Next Cap
    --     PCIe Device Serial Number
    --     PCIe Device Serial Number
    --     Reserved Extended Config Space
    -- end record with Size => 4096 * 8;

    -- for PCIeConfiguration use
    -- record

    -- end record;

    ---------------------------------------------------------------------------
    -- readConfig32 - read a 32-bit value from the PCI configuration space
    -- of the device at specified bus, slot and function.
    -- @param offset - must be 32-bit aligned (bottom 2 bits = 0)
    ---------------------------------------------------------------------------
    function readConfig32 (bus    : in PCIBusNum; 
                           slot   : in PCISlotNum;
                           func   : in PCIFunctionNum;
                           offset : in Unsigned_8) return Unsigned_32 with
        Pre => (offset and 3) = 0;

    ---------------------------------------------------------------------------
    -- readConfig16 - read a 16-bit word from the PCI configuration space
    -- of the device at specified bus, slot and function.
    -- @param offset - must be 16-bit aligned (bottom bit = 0)
    ---------------------------------------------------------------------------
    function readConfig16 (bus    : in PCIBusNum;
                           slot   : in PCISlotNum;
                           func   : in PCIFunctionNum;
                           offset : in Unsigned_8) return Unsigned_16 with
        Pre => (offset and 1) = 0;
    
    ---------------------------------------------------------------------------
    -- readConfig8 - read a byte from the PCI configuration space of the
    -- device at specified bus, slot and function.
    ---------------------------------------------------------------------------
    function readConfig8 (bus    : in PCIBusNum;
                          slot   : in PCISlotNum;
                          func   : in PCIFunctionNum;
                          offset : in Unsigned_8) return Unsigned_8;

    ---------------------------------------------------------------------------
    -- enumerateDevices - dump the configuration space of all PCI devices
    -- found on this computer.
    ---------------------------------------------------------------------------
    procedure enumerateDevices;

    ---------------------------------------------------------------------------
    -- enumerateDevices
    -- dump the configuration space of all PCIe devices found on this computer
    -- using the enhanced (memory-mapped) PCI configuration mechanism.
    ---------------------------------------------------------------------------
    procedure enumerateDevicesPCIe (base : System.Address);

    ---------------------------------------------------------------------------
    -- getNumDevices - get the number of devices on this system with a
    -- particular PCI class+subclass
    -- @param class - class code to search for
    -- @return number of devices that match this code.
    ---------------------------------------------------------------------------
    function getNumDevices (class : in PCIClassCode) return Natural;

    ---------------------------------------------------------------------------
    -- findDevice - given a particular PCI class+subclass, return the bus,
    --  slot and function of that device. This returns only the first device it
    --  finds with a particular class code.
    ---------------------------------------------------------------------------
    procedure findDevice (findClass : in PCIClassCode; 
                          foundBus  : out PCIBusNum; 
                          foundSlot : out PCISlotNum;
                          foundFunc : out PCIFunctionNum);

    ---------------------------------------------------------------------------
    -- getDeviceConfiguration - given a PCI address, return the configuration
    --  space for that device.
    ---------------------------------------------------------------------------
    function getDeviceConfiguration (bus  : in PCIBusNum;
                                     slot : in PCISlotNum;
                                     func : in PCIFunctionNum) return PCIDeviceHeader;
end pci;
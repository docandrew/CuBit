-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- PCI Bus
-------------------------------------------------------------------------------
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with TextIO; use TextIO;
with Util;

package body pci with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- Read dword
    ---------------------------------------------------------------------------
    function readConfig32 (bus    : in PCIBusNum;
                           slot   : in PCISlotNum;
                           func   : in PCIFunctionNum;
                           offset : in Unsigned_8) return Unsigned_32
        with SPARK_Mode => On
    is
        ret : Unsigned_32;
        addr : constant Unsigned_32 := 
                            Shift_Left (Unsigned_32(bus), 16) or
                            Shift_Left (Unsigned_32(slot), 11) or
                            Shift_Left (Unsigned_32(func), 8) or
                            Unsigned_32(offset) or 16#8000_0000#;
    begin
        x86.out32 (PCI_CONFIG_ADDRESS, addr);
        x86.in32 (PCI_CONFIG_DATA, ret);

        return ret;
    end readConfig32;

    ---------------------------------------------------------------------------
    -- Read word
    ---------------------------------------------------------------------------
    function readConfig16 (bus    : in PCIBusNum;
                           slot   : in PCISlotNum;
                           func   : in PCIFunctionNum;
                           offset : in Unsigned_8) return Unsigned_16
        with SPARK_Mode => On
    is
        ret : Unsigned_16;
        addr : constant Unsigned_32 := 
                            Shift_Left (Unsigned_32(bus), 16) or
                            Shift_Left (Unsigned_32(slot), 11) or
                            Shift_Left (Unsigned_32(func), 8) or
                            Unsigned_32(offset) or 16#8000_0000#;
    begin
        x86.out32 (PCI_CONFIG_ADDRESS, addr);
        x86.in16 (PCI_CONFIG_DATA + x86.IOPort(offset and 3), ret);

        return ret;
    end readConfig16;

    ---------------------------------------------------------------------------
    -- Single byte - specify byte offset for the input port.
    ---------------------------------------------------------------------------
    function readConfig8 (bus    : in PCIBusNum;
                          slot   : in PCISlotNum;
                          func   : in PCIFunctionNum;
                          offset : in Unsigned_8) return Unsigned_8 with
        SPARK_Mode => On
    is
        ret : Unsigned_8;
        addr : constant Unsigned_32 := 
                            Shift_Left (Unsigned_32(bus), 16) or
                            Shift_Left (Unsigned_32(slot), 11) or
                            Shift_Left (Unsigned_32(func), 8) or
                            Unsigned_32(offset) or 16#8000_0000#;
    begin
        x86.out32 (PCI_CONFIG_ADDRESS, addr);
        x86.in8 (PCI_CONFIG_DATA + x86.IOPort(offset and 2), ret);

        return ret;
    end readConfig8;

    ---------------------------------------------------------------------------
    -- Dump the device's configuration space
    ---------------------------------------------------------------------------
    procedure dumpDevice (bus  : PCIBusNum;
                          slot : PCISlotNum;
                          func : PCIFunctionNum) with
        SPARK_Mode => On
    is
        val : Unsigned_32;
    begin
        print (bus); print(":"); print(slot); print(":"); 
        printd (Unsigned_32(func));
        println ("Configuration Space: ");

        for off in Unsigned_8'Range loop
            print ("Offset: "); printd (Unsigned_32(off));
            val := readConfig32 (bus, slot, func, off);

            print (" "); print (Util.getByte (val, 0));
            print (" "); print (Util.getByte (val, 1));
            print (" "); print (Util.getByte (val, 2));
            print (" "); println (Util.getByte (val, 3));
        end loop;
    end dumpDevice;

    ---------------------------------------------------------------------------
    -- Enumerate PCI devices
    ---------------------------------------------------------------------------
    procedure enumerateDevices with
        SPARK_Mode => On
    is
        revision : Unsigned_32;
        kind     : Unsigned_8;
        class    : PCIClassCode;
        deviceID : Unsigned_16;
        vendorID : Unsigned_16;
    begin
        println ("  Vendor   Device ID   Class  ");
        println ("------------------------------");

        BusLoop: for bus in PCIBusNum'Range loop
            SlotLoop: for slot in PCISlotNum'Range loop
                FuncLoop: for func in PCIFunctionNum'Range loop

                    revision := readConfig32 (bus, slot, func, PCI_REVISION_ID);

                    if revision /= 16#FFFF_FFFF# then
                        --dumpPCIDevice(bus, slot, func);
                        vendorID := 
                            readConfig16 (bus, slot, func, PCI_VENDOR_ID);
                        deviceID := 
                            readConfig16 (bus, slot, func, PCI_DEVICE_ID);
                        class    := 
                            PCIClassCode(readConfig16 (bus, slot, func, PCI_CLASSCODE));

                        print (" "); print (Unsigned_32(vendorID));
                        print (" "); print (Unsigned_32(deviceID));
                        print (" "); print (Unsigned_32(class));

                        println;

                        if func = 0 then
                            kind := readConfig8 (bus, slot, func, PCI_HEADER_TYPE);
                            
                            -- If bit 7 of Header Type is 0, then device does
                            -- not have multiple functions and there's no need
                            -- to check the others.
                            exit FuncLoop when (kind and PCI_MULTIPLE_FUNCS) = 0;
                        end if;
                    end if;
                
                end loop FuncLoop;
            end loop SlotLoop;
        end loop BusLoop;
    end enumerateDevices;

    ---------------------------------------------------------------------------
    -- getNumDevices
    ---------------------------------------------------------------------------
    function getNumDevices (class : in PCIClassCode) return Natural with
        SPARK_Mode => On
    is
        kind      : Unsigned_8;
        thisClass : PCIClassCode;
        found     : Natural := 0;
        revision  : Unsigned_32;
    begin
        BusLoop: for bus in PCIBusNum'Range loop
            SlotLoop: for slot in PCISlotNum'Range loop
                FuncLoop: for func in PCIFunctionNum'Range loop
                    revision := readConfig32 (bus, slot, func, PCI_REVISION_ID);

                    if revision /= 16#FFFF_FFFF# then
                        thisClass := 
                            PCIClassCode(readConfig16 (bus, slot, func, PCI_CLASSCODE));

                        if thisClass = class then
                            found := found + 1;
                        end if;

                        if func = 0 then
                            kind := readConfig8 (bus, slot, func, PCI_HEADER_TYPE);
                            
                            -- If bit 7 of Header Type is 0, then device does
                            -- not have multiple functions and there's no need
                            -- to check the others.
                            exit FuncLoop when (kind and PCI_MULTIPLE_FUNCS) = 0;
                        end if;
                    end if;
                
                end loop FuncLoop;
            end loop SlotLoop;
        end loop BusLoop;

        return found;
    end getNumDevices;

    ---------------------------------------------------------------------------
    -- findDevice
    ---------------------------------------------------------------------------
    procedure findDevice (findClass : in PCIClassCode;
                          foundBus  : out PCIBusNum; 
                          foundSlot : out PCISlotNum;
                          foundFunc : out PCIFunctionNum) with
        SPARK_Mode => On
    is
        kind      : Unsigned_8;
        thisClass : PCIClassCode;
        revision  : Unsigned_32;
    begin
        BusLoop: for bus in PCIBusNum'Range loop
            SlotLoop: for slot in PCISlotNum'Range loop
                FuncLoop: for func in PCIFunctionNum'Range loop
                    revision := readConfig32 (bus, slot, func, PCI_REVISION_ID);

                    if revision /= 16#FFFF_FFFF# then
                        thisClass    := 
                            PCIClassCode(readConfig16 (bus, slot, func, PCI_CLASSCODE));

                        if thisClass = findClass then
                            foundBus := bus;
                            foundSlot := slot;
                            foundFunc := func;
                            return;
                        end if;

                        if func = 0 then
                            kind := readConfig8 (bus, slot, func, PCI_HEADER_TYPE);
                            
                            -- If bit 7 of Header Type is 0, then device does
                            -- not have multiple functions and there's no need
                            -- to check the others.
                            exit FuncLoop when 
                                (kind and PCI_MULTIPLE_FUNCS) = 0;
                        end if;
                    end if;
                
                end loop FuncLoop;
            end loop SlotLoop;
        end loop BusLoop;

        foundBus := 0;
        foundSlot := 0;
        foundFunc := 0;
    end findDevice;

    ---------------------------------------------------------------------------
    -- getDeviceConfiguration
    ---------------------------------------------------------------------------
    function getDeviceConfiguration (bus  : in PCIBusNum;
                                     slot : in PCISlotNum;
                                     func : in PCIFunctionNum) return PCIDeviceHeader with
        SPARK_Mode => On
    is
        ret : PCIDeviceHeader;
    begin
        ret.vendorID        := readConfig16 (bus, slot, func, PCI_HEADER_TYPE);
        ret.deviceID        := readConfig16 (bus, slot, func, PCI_DEVICE_ID);
        ret.command         := readConfig16 (bus, slot, func, PCI_COMMAND);
        ret.status          := readConfig16 (bus, slot, func, PCI_STATUS);
        ret.revisionID      := readConfig8 (bus, slot, func,  PCI_REVISION_ID);
        ret.progInterface   := readConfig8 (bus, slot, func,  PCI_PROG_IF);
        ret.classCode       := PCIClassCode(readConfig16 (bus, slot, func, PCI_CLASSCODE));
        
        ret.cacheLineSize   := readConfig8 (bus, slot, func, PCI_CACHELINE_SIZE);
        ret.latencyTimer    := readConfig8 (bus, slot, func, PCI_LATENCY_TIMER);
        
        ret.headerType      := PCIHeaderType(readConfig8 (bus, slot, func, PCI_HEADER_TYPE));
        
        ret.builtInSelfTest := readConfig8 (bus, slot, func,  PCI_BIST);
        ret.baseAddr0       := readConfig32 (bus, slot, func, PCI_BASEADDR_0);
        ret.baseAddr1       := readConfig32 (bus, slot, func, PCI_BASEADDR_1);
        ret.baseAddr2       := readConfig32 (bus, slot, func, PCI_BASEADDR_2);
        ret.baseAddr3       := readConfig32 (bus, slot, func, PCI_BASEADDR_3);
        ret.baseAddr4       := readConfig32 (bus, slot, func, PCI_BASEADDR_4);
        ret.baseAddr5       := readConfig32 (bus, slot, func, PCI_BASEADDR_5);
        ret.cardbusPtr      := readConfig32 (bus, slot, func, PCI_CARDBUS_PTR);
        ret.subVendorID     := readConfig16 (bus, slot, func, PCI_SUB_VENDOR_ID);
        ret.subSystemID     := readConfig16 (bus, slot, func, PCI_SUBSYSTEM_ID);
        ret.romBaseAddr     := readConfig32 (bus, slot, func, PCI_EXPROM_BASEADDR);
        ret.capesPtr        := readConfig8 (bus, slot, func,  PCI_CAPES_PTR);
        ret.interruptLine   := readConfig8 (bus, slot, func,  PCI_INTERRUPT_LINE);
        ret.interruptPIN    := readConfig8 (bus, slot, func,  PCI_INTERRUPT_PIN);
        ret.minGrant        := readConfig8 (bus, slot, func,  PCI_MIN_GRANT);
        ret.maxLatency      := readConfig8 (bus, slot, func,  PCI_MAX_LATENCY);

        return ret;
    end getDeviceConfiguration;

    ---------------------------------------------------------------------------
    -- getPCIeConfig
    ---------------------------------------------------------------------------
    procedure getPCIeConfig (base : System.Address;
                             bus  : PCIBusNum;
                             slot : PCISlotNum;
                             func : PCIFunctionNum;
                             conf : out PCIDeviceHeader) with
        SPARK_Mode => On
    is
        devBase : System.Address := base + 
            ((Storage_Offset(bus) * 256) + (Storage_Offset(slot) * 8) + Storage_Offset(func)) * 4096;
    
        ret     : PCIDeviceHeader with Import, Address => devBase;
    begin
        conf := ret;
    end getPCIeConfig;

    ---------------------------------------------------------------------------
    -- enumerateDevicesPCIe
    ---------------------------------------------------------------------------
    procedure enumerateDevicesPCIe (base : System.Address) with
        SPARK_Mode => On
    is
        conf : PCIDeviceHeader;
    begin
        println ("  Vendor   Device ID   Class  ");
        println ("------------------------------");

        BusLoop: for bus in PCIBusNum'Range loop
            SlotLoop: for slot in PCISlotNum'Range loop
                FuncLoop: for func in PCIFunctionNum'Range loop
                    getPCIeConfig (base, bus, slot, func, conf);

                    if conf.vendorID /= 16#FFFF# then
                        print ("  "); print (conf.vendorID);
                        print ("    "); print (conf.deviceID);
                        print ("    "); print (conf.classCode);
                        println;
                    end if;
                end loop FuncLoop;
            end loop SlotLoop;
        end loop BusLoop;
    end enumerateDevicesPCIe;

    ---------------------------------------------------------------------------
    -- dumpConfiguration
    ---------------------------------------------------------------------------
    -- procedure dumpConfiguration (conf : in PCIDeviceHeader) with
    --     SPARK_Mode => On
    -- is
    -- begin

    -- end dumpConfiguration;
end pci;

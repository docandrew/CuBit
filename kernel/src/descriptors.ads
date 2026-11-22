-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- CuBitOS Descriptors
--
-- CuBit Descriptors play the same role as "File Descriptors" or "Handles."
-- They're used by user-space code to refer to in-kernel structures.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Config;
with Devices;

package Descriptors 
    with SPARK_Mode => On
is

    type DescriptorNum is new Natural range 0..Config.PER_PROCESS_DESCRIPTORS;

    ---------------------------------------------------------------------------
    -- Descriptor
    --  When a device, file, or other entity known to the kernel is opened or
    --  otherwise initialized or obtained by a process, one of these records is
    --  used to refer to it in user -> kernel interactions. Specifically, the
    --  descriptor number returned by a call like open() is just an index into
    --  the process' descriptor array.
    -- @field dev - Device referred to by this descriptor
    -- @field resourceAddr - For use by the device driver, perhaps an Inode
    --  address or other reference.
    -- @field resourceOffset - For use by the device driver, perhaps a file
    --  offset or other differentiator.
    -- @field refCount - Number of processes holding a descriptor to the same
    --  resource.
    ---------------------------------------------------------------------------
    type Descriptor is record
        dev             : Devices.DeviceID;
        resourceAddr    : Unsigned_64;
        resourceOffset  : Unsigned_64;
        refCount        : Natural;
    end record;

    STDIN   : constant DescriptorNum := 0;
    STDOUT  : constant DescriptorNum := 1;
    STDERR  : constant DescriptorNum := 2;

    type DescriptorArray is array (DescriptorNum) of Descriptor;

end Descriptors;

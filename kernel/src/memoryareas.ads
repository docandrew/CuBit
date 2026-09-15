-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Physical Memory Areas
-------------------------------------------------------------------------------

with Virtmem;
with Firmware_Frames;

package MemoryAreas is

    ---------------------------------------------------------------------------
    -- Multiboot and the ACPI specification define these types of memory areas.
    -- UEFI adds some others, but they map neatly to these categories.
    --
    -- VIDEO is one we add ourselves.
    ---------------------------------------------------------------------------
    type MemoryAreaType is (USABLE, RESERVED, ACPI, HIBERNATE, BAD, VIDEO, IO);

    ---------------------------------------------------------------------------
    -- MemoryArea describes a range of memory
    ---------------------------------------------------------------------------
    type MemoryArea is
    record
        kind        : MemoryAreaType := BAD;
        startAddr   : Virtmem.PhysAddress := 1;
        endAddr     : Virtmem.PhysAddress := 0;
    end record;

    type MemoryAreaArray is array (Natural range <>) of MemoryArea;

    -- Inclusive byte endpoints. This named empty entry is the only reversed
    -- interval accepted, used for absent/zero-length firmware entries.
    Empty_Area : constant MemoryArea := (others => <>);
    InvalidMemoryMap : exception;
    procedure Allocation_Map (Areas : MemoryAreaArray;
                              Result : out Firmware_Frames.Region_Array);

end MemoryAreas;

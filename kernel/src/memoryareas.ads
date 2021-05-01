-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Physical Memory Areas
-------------------------------------------------------------------------------

with Virtmem;

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
        kind        : MemoryAreaType;
        startAddr   : Virtmem.PhysAddress;
        endAddr     : Virtmem.PhysAddress;
    end record;

    type MemoryAreaArray is array (Natural range <>) of MemoryArea;

end MemoryAreas;
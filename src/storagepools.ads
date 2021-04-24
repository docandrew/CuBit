-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2021 Jon Andrew
--
-- General-purpose allocation package. This uses the SlabAllocator for small
-- allocations (< 4KiB) and BuddyAllocator directly for large allocations.
-------------------------------------------------------------------------------
with System.Storage_Elements;

with SlabAllocator;

package StoragePools is
    
    type StoragePool is limited null record;

    pragma Simple_Storage_Pool_Type (StoragePool);

    pool : StoragePool;

    ---------------------------------------------------------------------------
    -- SlabSizeArr
    -- Object allocations less than or equal to the sizes here will be serviced
    -- by the corresponding slab in SlabArray.
    -- @TODO Good opportunities for performance tuning here.
    ---------------------------------------------------------------------------
    type SlabSizeArr is array (Natural range <>) of Natural;
    slabSizes : SlabSizeArr := (16, 32, 64, 128, 256, 512, 1024, 2048, 4096);

    ---------------------------------------------------------------------------
    -- InitialCapacities
    -- How many objects of each size each slab should be set to allocate before
    -- needing expansion.
    ---------------------------------------------------------------------------
    type InitialCapacities is array (slabSizes'Range) of Natural;
    capacities : InitialCapacities := (256, 128, 64, 32, 16, 8, 8, 8, 4);

    ---------------------------------------------------------------------------
    -- SlabArray
    -- The array of actual slab types that will be used to service each size
    -- allocation (or smaller) laid out in slabSizes.
    ---------------------------------------------------------------------------
    type SlabArray is array (slabSizes'Range) of SlabAllocator.Slab;

    ---------------------------------------------------------------------------
    -- setup
    -- Configure the allocators used for the Storage Pools. See GNAT 
    -- Simple_Storage_Pools for details.
    ---------------------------------------------------------------------------
    procedure setup;

    ---------------------------------------------------------------------------
    -- Allocate
    -- This should not be called directly, but instead set StoragePools.pool
    -- as the 'Simple_Storage_Pool attribute for dynamically-allocated types and
    -- use new and Unchecked_Deallocation for managing the memory.
    ---------------------------------------------------------------------------
    procedure Allocate (pool   : in out StoragePool;
                        addr   : out System.Address;
                        size   : in System.Storage_Elements.Storage_Count;
                        ignore : in System.Storage_Elements.Storage_Count);

    ---------------------------------------------------------------------------
    -- Deallocate
    -- This should not be called directly. See StoragePools.Allocate.
    ---------------------------------------------------------------------------
    procedure Deallocate (pool   : in out StoragePool;
                          addr   : in System.Address;
                          size   : in System.Storage_Elements.Storage_Count;
                          ignore : in System.Storage_Elements.Storage_Count);

end StoragePools;
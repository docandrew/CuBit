-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2021 Jon Andrew
--
-- General-purpose allocation package. This uses the SlabAllocator for small
-- allocations (< 4KiB) and BuddyAllocator directly for large allocations.
-------------------------------------------------------------------------------
with System.Storage_Elements;
-- with System.Storage_Pool;

with SlabAllocator;

package StoragePools is

    StoragePoolException : exception;
    
    ---------------------------------------------------------------------------
    -- StoragePool
    -- Empty object used to satisfy GNAT's requirements for Simple_Storage_Pool
    --
    -- Objects of this type aren't actually used in the calls to Allocate or
    -- Deallocate, but one needs to be present in your package to set as the
    -- 'Simple_Storage_Pool attribute.
    ---------------------------------------------------------------------------
    type StoragePool is limited null record;
    pragma Simple_Storage_Pool_Type (StoragePool);

    ---------------------------------------------------------------------------
    -- SlabSizeArr
    -- Object allocations less than or equal to the sizes here will be serviced
    -- by the corresponding slab in SlabArray. (These are sizes in bytes)
    -- @TODO Good opportunities for performance tuning here.
    ---------------------------------------------------------------------------
    type SlabSizeArr is array (Natural range <>) of System.Storage_Elements.Storage_Count;
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
    slabs : SlabArray;

    ---------------------------------------------------------------------------
    -- setup
    -- Configure the allocators used for the Storage Pools. See GNAT 
    -- Simple_Storage_Pools for details.
    ---------------------------------------------------------------------------
    procedure setup;

    ---------------------------------------------------------------------------
    -- Allocate
    --
    -- This should not be called directly, but instead create a
    -- StoragePools.StoragePool variable in the package you'll be allocating
    -- in and then set that variable as the 'Simple_Storage_Pool attribute for
    -- dynamically-allocated types.
    --
    -- Use new and Unchecked_Deallocation for managing the memory.
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


    function Storage_Size (pool : in StoragePool)
        return System.Storage_Elements.Storage_Count;

end StoragePools;

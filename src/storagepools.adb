-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2021 Jon Andrew
--
-- General-purpose Allocation Package
-------------------------------------------------------------------------------
with BuddyAllocator;

package body StoragePools is

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup is
    begin
        for i in slabs'Range loop
            SlabAllocator.setup (pool     => slabs(i);
                                 objSize  => slabSizes(i);
                                 capacity => capacities(i));
        end loop;
    end setup;

    ---------------------------------------------------------------------------
    -- allocSmall
    ---------------------------------------------------------------------------
    procedure allocSmall (addr   : out System.Address;
                          bytes  : in System.Storage_Elements.Storage_Count;
                          ignore : in System.Storage_Elements.Storage_Count) is
    begin
        -- What slab does this belong in?
        for i in slabSizes'Range loop
            if bytes <= slabSizes(i) then
                -- @NOTE size and alignment are ignored
                SlabAllocator.Allocate (slabs(i), addr, bytes, alignment);
                return;
            end if;
        end loop;

        raise StoragePoolException with "No pool found to alloc small object";
    end allocSmall;

    ---------------------------------------------------------------------------
    -- freeSmall
    ---------------------------------------------------------------------------
    procedure freeSmall (addr   : out System.Address;
                         bytes  : in System.Storage_Elements.Storage_Count;
                         ignore : in System.Storage_Elements.Storage_Count) is
    begin
        for i in slabSizes'Range loop
            if bytes <= slabSizes(i) then
                SlabAllocator.Deallocate (slabs(i), addr, bytes, alignment) ;
            end if;
        end loop;

        raise StoragePoolException with "No pool found to free small object";
    end freeSmall;

    ---------------------------------------------------------------------------
    -- allocBig
    ---------------------------------------------------------------------------
    procedure allocBig (addr   : out System.Address;
                        bytes  : in System.Storage_Elements.Storage_Count;
                        ignore : in System.Storage_Elements.Storage_Count) is
    begin
        BuddyAllocator.alloc (BuddyAllocator.getOrder(bytes), addr);
    end allocBig;

    ---------------------------------------------------------------------------
    -- freeBig
    ---------------------------------------------------------------------------
    procedure freeBig (addr   : out System.Address;
                       bytes  : in System.Storage_Elements.Storage_Count;
                       ignore : in System.Storage_Elements.Storage_Count) is
    begin
        BuddyAllocator.free (BuddyAllocator.getOrder(bytes), addr);
    end freeBig;

    ---------------------------------------------------------------------------
    -- Allocate
    ---------------------------------------------------------------------------
    procedure Allocate (pool   : in out StoragePool;
                        addr   : out System.Address;
                        size   : in System.Storage_Elements.Storage_Count;
                        ignore : in System.Storage_Elements.Storage_Count) is
    begin
        -- @NOTE that we're passed the size in bits. We'll pass bytes to the
        -- other functions.
        if (size / 8) < slabSizes(slabSizes'Last) then
            allocSmall (addr, size / 8, alignment);
        else
            allocBig (addr, size / 8, alignment);
        end if;
    end Allocate;

    ---------------------------------------------------------------------------
    -- Deallocate
    ---------------------------------------------------------------------------
    procedure Deallocate (pool   : in out StoragePool;
                          addr   : in System.Address;
                          size   : in System.Storage_Elements.Storage_Count;
                          ignore : in System.Storage_Elements.Storage_Count) is
    begin
        if (size / 8) < slabSizes(slabSizes'Last) then
            freeSmall (addr, size / 8, ignore);
        else
            freeBig (addr, size / 8, ignore);
        end if;
    end Deallocate;

    ---------------------------------------------------------------------------
    -- Storage_Size
    ---------------------------------------------------------------------------
    -- function Storage_Size (pool : in StoragePool)
    --     return System.Storage_Elements.Storage_Count with
    --     SPARK_Mode => On
    -- is
    --     package SSE renames System.Storage_Elements;
    -- begin
    --     if not pool.initialized then
    --         raise NotInitializedException with "Slab not initialized with call to setup";
    --     end if;
        
    --     -- Need to do some more work here - for small objects need to determine
    --     -- the difference between the object size and the largest object size
    --     -- in the slab. For larger objects, can just do BuddyAllocator.getBlockSize (getOrder (objSize))
    --     return 0;
    -- end Storage_Size;

end StoragePools;
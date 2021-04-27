-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2021 Jon Andrew
--
-- General-purpose Allocation Package
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with BuddyAllocator;
with TextIO; use TextIO;

package body StoragePools is

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup is
    begin
        for i in slabs'Range loop
            SlabAllocator.setup (pool     => slabs(i),
                                 objSize  => slabSizes(i) * 8,
                                 capacity => capacities(i));
        end loop;
    end setup;

    ---------------------------------------------------------------------------
    -- allocSmall
    ---------------------------------------------------------------------------
    procedure allocSmall (addr   : out System.Address;
                          bytes  : in System.Storage_Elements.Storage_Count) is
    begin
        -- What slab does this belong in?
        for i in slabSizes'Range loop
            if bytes <= slabSizes(i) then
                -- @NOTE size is ignored by the SlabAllocator, and we ignore alignment.
                SlabAllocator.Allocate (slabs(i), addr);
                return;
            end if;
        end loop;

        raise StoragePoolException with "No pool found to alloc small object";
    end allocSmall;

    ---------------------------------------------------------------------------
    -- freeSmall
    ---------------------------------------------------------------------------
    procedure freeSmall (addr   : in System.Address;
                         bytes  : in System.Storage_Elements.Storage_Count) is
    begin
        for i in slabSizes'Range loop
            if bytes <= slabSizes(i) then
                -- @NOTE size is ignored by the SlabAllocator, and we ignore alignment.
                SlabAllocator.Deallocate (slabs(i), addr);
            end if;
        end loop;

        raise StoragePoolException with "No pool found to free small object";
    end freeSmall;

    ---------------------------------------------------------------------------
    -- allocBig
    ---------------------------------------------------------------------------
    procedure allocBig (addr   : out System.Address;
                        bytes  : in System.Storage_Elements.Storage_Count) is
    begin
        BuddyAllocator.alloc (BuddyAllocator.getOrder(bytes), addr);
    end allocBig;

    ---------------------------------------------------------------------------
    -- freeBig
    ---------------------------------------------------------------------------
    procedure freeBig (addr   : in System.Address;
                       bytes  : in System.Storage_Elements.Storage_Count) is
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
        if size < slabSizes(slabSizes'Last) then
            allocSmall (addr, size);
        else
            allocBig (addr, size);
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
        if size < slabSizes(slabSizes'Last) then
            freeSmall (addr, size);
        else
            freeBig (addr, size);
        end if;
    end Deallocate;

    ---------------------------------------------------------------------------
    -- Storage_Size
    ---------------------------------------------------------------------------
    function Storage_Size (pool : in StoragePool)
        return System.Storage_Elements.Storage_Count
    is
        -- package SSE renames System.Storage_Elements;
    begin
        -- if not pool.initialized then
        --     raise NotInitializedException with "Slab not initialized with call to setup";
        -- end if;
        
        -- Need to do some more work here - for small objects need to determine
        -- the difference between the object size and the largest object size
        -- in the slab. For larger objects, can just do BuddyAllocator.getBlockSize (getOrder (objSize))
        -- However, since we share the pool across object sizes, it's not clear
        -- what object is being referred to when this object is called.
        return 0;
    end Storage_Size;

end StoragePools;

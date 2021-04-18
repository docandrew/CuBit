-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- Slab Allocator for fast allocation of fixed-size objects
-------------------------------------------------------------------------------
--with System.Address_To_Access_Conversions;

with Util;

package body SlabAllocator 
    with SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup 
        (pool           : in out Slab;
         objSize        : in Natural;
         blockOrder     : in BuddyAllocator.Order := 0;
         alignment      : in System.Storage_Elements.Storage_Count := 8)
    with
        SPARK_Mode => Off
    is
        package SSE renames System.Storage_Elements;

        function max is new Util.max(Integer);

        minSize : SSE.Storage_Count;

        blockSize : constant SSE.Storage_Count :=
            SSE.Storage_Count(BuddyAllocator.blockSize(blockOrder));

    begin

        pool.blockOrder := blockOrder;
        pool.alignment := alignment;

        -- The object size needs to be at least as big as the node
        minSize := SSE.Storage_Count(max(FreeNode'Size, objSize));

        if minSize < alignment and alignment /= 0 then
            raise BadAlignmentException with "Slab alignment must be bigger than object size";
        end if;

        if alignment = 0 then
            pool.paddedSize := (minSize / 8);
        else
            pool.paddedSize := (minSize / 8) + ((minSize / 8) mod alignment);
        end if;

        -- Allocate a new block
        BuddyAllocator.alloc(pool.blockOrder, pool.blockAddr);

        if pool.blockAddr = 0 then
            raise OutOfMemoryException with "Slab could not allocate enough physical memory";
        end if;

        -- Link the list head to itself to start
        pool.freeList.prev := pool.freeList'Address;
        pool.freeList.next := pool.freeList'Address;
        
        pool.capacity := Integer(blockSize / pool.paddedSize);

        pool.initialized := True;

        -- Create freeNodes at each spot we'd like to allocate an object from
        for index in SSE.Storage_Offset(0)..SSE.Storage_Offset(pool.capacity-1)
        loop
            Deallocate( pool, 
                        To_Address( Virtmem.P2V(pool.blockAddr + 
                                    Integer_Address(index * pool.paddedSize))),
                        0, 0);
        end loop;

    end setup;


    ---------------------------------------------------------------------------
    -- Teardown
    ---------------------------------------------------------------------------
    procedure teardown(pool : in out Slab) with
        SPARK_Mode => On
    is
    begin
        BuddyAllocator.free(pool.blockOrder, pool.blockAddr);
        pool.capacity := 0;
        pool.blockAddr := Virtmem.PhysAddress(0);
    end teardown;

    ---------------------------------------------------------------------------
    -- Allocate
    ---------------------------------------------------------------------------
    procedure Allocate
        (pool       : in out Slab;
         addr       : out System.Address;
         ignore_1   : in System.Storage_Elements.Storage_Count;
         ignore_2   : in System.Storage_Elements.Storage_Count)
    with
        SPARK_Mode => On
    is
        --package ToObj is new System.Address_To_Access_Conversions(T);

        -- This won't be a node for much longer, since we are taking its
        -- underlying memory for the returned object.
        oldNode : aliased FreeNode
            with Import, Address => pool.freeList.next;
        
    begin
        if not pool.initialized then
            raise NotInitializedException with "Slab not initialized with call to setup";
        end if;

        Spinlocks.enterCriticalSection(pool.mutex);

        if pool.numFree = 0 then
            raise OutOfMemoryException with "Slab is empty";
        end if;

        -- out param - first free node in the list
        addr := pool.freeList.next;

        linkNext: declare
            nextBlock : aliased FreeNode
                with Import, Address => oldNode.next;
        begin
            -- fwd link to next block in list
            pool.freeList.next := oldNode.next;

            -- link next block back to head
            nextBlock.prev := oldNode.prev;
        end linkNext;

        pool.numFree := pool.numFree - 1;

        Spinlocks.exitCriticalSection(pool.mutex);
    end Allocate;


    ---------------------------------------------------------------------------
    -- free - inserts object at front of free list
    ---------------------------------------------------------------------------
    procedure Deallocate
        (pool       : in out Slab;
         addr       : in System.Address;
         ignore_1   : in System.Storage_Elements.Storage_Count;
         ignore_2   : in System.Storage_Elements.Storage_Count)
    with
        SPARK_Mode => On
    is
        newNode : aliased FreeNode with
            Import, Address => addr;

        nextNode : aliased FreeNode with
            Import, Address => pool.freeList.next;
    begin

        if not pool.initialized then
            raise NotInitializedException with "Slab not initialized with call to setup";
        end if;

        Spinlocks.enterCriticalSection(pool.mutex);

        -- point us back to list head and fwd to next block in line
        newNode.prev := nextNode.prev;
        newNode.next := pool.freeList.next;

        -- point list head fwd to us
        pool.freeList.next := addr;

        -- point next node in line back to us
        nextNode.prev := addr;

        -- increase free count
        pool.numFree := pool.numFree + 1;

        Spinlocks.exitCriticalSection(pool.mutex);
    end Deallocate;


    ---------------------------------------------------------------------------
    -- Storage_Size
    ---------------------------------------------------------------------------
    function Storage_Size (pool : in Slab)
        return System.Storage_Elements.Storage_Count with
        SPARK_Mode => On
    is
        package SSE renames System.Storage_Elements;
    begin
        if not pool.initialized then
            raise NotInitializedException with "Slab not initialized with call to setup";
        end if;
        
        return SSE.Storage_Count(pool.capacity) * pool.paddedSize;
    end Storage_Size;

end SlabAllocator;
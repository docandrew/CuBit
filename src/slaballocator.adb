-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- Slab Allocator for fast allocation of fixed-size objects
-------------------------------------------------------------------------------
--with System.Address_To_Access_Conversions;
with Interfaces; use Interfaces;
with TextIO; use TextIO;

package body SlabAllocator 
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- addStorage
    -- Take an underlying block of physical memory, add it to our list of
    -- blocks and create free nodes throughout that block, expanding our
    -- available storage.
    ---------------------------------------------------------------------------
    procedure addStorage (pool : in out Slab) is
        blockAddr    : Virtmem.PhysAddress;
        objsPerBlock : constant Natural := Natural(BuddyAllocator.blockSize(pool.blockOrder)) / pool.paddedSize;
    begin
        -- Allocate a new block
        BuddyAllocator.alloc (pool.blockOrder, blockAddr);

        if blockAddr = 0 then
            raise OutOfMemoryException with "Slab could not allocate additional physical memory";
        end if;

        pool.blocks(pool.numBlocks) := blockAddr;
        pool.numBlocks := pool.numBlocks + 1;

        -- Create freeNodes at each spot we'd like to allocate an object from
        for index in 0..objsPerBlock-1
        loop
            Deallocate (pool,
                        To_Address( Virtmem.P2V(blockAddr + 
                                    Integer_Address(index * pool.paddedSize))),
                        0, 0);
        end loop;
    end addStorage;

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup
        (pool      : in out Slab;
         objSize   : in Natural;
         capacity  : in Natural;
         alignment : in Natural := 8)
    with
        SPARK_Mode => Off
    is
    begin

        if objSize < FreeNode'Size then
            pool.alignment := (FreeNode'Size / 8);
        else
            pool.alignment := alignment;
        end if;

        if pool.alignment = 0 then
            pool.paddedSize := (objSize / 8);
        else
            pool.paddedSize := (objSize / 8) + ((objSize / 8) mod pool.alignment);
        end if;
        
        pool.objSize    := objSize;
        pool.blockOrder := BuddyAllocator.getOrder (Unsigned_64(pool.paddedSize * capacity));
        print ("SlabAllocator.setup: using block order "); println (Integer(pool.blockOrder));

        -- Link the list head to itself to start
        pool.freeList.prev := pool.freeList'Address;
        pool.freeList.next := pool.freeList'Address;

        addStorage (pool);

        pool.initialized := True;

    end setup;


    ---------------------------------------------------------------------------
    -- Teardown
    ---------------------------------------------------------------------------
    procedure teardown (pool : in out Slab) with
        SPARK_Mode => On
    is
    begin
        for b in 1..pool.numBlocks loop
            BuddyAllocator.free (pool.blockOrder, pool.blocks(b));
            pool.blocks(b) := 0;
        end loop;

        pool.initialized := False;
        pool.numFree     := 0;
        pool.numBlocks   := 0;
    end teardown;

    ---------------------------------------------------------------------------
    -- hasFree
    ---------------------------------------------------------------------------
    function hasFree (pool : in out Slab) return Boolean is
    begin
        if pool.numFree = 0 and pool.numBlocks = Config.MAX_SLAB_EXPAND_TIMES then
            return False;
        end if;

        return True;
    end hasFree;

    ---------------------------------------------------------------------------
    -- Allocate
    ---------------------------------------------------------------------------
    procedure Allocate
        (pool     : in out Slab;
         addr     : out System.Address;
         ignore_1 : in System.Storage_Elements.Storage_Count;
         ignore_2 : in System.Storage_Elements.Storage_Count)
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

        Spinlocks.enterCriticalSection (pool.mutex);

        if not hasFree (pool) then
            raise OutOfMemoryException with "Slab is empty";
        elsif pool.numFree = 0 then
            addStorage (pool);
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

        Spinlocks.exitCriticalSection (pool.mutex);
    end Allocate;


    ---------------------------------------------------------------------------
    -- Deallocate - inserts object at front of free list
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

        Spinlocks.enterCriticalSection (pool.mutex);

        -- point us back to list head and fwd to next block in line
        newNode.prev := nextNode.prev;
        newNode.next := pool.freeList.next;

        -- point list head fwd to us
        pool.freeList.next := addr;

        -- point next node in line back to us
        nextNode.prev := addr;

        -- increase free count
        pool.numFree := pool.numFree + 1;

        Spinlocks.exitCriticalSection (pool.mutex);
    end Deallocate;


    ---------------------------------------------------------------------------
    -- Storage_Size - used to return the 'Storage_Size attribute of an access
    --  type stored in this pool.
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
        
        return SSE.Storage_Count(pool.objSize);
    end Storage_Size;

end SlabAllocator;
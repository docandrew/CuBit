-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Slab Allocator for fast allocation of fixed-size objects
--
-- @description We provide this instead of the standard Ada storage pool
-- mechanism because of issues with finalization, required for Ada storage
-- pools. This is set up to mimic Ada storage pools, but using GNAT's
-- Simple_Storage_Pool functionality.
--
-- The slab is built using a circular doubly-linked free list structure within
-- the backing memory itself. Since we use a separate slab allocator for each
-- size of object, we needn't worry about first-fit, best-fit, or any of those
-- problems with traditional free-list allocators. Allocations and
-- deallocations are O(1).
-- 
-- The object size and alignment for each object this allocator holds is
-- specified at setup time. The first object in the slab will be aligned at
-- the BuddyAllocator order-aligned block (4k-8k-16k, etc.), subsequent objects
-- in the slab will be aligned at the starting address + object size + whatever
-- alignment is given.
--
--
--            |               |  start addr                                   |
--  Free      |<- start addr  |<-+size (aligned)               end of block ->|
--  List      |---------------|--------------|---------------|----------------+
--            |                                                               |
-- +------+   +------+        +------+       +-----------+   +------+         |
-- | next |==>| next |=======>| next |=======|.Allocated |==>| next |========>|
-- | prev |<==| prev |<=======| prev |<======|.Object....|===| prev |<========|
-- +------+   +------+        +------+       |...........|   +------+         |
--            |                              |...........|                    |
--            |                              +-----------+                    |
--            |                                                               |
--            |           Underlying block from physical allocator            |
--            +---------------------------------------------------------------+
--
-- To use this package, declare a SlabAllocator.Slab, and set your
-- access type to use that slab as its 'Simple_Storage_Pool. From there, you
-- must call setup on the slab, and then you can use new and free as you
-- would in a normal userspace Ada program.
--
-- For example:
--
--  objSlab : SlabAllocator.Slab;
--
--  type myObjPtr is access myObject;
--  for myObjPtr'Simple_Storage_Pool use objSlab;
--
--  procedure free is new Ada.Unchecked_Deallocation(myObject, myObjPtr);
--  SlabAllocator.setup(objSlab, myObject'Size, alignment => 64);
-- 
-- CAUTION: The alignment for all objects allocated by this allocator must be
--  specified in the setup procedure. Do not specify the 'Alignment attribute
--  for objects or types that use this allocator. It will be ignored.
--
-- CAUTION: Make sure you call setup before using new or
-- Ada.Unchecked_Deallocate on an object with this slab.
--
-- NOTE: Objects smaller than 16 bytes in this slab will be padded to 16 bytes.
--
-- NOTE: Functions in this package are not intended to be called directly, but
--  instead, use the "new" operator and Ada.Unchecked_Deallocation.
-------------------------------------------------------------------------------
with System;
with System.Storage_Elements; use System.Storage_Elements;

with BuddyAllocator;
with Spinlock;
with Virtmem;

package SlabAllocator with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- @field next - pointer to next free object, or to the list head if empty
    -- @field prev - pointer to previous free object, or to list head if empty
    ---------------------------------------------------------------------------
    type FreeNode is
    record
        next : System.Address;
        prev : System.Address;
    end record with Size => 16 * 8;

    for FreeNode use
    record
        next at 0 range 0..63;
        prev at 8 range 0..63;
    end record;

    ---------------------------------------------------------------------------
    -- Slab is the "storage pool" type. We don't have finalization or some of
    --  the other Ada features needed for the official storage pools, but GNAT
    --  simple storage pools can be made to work with our setup here.
    --
    -- @field freeList - node for the head of the linked free list
    --
    -- @field numFree - number of free objects in this slab
    --
    -- @field capacity - number of objects this slab can hold before any
    --  allocations take place
    --
    -- @field blockOrder - order of underlying memory block 
    --  (See BuddyAllocator)
    --
    -- @field blockAddr - _physical_ base address of underlying memory block
    --
    -- @field mutex - spinlock to ensure synchronized (de)allocations
    --
    -- @field alignment - alignment for allocated objects in this slab. Use
    --  zero if the slab should be packed, but in no case should this be less
    --  than (T'Size/8) or (FreeNode'Size/8), whichever is smaller.
    --
    -- @field paddedSize - number of _bytes_ each object takes up in this slab
    --  once alignment and minimum size of a FreeNode is taken into account.
    --
    -- @field initialized - True if setup; has been called on this slab, False
    --  otherwise.
    ---------------------------------------------------------------------------
    type Slab is limited record
        freeList    : FreeNode;

        numFree     : Integer := 0;
        capacity    : Integer := 0;

        blockOrder  : BuddyAllocator.Order;
        blockAddr   : Virtmem.PhysAddress;
        
        mutex       : aliased Spinlock.Spinlock;
        
        alignment   : System.Storage_Elements.Storage_Count;
        paddedSize  : System.Storage_Elements.Storage_Count;
        
        initialized : Boolean := False;
    end record;

    -- GNAT-specific pragma
    pragma Simple_Storage_Pool_Type(Slab);

    OutOfMemoryException : exception;
    BadFreeAddressException : exception;
    BadAlignmentException : exception;
    NotInitializedException : exception;

    ---------------------------------------------------------------------------
    -- setup - allocate underlying physical memory, initialize the free lists
    -- with the desired object size and alignment.
    --
    -- @param pool - Slab (Ada Storage Pool) to setup.
    -- @param objSize - the size _in bits_ of the object (so Obj'Size can be
    --  used)
    -- @param blockOrder - size of underlying physical memory to allocate, in
    --  terms of FRAME_SIZE * 2^(blockOrder)
    -- @param alignment - alignment for stored objects within the slab.
    --
    -- @exception OutOfMemoryException raised when the underlying physical
    --  memory allocator cannot allocate a block large enough for this slab.
    -- @exception BadAlignmentException raised when alignment is less than
    --  the object size (T'Size/8) or (FreeNode'Size/8), whichever is smaller.
    ---------------------------------------------------------------------------
    procedure setup
        (pool           : in out Slab;
         objSize        : in Natural;
         blockOrder     : in BuddyAllocator.Order := 0;
         alignment      : in System.Storage_Elements.Storage_Count := 8);

    ---------------------------------------------------------------------------
    -- teardown - free underlying memory used by this allocator
    ---------------------------------------------------------------------------
    procedure teardown(pool : in out Slab);

    ---------------------------------------------------------------------------
    -- Allocate - called automagically by "new". The first time this is called,
    --  the setup procedure will be called to initialized the slab's free list.
    --
    -- @param pool - a Slab set up with the desired object size & alignment.
    -- @param addr - value used by the "new" operator.
    -- @param ignore_1 - (Normally number of bytes to allocate) Ignored.
    -- @param ignore_2 - (Normally alignment for storage pools) Ignored.
    --
    -- @exception OutOfMemoryException raised when slab has no more free
    --  objects
    -- @exception NotInitializedException if setup has not been called on pool
    ---------------------------------------------------------------------------
    procedure Allocate
        (pool       : in out Slab;
         addr       : out System.Address;
         ignore_1   : in System.Storage_Elements.Storage_Count;
         ignore_2   : in System.Storage_Elements.Storage_Count);

    ---------------------------------------------------------------------------
    -- Deallocate - called automagically by "Ada.Unchecked_Deallocate"
    -- @param pool - a Slab set up with the desired object size
    -- @param addr - address of the dellocated object.
    -- @param ignore_1 - (Normally number of bytes to free) Ignored.
    -- @param ignore_2 - (Normally alignment for storage pools) Ignored.
    --
    -- @exception NotInitializedException if setup has not been called on pool
    ---------------------------------------------------------------------------
    procedure Deallocate
        (pool       : in out Slab;
         addr       : in System.Address;
         ignore_1   : in System.Storage_Elements.Storage_Count;
         ignore_2   : in System.Storage_Elements.Storage_Count);

    ---------------------------------------------------------------------------
    -- Storage_Size
    -- @param pool - pool that has previously been initialized using setup;
    -- @return size of the storage pool, before any allocations, in bytes
    --
    -- @exception NotInitializedException if setup has not been called on pool
    ---------------------------------------------------------------------------
    function Storage_Size (pool : in Slab)
        return System.Storage_Elements.Storage_Count;

end SlabAllocator;
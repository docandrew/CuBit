------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  CuAlloc as a standard Ada storage pool (RT-008, ALLOC-001 step 10).
--
--  @description
--  Allocate is cualloc_allocate and Deallocate is cualloc_free_sized: Ada
--  passes Deallocate the size and alignment the object was allocated with,
--  so the free finds a small or medium block from its arena header instead
--  of the radix map (CuAlloc.Free_Sized). CuAlloc rounds a zero size up to
--  one byte and an alignment up to 16 on both paths alike.
--
--  A package's access types use it with
--
--     pragma Default_Storage_Pool (CuBit.CuAlloc_Pool.Pool);
--
--  in the package spec (RM 13.11.3: as a configuration pragma, only null
--  or Standard is allowed), or one access type with
--  for T'Storage_Pool use CuBit.CuAlloc_Pool.Pool. The calls are static
--  (the pool's specific type is known), not dispatching.
------------------------------------------------------------------------------
with System.Storage_Elements;
with System.Storage_Pools;

package CuBit.CuAlloc_Pool is

   type CuAlloc_Storage_Pool is new System.Storage_Pools.Root_Storage_Pool with null record;

   overriding procedure Allocate
     (Pool                     : in out CuAlloc_Storage_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);
   --  A block from the calling thread's CuAlloc heap. Storage_Error when
   --  the heap's provider refuses.

   overriding procedure Deallocate
     (Pool                     : in out CuAlloc_Storage_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);
   --  Gives the block back with its size and alignment (any thread may).

   overriding function Storage_Size
     (Pool : CuAlloc_Storage_Pool) return System.Storage_Elements.Storage_Count;
   --  Unbounded: Storage_Count'Last.

   Pool : CuAlloc_Storage_Pool;
   --  The process heap's pool.

end CuBit.CuAlloc_Pool;

------------------------------------------------------------------------------
--  System.Memory for the CuBit user runtime: GNAT's allocator entry points
--  (allocators, Unchecked_Deallocation, unconstrained function results)
--  over CuAlloc, the one process heap (userspace/allocator/process,
--  docs/userspace-allocator.md). A request the heap cannot satisfy ends the
--  process with a message: this runtime has no exception propagation, and
--  allocator call sites do not check for null.
------------------------------------------------------------------------------

package System.Memory is

   type size_t is mod 2 ** Standard'Address_Size;

   function Alloc (Size : size_t) return System.Address
     with Export, Convention => C, External_Name => "__gnat_malloc";
   procedure Free (Ptr : System.Address)
     with Export, Convention => C, External_Name => "__gnat_free";
   function Realloc
     (Ptr : System.Address; Size : size_t) return System.Address
     with Export, Convention => C, External_Name => "__gnat_realloc";

end System.Memory;

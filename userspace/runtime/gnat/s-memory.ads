------------------------------------------------------------------------------
--  System.Memory for the CuBit user runtime: GNAT's allocator entry points
--  (allocators, Unchecked_Deallocation, unconstrained function results).
--  Their bodies are CuAlloc's (CuAlloc_Gnat, userspace/allocator/process,
--  in libgnat-user.a beside CuAlloc itself), so each is CuAlloc's fast path
--  inline: the one process heap (docs/userspace-allocator.md). A request
--  the heap cannot satisfy ends the process with a message.
------------------------------------------------------------------------------

package System.Memory is

   type size_t is mod 2 ** Standard'Address_Size;

   function Alloc (Size : size_t) return System.Address
     with Import, Convention => C, External_Name => "__gnat_malloc";
   procedure Free (Ptr : System.Address)
     with Import, Convention => C, External_Name => "__gnat_free";
   function Realloc
     (Ptr : System.Address; Size : size_t) return System.Address
     with Import, Convention => C, External_Name => "__gnat_realloc";

end System.Memory;

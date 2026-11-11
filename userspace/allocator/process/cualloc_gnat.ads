------------------------------------------------------------------------------
--  GNAT's heap entry points on CuBit (System.Memory imports them): Ada
--  allocators, Unchecked_Deallocation and unconstrained function results
--  over CuAlloc, the one process heap. Here, beside CuAlloc_Native, so that
--  each is CuAlloc's fast path inline with one tail call for the rest (the
--  user runtime's own units cannot see CuAlloc's bodies). In
--  libgnat-user.a and libcubit_allocator.a, not libc.a (C programs have no
--  Ada allocators).
--
--  A request the heap cannot satisfy ends the process with a message: the
--  user runtime has no exception propagation here, and allocator call
--  sites do not check for null.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;

package CuAlloc_Gnat is
   type size_t is mod 2 ** Standard'Address_Size;

   function Alloc (Size : size_t) return System.Address
     with Export, Convention => C, External_Name => "__gnat_malloc";
   procedure Free (Ptr : System.Address)
     with Export, Convention => C, External_Name => "__gnat_free";
   function Realloc (Ptr : System.Address; Size : size_t) return System.Address
     with Export, Convention => C, External_Name => "__gnat_realloc";
end CuAlloc_Gnat;

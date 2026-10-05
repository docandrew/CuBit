------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The C library's malloc family over CuAlloc, the one process heap
--  (docs/userspace-allocator.md, "CuAlloc: one allocator for everything").
--  musl's mallocng is not built (replaced-by-ada.txt). Also musl's internal
--  names for the same calls (__libc_malloc and friends) and its malloc hooks
--  that do nothing here (no fork, no dynamic loader donating memory).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System;

package CuBit.Libc_Memory is
   subtype size_t is Interfaces.C.size_t;
   subtype int is Interfaces.C.int;

   function malloc (Bytes : size_t) return System.Address
     with Export, Convention => C, External_Name => "malloc";
   procedure free (Item : System.Address)
     with Export, Convention => C, External_Name => "free";
   function calloc (Count, Bytes : size_t) return System.Address
     with Export, Convention => C, External_Name => "calloc";
   function realloc (Item : System.Address; Bytes : size_t) return System.Address
     with Export, Convention => C, External_Name => "realloc";
   function reallocarray (Item : System.Address; Count, Bytes : size_t) return System.Address
     with Export, Convention => C, External_Name => "reallocarray";
   function aligned_alloc (Alignment, Bytes : size_t) return System.Address
     with Export, Convention => C, External_Name => "aligned_alloc";
   function memalign (Alignment, Bytes : size_t) return System.Address
     with Export, Convention => C, External_Name => "memalign";
   function posix_memalign (Result : System.Address; Alignment, Bytes : size_t) return int
     with Export, Convention => C, External_Name => "posix_memalign";
   function malloc_usable_size (Item : System.Address) return size_t
     with Export, Convention => C, External_Name => "malloc_usable_size";

   --  musl's internal names (its stdio, locale, time-zone code call these).
   function libc_malloc (Bytes : size_t) return System.Address
     with Export, Convention => C, External_Name => "__libc_malloc";
   function libc_malloc_impl (Bytes : size_t) return System.Address
     with Export, Convention => C, External_Name => "__libc_malloc_impl";
   procedure libc_free (Item : System.Address)
     with Export, Convention => C, External_Name => "__libc_free";
   function libc_calloc (Count, Bytes : size_t) return System.Address
     with Export, Convention => C, External_Name => "__libc_calloc";
   function libc_realloc (Item : System.Address; Bytes : size_t) return System.Address
     with Export, Convention => C, External_Name => "__libc_realloc";
   procedure malloc_atfork (Who : int)
     with Export, Convention => C, External_Name => "__malloc_atfork";
   procedure malloc_donate (First, Last : System.Address)
     with Export, Convention => C, External_Name => "__malloc_donate";
end CuBit.Libc_Memory;

------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  musl functions the libc's Ada calls (docs/c-removal.md): they are in
--  the same libc.a, so hidden ones (__lock) link too.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System;

package CuBit.Libc_Imports with Preelaborate is

   subtype int is Interfaces.C.int;
   subtype size_t is Interfaces.C.size_t;

   --  musl's internal lock: an int, zero when free.
   type Lock_Word is new int with Volatile;
   procedure Lock (Word : access Lock_Word)
   with Import, Convention => C, External_Name => "__lock";
   procedure Unlock (Word : access Lock_Word)
   with Import, Convention => C, External_Name => "__unlock";

   --  The calling thread's errno.
   type int_Access is access all int with Convention => C;
   function Errno_Location return int_Access
   with Import, Convention => C, External_Name => "__errno_location";

   function mmap
     (Address : System.Address; Length : size_t; Protection, Flags : int;
      Descriptor : int; Offset : Interfaces.C.long) return System.Address
   with Import, Convention => C, External_Name => "mmap";
   function munmap (Address : System.Address; Length : size_t) return int
   with Import, Convention => C, External_Name => "munmap";
   --  mmap's failure value, (void *) -1.
   MAP_FAILED : constant System.Address :=
     System'To_Address (16#FFFF_FFFF_FFFF_FFFF#);

end CuBit.Libc_Imports;

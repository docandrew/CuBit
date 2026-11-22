------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  C entry points for CuBit.Path_Names, so the libc resolves names with
--  the proved code instead of a C copy. Compiled into the libc
--  (userspace/libc/build.sh); declared in the libc's cubit_fd.h.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System;

package CuBit.Path_Names_C with Preelaborate is

   --  errno values returned negated (the Linux x86-64 ABI, as musl's
   --  <errno.h>).
   ENOENT       : constant := 2;
   EINVAL       : constant := 22;
   ERANGE       : constant := 34;
   ENAMETOOLONG : constant := 36;

   --  The CuBit name for the NUL-terminated Path, relative ones starting
   --  from Base (Base_Length bytes, a CuBit name; ignored otherwise),
   --  written to Result (Capacity bytes, not NUL-terminated). Its length,
   --  or -ENOENT (empty or null path), -ENAMETOOLONG, -EINVAL (bad base).
   function Resolve
     (Base : System.Address; Base_Length : Interfaces.C.size_t;
      Path : System.Address; Result : System.Address;
      Capacity : Interfaces.C.size_t) return Interfaces.C.long
   with Export, Convention => C, External_Name => "__cubit_name_resolve";

   --  getcwd's text for the name (Length bytes) in Result (Size bytes),
   --  NUL-terminated: its length with the NUL, or -ERANGE if Size is too
   --  small, -EINVAL for an overlong name.
   function Display
     (Item : System.Address; Length : Interfaces.C.size_t;
      Result : System.Address; Size : Interfaces.C.size_t)
      return Interfaces.C.long
   with Export, Convention => C, External_Name => "__cubit_name_display";

end CuBit.Path_Names_C;

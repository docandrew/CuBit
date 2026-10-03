------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  C entry point for CuBit.Launch_Arguments, so the libc start code
--  (userspace/libc/crt/crt1.c) and posix_spawn check launch blocks with the
--  proved validator rather than a C copy. Compiled into the libc
--  (userspace/libc/build.sh); declared in <cubit/launch.h>.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;
with System;

package CuBit.Launch_Arguments_C with Preelaborate is

   --  1 if the Length bytes at Block are a well-formed launch block, and
   --  then its argument and environment counts; 0 (counts untouched)
   --  otherwise, including a null Block or a Length out of range.
   function Validate
     (Block : System.Address; Length : Unsigned_32;
      Arguments, Environment : access Unsigned_32) return Interfaces.C.int
   with Export, Convention => C,
        External_Name => "__cubit_launch_arguments_validate";

end CuBit.Launch_Arguments_C;

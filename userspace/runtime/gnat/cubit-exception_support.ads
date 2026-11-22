------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The C-named hooks upstream GNAT's exception units import from its C
--  runtime files (cio.c, adaint.c), for the user runtime (EXC-001).
--
--  @description
--  put_char_stderr: CuBit has no standard error, so the character goes to
--  the debug console; only exception traces (GNAT.Exception_Traces, off
--  unless a program turns them on) and Exception_Information's callers
--  write this way. The last-chance handler formats its report itself
--  (Ada.Exceptions.Last_Chance_Handler).
--
--  __gnat_get_executable_load_address: Null_Address, as upstream answers on
--  targets whose programs are not position independent: CuBit programs
--  are static and non-PIE, so traceback addresses are link-time addresses.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System;

package CuBit.Exception_Support with Preelaborate is

   procedure Put_Char_Stderr (C : Interfaces.C.int)
   with Export, Convention => C, External_Name => "put_char_stderr";

   function Executable_Load_Address return System.Address
   with Export, Convention => C, External_Name => "__gnat_get_executable_load_address";

end CuBit.Exception_Support;

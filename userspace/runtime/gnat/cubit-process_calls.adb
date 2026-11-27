------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Kernel_Calls;

package body CuBit.Process_Calls is

   package ABI renames CuBit.Kernel_ABI;

   procedure Exit_Process (Code : Exit_Code := Success) is
      Ignore : Unsigned_64;
   begin
      --  The kernel never returns from it.
      loop
         Ignore := CuBit.Kernel_Calls.Call (ABI.Exit_Process, Code);
      end loop;
   end Exit_Process;

   procedure Yield is
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call (ABI.Yield);
   end Yield;

   function Grow_Heap (Bytes : Unsigned_64) return Unsigned_64 is
     (CuBit.Kernel_Calls.Call (ABI.Grow_Heap, Bytes));

   function Write_Console (Text : System.Address; Length : Unsigned_64)
     return Unsigned_64
   is (CuBit.Kernel_Calls.Call
         (ABI.Write, ABI.Console,
          Unsigned_64 (System.Storage_Elements.To_Integer (Text)), Length));

   procedure Write_Console (Text : String) is
      Ignore : Unsigned_64;
   begin
      Ignore := Write_Console (Text'Address, Text'Length);
   end Write_Console;

   function Create_Thread
     (Start, Stack, Argument, Thread_Pointer, Exit_Word : Unsigned_64)
     return Unsigned_64
   is (CuBit.Kernel_Calls.Call
         (ABI.Thread_Create, Start, Stack, Argument, Thread_Pointer, Exit_Word));

   procedure Exit_Thread is
      Ignore : Unsigned_64;
   begin
      loop
         Ignore := CuBit.Kernel_Calls.Call (ABI.Thread_Exit);
      end loop;
   end Exit_Thread;

end CuBit.Process_Calls;

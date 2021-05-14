 ------------------------------------------------------------------------------
 -- CuBit
 -- Copyright (C) 2021 Jon Andrew
 --
 -- @summary
 -- IPC Messages / Syscalls
 ------------------------------------------------------------------------------
with Interfaces; use Interfaces;

pragma Warnings (Off, "internal GNAT unit");
with System.Secondary_Stack;
pragma Warnings (On, "internal GNAT unit");

package CuBit.Messages is

   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0)
      return Unsigned_64;

   function sendMsg (to : Unsigned_64; msg : Unsigned_64) return Unsigned_64;

   function recvMsg (from : out Unsigned_64) return Unsigned_64;

   function getInfo (query : Unsigned_64; detail : Unsigned_64 := 0)
      return Unsigned_64;

   function registerDriver (driver : Unsigned_64) return Unsigned_64;

   ----------------
   -- debugPrint --
   ----------------
   procedure debugPrint (str : String);

   -----------------------
   -- getSecondaryStack --
   -----------------------
   function getSecondaryStack return System.Secondary_Stack.SS_Stack_Ptr
      with Export, Convention => C,
            External_Name => "__gnat_get_secondary_stack";

end CuBit.Messages;

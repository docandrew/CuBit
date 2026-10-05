------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The system-call instruction, for code without an Ada run-time library
--  (the libc's Ada, docs/c-removal.md).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Kernel_ABI;

package CuBit.Kernel_Calls with Preelaborate is

   --  System call Number with up to six arguments; what RAX holds after.
   function Call
     (Number : CuBit.Kernel_ABI.System_Call;
      A0, A1, A2, A3, A4, A5 : Unsigned_64 := 0) return Unsigned_64
   with Inline;

   --  SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY: send the message (Label, and
   --  Length in the tag's high half; words W0 .. W3) through the endpoint
   --  in Slot. Its reply becomes a completion carrying Token, or with
   --  No_Completion_Token there is none (one-way). 1 if queued.
   No_Completion_Token : constant Unsigned_64 := Unsigned_64'Last;
   function Submit
     (Slot : Unsigned_64; Label : Unsigned_32; Length : Unsigned_8;
      W0, W1, W2, W3 : Unsigned_64; Token : Unsigned_64) return Unsigned_64;

end CuBit.Kernel_Calls;

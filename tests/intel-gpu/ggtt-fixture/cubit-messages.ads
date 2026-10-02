with Interfaces; use Interfaces;
with System;
package CuBit.Messages is
   -- Hosted transport fixture, not a kernel ABI/authority simulation.
   subtype CapabilitySlot is Unsigned_64 range 0 .. 63;
   type MessageTag is record
      label : Unsigned_32;
      length, flags : Unsigned_8;
      reserved : Unsigned_16;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      authorityTag : Unsigned_64 := 0;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message := ((0, 0, 0, 0), 0, [others => 0]);
   COMPLETION_OK : constant Unsigned_64 := 0;
   type CompletionEntry is record
      requestId, token : Unsigned_64;
      msg : Message;
      from : Unsigned_64;
      status : Unsigned_64 := COMPLETION_OK;
      valid : Boolean := False;
   end record;
   type Activity_Result is (Work_Available, Deadline_Reached, Unavailable);
   SYSCALL_GETTIME : constant Unsigned_64 := 1;
   SYSCALL_MAP_DEVICE : constant Unsigned_64 := 2;
   function syscall
     (call : Unsigned_64; arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64 := 0)
      return Unsigned_64;
   function capSubmit (slot : CapabilitySlot; msg : Message; token : Unsigned_64) return Boolean;
   function Poll_Completion (result : System.Address) return Unsigned_64;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result;
   Mode : Natural := 0;
   Expected_Bytes : Unsigned_64 := 8_388_608;
   Base : constant Unsigned_64 := 16#60_0000_0000#;
   Submissions, Maps, Polls, Clocks : Natural := 0;
end CuBit.Messages;

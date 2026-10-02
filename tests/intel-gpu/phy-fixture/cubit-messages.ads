with Interfaces; use Interfaces;
with System;
package CuBit.Messages is
   -- Hosted fixture only; not a kernel ABI or authorization model.
   type MessageTag is record
      label : Unsigned_32;
      length, flags : Unsigned_8;
      reserved : Unsigned_16;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message := ((0, 0, 0, 0), [others => 0]);
   COMPLETION_OK : constant Unsigned_64 := 0;
   type CompletionEntry is record
      token : Unsigned_64;
      msg : Message;
      status : Unsigned_64;
   end record;
   type Activity_Result is (Deadline_Reached);
   SYSCALL_GETTIME : constant Unsigned_64 := 1;
   SYSCALL_MAP_DEVICE : constant Unsigned_64 := 2;
   function syscall (Call : Unsigned_64; Arg0, Arg1, Arg2, Arg3 : Unsigned_64 := 0) return Unsigned_64;
   function capSubmit (Slot : Unsigned_64; Msg : Message; Token : Unsigned_64) return Boolean;
   function Poll_Completion (Result : System.Address) return Unsigned_64;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result;
   Mode, Maps, Submissions, Polls, Clocks : Natural := 0;
   Base : constant Unsigned_64 := 16#6000000000#;
end CuBit.Messages;

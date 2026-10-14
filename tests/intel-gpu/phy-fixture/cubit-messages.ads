with Interfaces; use Interfaces;
with System;
package CuBit.Messages is
   -- Hosted fixture only; not a kernel ABI or authorization model.
   --  The tag's flag byte, as CuBit.Messages has it (CAP-002).
   type Protocol_Flags is mod 2 ** 4 with Size => 4;
   type IPC_Flags is record
      Accepts_Capability, Capability_Transferred, Capability_Refused,
      Capability_Offered : Boolean := False;
   end record with Size => 4;
   for IPC_Flags use record
      Accepts_Capability     at 0 range 0 .. 0;
      Capability_Transferred at 0 range 1 .. 1;
      Capability_Refused     at 0 range 2 .. 2;
      Capability_Offered     at 0 range 3 .. 3;
   end record;
   No_IPC_Flags : constant IPC_Flags := (others => False);
   type MessageTag is record
      label : Unsigned_32;
      length : Unsigned_8;
      protocol : Protocol_Flags := 0;
      ipc : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_16;
   end record;
   --  The whole flag byte, as decoders that validate it exactly read it.
   function Flag_Byte (Tag : MessageTag) return Unsigned_8 is
     (Unsigned_8 (Tag.protocol)
      + (if Tag.ipc.Accepts_Capability then 16 else 0)
      + (if Tag.ipc.Capability_Transferred then 32 else 0)
      + (if Tag.ipc.Capability_Refused then 64 else 0)
      + (if Tag.ipc.Capability_Offered then 128 else 0));
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message := ((label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0), [others => 0]);
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

with Interfaces; use Interfaces;
with System;
package CuBit.Messages is
   --  The tag's flag byte, as CuBit.Messages has it (CAP-002).
   type Protocol_Flags is mod 2 ** 4;
   type IPC_Flags is record
      Accepts_Capability, Capability_Transferred, Capability_Refused,
      Capability_Offered : Boolean := False;
   end record;
   No_IPC_Flags : constant IPC_Flags := (others => False);
   type Tag_Type is record
      label : Unsigned_32 := 0;
      length : Unsigned_8 := 0;
      protocol : Protocol_Flags := 0;
      ipc : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_16 := 0;
   end record;
   --  The whole flag byte, as decoders that validate it exactly read it.
   function Flag_Byte (Tag : Tag_Type) return Unsigned_8 is
     (Unsigned_8 (Tag.protocol)
      + (if Tag.ipc.Accepts_Capability then 16 else 0)
      + (if Tag.ipc.Capability_Transferred then 32 else 0)
      + (if Tag.ipc.Capability_Refused then 64 else 0)
      + (if Tag.ipc.Capability_Offered then 128 else 0));
   type Word_Array is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : Tag_Type;
      words : Word_Array := [others => 0];
   end record;
   NULL_MESSAGE : constant Message := (others => <>);
   type CompletionEntry is record
      token, status : Unsigned_64 := 0;
      msg : Message;
   end record;
   type Activity_Result is (Idle);
   COMPLETION_OK : constant Unsigned_64 := 0;
   SYSCALL_GETTIME : constant := 1;
   Response : Message;
   Bad_Token, Bad_Status, Submit_Fails, Pending, Retry_First : Boolean := False;
   Submissions, Polls : Natural := 0;
   Now, Step : Unsigned_64 := 0;
   Last_Token : Unsigned_64 := 0;
   function syscall (Op : Natural) return Unsigned_64;
   function capSubmit (Slot : Natural; Msg : Message; Token : Unsigned_64) return Boolean;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result;
   function Poll (Result : System.Address) return Unsigned_64;
end CuBit.Messages;

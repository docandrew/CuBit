with Interfaces; use Interfaces;
with CuBit.Process_IDs;
with CuBit.Handles;
--  Hosted stand-in for CuBit.Messages (CAP-002): the transport surface the
--  intel-gpu buffer units use, recording what they submit and answer.
package CuBit.Messages is
   function From_Word (Word : Unsigned_64) return CuBit.Process_IDs.Process_ID
     renames CuBit.Process_IDs.From_Word;
   function To_Word (ID : CuBit.Process_IDs.Process_ID) return Unsigned_64
     renames CuBit.Process_IDs.To_Word;
   function Is_Process (ID : CuBit.Process_IDs.Process_ID) return Boolean
     renames CuBit.Process_IDs.Is_Process;
   SYSCALL_GETPID : constant := 1;
   -- Reply syscalls return 1 on success.
   Reply_Result : Unsigned_64 := 1;
   Reply_Count : Natural := 0;
   --  The handle word of the last saved reply answered.
   Replied_Word : Unsigned_64 := 0;
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
   type Message_Words is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      words : Message_Words;
   end record;
   NULL_MESSAGE : constant Message := ((label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0), [others => 0]);
   Last_Reply : Message := NULL_MESSAGE;
   function replyCap (saved : CuBit.Handles.Reply; msg : Message) return Unsigned_64;
   COMPLETION_OK : constant := 0;
   type CompletionEntry is record
      token, from, status : Unsigned_64 := 0;
      msg : Message := NULL_MESSAGE;
      valid : Boolean := False;
   end record;
   Last_Submit : Message := NULL_MESSAGE;
   Last_Target_Word, Last_Token : Unsigned_64 := 0;
   Submit_Result : Boolean := True;
   function capSubmit (target : CuBit.Handles.Endpoint; msg : Message;
                       token : Unsigned_64) return Boolean;
   --  Synchronous calls (Native_GPU_Calls): none of these fixtures expects
   --  one, so a call fails the test.
   Wait_Forever : constant Unsigned_64 := Unsigned_64'Last;
   REPLY_TIMEOUT : constant Unsigned_32 := 16#FFFF_0001#;
   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64 is (Milliseconds);
   function capCall (target : CuBit.Handles.Endpoint; msg : in out Message;
                     Deadline : Unsigned_64) return MessageTag;
   function Syscall (Number : Unsigned_64;
     A, B, C, D, E, F : Unsigned_64 := 0) return Unsigned_64;
end CuBit.Messages;

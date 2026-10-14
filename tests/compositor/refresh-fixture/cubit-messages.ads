with Interfaces; use Interfaces;
with CuBit.Handles;
package CuBit.Messages is
   SYSCALL_SBRK : constant Unsigned_64 := 8;
   COMPLETION_OK : constant Unsigned_64 := 0;
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
      label : Unsigned_32 := 0;
      length : Unsigned_8 := 0;
      protocol : Protocol_Flags := 0;
      ipc : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_16 := 0;
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
      authorityTag : Unsigned_64 := 0;
      words : MessageWords := [others => 0];
   end record;
   NULL_MESSAGE : constant Message := (others => <>);
   type CompletionEntry is record
      token : Unsigned_64 := 0;
      msg : Message;
      valid : Boolean := True;
      status : Unsigned_64 := 0;
   end record;
   function syscall (Number, Bytes : Unsigned_64) return Unsigned_64;
   function capSubmit (Target : CuBit.Handles.Endpoint; Msg : Message; Token : Unsigned_64) return Boolean;
   procedure debugPrint (Text : String);
   Storage : aliased String (1 .. 12288) := [others => ASCII.NUL] with Alignment => 4096;
   Last_Message : Message;
   Last_Token : Unsigned_64 := 0;
   Submissions : Natural := 0;
   Fail_Submit, Fail_Allocate : Boolean := False;
end CuBit.Messages;

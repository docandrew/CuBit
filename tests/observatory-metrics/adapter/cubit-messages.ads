with CuBit.Handles;
with Interfaces; use Interfaces;
package CuBit.Messages is
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
   type Payload is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      words : Payload := [others => 0];
   end record;
   NULL_MESSAGE : constant Message := (others => <>);
   type CompletionEntry is record
      token, status : Unsigned_64 := 0;
      msg : Message;
      valid : Boolean := False;
   end record;
   Allow_Submit : Boolean := True;
   Submits : Natural := 0;
   Sent : Message;
   Sent_Token : Unsigned_64 := 0;
   function capSubmit (slot : CuBit.Handles.Endpoint; msg : Message; token : Unsigned_64) return Boolean;
end CuBit.Messages;

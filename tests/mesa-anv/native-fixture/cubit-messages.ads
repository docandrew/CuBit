with Interfaces; use Interfaces;
with CuBit.Handles;
package CuBit.Messages is
   -- The GPU service endpoint the bridge is handed (any live handle word).
   function GPU return CuBit.Handles.Endpoint;
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
      authorityTag : Unsigned_64;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message := ((label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0), 0, [others => 0]);
   Calls : Natural := 0;
   Budget_Mode : Boolean := False;
   Fault : Natural := 0;
   Corrupt_Return : Boolean := False;
   Envelope_Bit : Natural range 0 .. 63 := 0;
   Wait_Forever : constant Unsigned_64 := Unsigned_64'Last;
   REPLY_TIMEOUT : constant Unsigned_32 := 16#FFFF_0001#;
   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64;
   function capCall (Target : CuBit.Handles.Endpoint; Msg : in out Message;
                     Deadline : Unsigned_64) return MessageTag;
end CuBit.Messages;

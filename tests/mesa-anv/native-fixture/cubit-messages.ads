with Interfaces; use Interfaces;
package CuBit.Messages is
   subtype CapabilitySlot is Unsigned_64 range 0 .. 63;
   type MessageTag is record
      label : Unsigned_32;
      length, flags : Unsigned_8;
      reserved : Unsigned_16;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      authorityTag : Unsigned_64;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message := ((0, 0, 0, 0), 0, [others => 0]);
   Calls : Natural := 0;
   Budget_Mode : Boolean := False;
   Fault : Natural := 0;
   Corrupt_Return : Boolean := False;
   Envelope_Bit : Natural range 0 .. 63 := 0;
   function capCall (Slot : CapabilitySlot; Msg : in out Message) return MessageTag;
end CuBit.Messages;

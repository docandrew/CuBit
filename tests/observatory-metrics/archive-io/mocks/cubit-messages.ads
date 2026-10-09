with Interfaces; use Interfaces;
package CuBit.Messages is
   subtype CapabilitySlot is Unsigned_64;
   type MessageTag is record
      label : Unsigned_32 := 0;
      length, flags : Unsigned_8 := 0;
      reserved : Unsigned_16 := 0;
   end record;
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
   function capSubmit (slot : CapabilitySlot; msg : Message; token : Unsigned_64) return Boolean;
end CuBit.Messages;

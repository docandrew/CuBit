with Interfaces; use Interfaces;
package CuBit.Messages is
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
   function capCall (slot : Unsigned_64; msg : in out Message) return MessageTag;
   procedure debugPrint (value : String);
   type Bytes is array (Natural range <>) of Unsigned_8;
   Grant_Buffer : Bytes (0 .. 4095) := [others => 0];
   procedure Open_Image (Path : String);
   procedure Close_Image;
end CuBit.Messages;

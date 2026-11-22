pragma Ada_2022;
with Interfaces;
package CuBit.Messages is
   use type Interfaces.Unsigned_8;
   Wait_Forever : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
   type MessageTag is record
      label : Interfaces.Unsigned_32 := 0;
      length : Interfaces.Unsigned_8 := 0;
      protocol, ipc : Interfaces.Unsigned_8 := 0;
      reserved : Interfaces.Unsigned_16 := 0;
   end record;
   type MessageWords is array (Natural range 0 .. 3) of Interfaces.Unsigned_64;
   type Message is record
      tag : MessageTag;
      words : MessageWords := [others => 0];
      authorityTag : Interfaces.Unsigned_64 := 0;
   end record;
   NULL_MESSAGE : constant Message := (others => <>);
   function Flag_Byte (Tag : MessageTag) return Interfaces.Unsigned_8 is (Tag.protocol or Tag.ipc);
end CuBit.Messages;

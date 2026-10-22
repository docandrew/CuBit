pragma Ada_2022;
with Interfaces; use Interfaces;
package CuBit.Messages is
 type MessageWords is array(Natural range 0..3) of Unsigned_64;
 type IPC_Flags is record Empty : Boolean := True; end record;
 type MessageTag is record
  label : Unsigned_32 := 0; length : Unsigned_8 := 4;
  protocol : Unsigned_8 := 0; ipc : IPC_Flags; reserved : Unsigned_16 := 0;
 end record;
 type Message is record tag : MessageTag; words : MessageWords := [others=>0]; end record;
 NULL_MESSAGE : constant Message := (others=><>);
 Kernel_Reply_First : constant Unsigned_32 := 16#FFFF_0000#;
 REPLY_TIMEOUT : constant Unsigned_32 := 16#FFFF_0001#;
 function Flag_Byte(Tag : MessageTag) return Unsigned_8 is (Tag.protocol);
end CuBit.Messages;

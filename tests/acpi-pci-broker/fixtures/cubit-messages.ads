pragma Ada_2022;
with Interfaces; use Interfaces; with CuBit.Process_IDs; with Capability_ABI;
package CuBit.Messages is
 subtype Process_ID is CuBit.Process_IDs.Process_ID;
 No_Process : constant Process_ID := CuBit.Process_IDs.No_Process;
 subtype IPC_Flags is Capability_ABI.IPC_Flags;
 type MessageTag is record label : Unsigned_32 := 0; length : Unsigned_8 := 0; protocol : Capability_ABI.Protocol_Flags := 0; ipc : IPC_Flags; reserved : Unsigned_16 := 0; end record;
 type MessageWords is array (0..3) of Unsigned_64;
 type Message is record tag : MessageTag; authorityTag : Unsigned_64 := 0; words : MessageWords := [others=>0]; end record;
 NULL_MESSAGE : constant Message := (others=><>);
 Last_Reply : Message; Plain_Replies : Natural := 0;
 function reply (replyTo : Process_ID; msg : Message) return Unsigned_64;
end CuBit.Messages;

pragma Ada_2022;
with Interfaces; use Interfaces;
with Capability_ABI;
with CuBit.Process_IDs;
package CuBit.Messages is
   type Protocol_Flags is new Capability_ABI.Protocol_Flags;
   type IPC_Flags is new Capability_ABI.IPC_Flags;
   No_IPC_Flags : constant IPC_Flags := (others => False);
   type MessageTag is record
      label : Unsigned_32 := 0;
      length : Unsigned_8 := 0;
      protocol : Protocol_Flags := 0;
      ipc : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_16 := 0;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      authorityTag : Unsigned_64 := 0;
      words : MessageWords := [others => 0];
   end record;
   NULL_MESSAGE : constant Message := (others => <>);
   subtype Process_ID is CuBit.Process_IDs.Process_ID;
   function To_Word (Process : Process_ID) return Unsigned_64
     renames CuBit.Process_IDs.To_Word;
   function Flag_Byte (Tag : MessageTag) return Unsigned_8 is
     (Capability_ABI.Flag_Byte (Capability_ABI.Protocol_Flags (Tag.protocol),
                              Capability_ABI.IPC_Flags (Tag.ipc)));
   function reply (Sender : Process_ID; Msg : Message) return Unsigned_64
     with Import, Convention => Ada, External_Name => "unused_model_current_reply";
end CuBit.Messages;

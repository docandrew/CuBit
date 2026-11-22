with Interfaces;use Interfaces;with Capability_ABI;with CuBit.Process_IDs;
package CuBit.Messages is
   type Protocol_Flags is new Capability_ABI.Protocol_Flags;
   type IPC_Flags is new Capability_ABI.IPC_Flags;
   No_IPC_Flags : constant IPC_Flags := (others => False);

   type MessageTag is record
      label    : Unsigned_32;
      length   : Unsigned_8;
      protocol : Protocol_Flags := 0;
      ipc      : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_16; -- Not authenticated; never an authority tag
   end record with Size => 64;

   for MessageTag use record
      label    at 0 range 0 .. 31;
      length   at 4 range 0 .. 7;
      protocol at 5 range 0 .. 3;
      ipc      at 5 range 4 .. 7;
      reserved at 6 range 0 .. 15;
   end record;

   NULL_TAG : constant MessageTag :=
     (label => 0, length => 0, protocol => 0, ipc => No_IPC_Flags, reserved => 0);

   --  The tag's whole flag byte, for decoders that validate it exactly.
   function Flag_Byte (Tag : MessageTag) return Unsigned_8 is
     (Capability_ABI.Flag_Byte (Capability_ABI.Protocol_Flags (Tag.protocol),
                                Capability_ABI.IPC_Flags (Tag.ipc)));

   type MessageWords is array (0 .. 3) of Unsigned_64;

   type Message is record
      tag      : MessageTag;
      --  Kernel-stamped from the capability authorizing this message.
      authorityTag : Unsigned_64 := 0;
      words    : MessageWords;
   end record;

   pragma Compile_Time_Error (Message'Size /= 48 * 8,
                              "IPC message ABI must remain 48 bytes");

   NULL_MESSAGE : constant Message :=
     (tag => NULL_TAG, authorityTag => 0, words => [others => 0]);

   subtype Process_ID is CuBit.Process_IDs.Process_ID;
   No_Process : Process_ID renames CuBit.Process_IDs.No_Process;
   function "=" (Left, Right : Process_ID) return Boolean
     renames CuBit.Process_IDs."=";
   function To_Word (Process : Process_ID) return Unsigned_64
     renames CuBit.Process_IDs.To_Word;
   function From_Word (Word : Unsigned_64) return Process_ID
     renames CuBit.Process_IDs.From_Word;
   function Is_Process (Process : Process_ID) return Boolean
     renames CuBit.Process_IDs.Is_Process;

   -- Same fixed hosted owner identity as the original EC event fixture.
   function Own_Process return Process_ID is
     (CuBit.Process_IDs.From_Word(16#1234_0000_0001#));
end CuBit.Messages;

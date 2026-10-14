--  Hosted stand-in for CuBit.Messages: only the message and process types
--  CuBit.Handles.IPC names. No system calls (those go through the mocked
--  CuBit.Handles.Kernel, kernel_mock.adb).
with Interfaces; use Interfaces;
with CuBit.Process_IDs;
package CuBit.Messages is
   subtype Process_ID is CuBit.Process_IDs.Process_ID;
   No_Process : Process_ID renames CuBit.Process_IDs.No_Process;
   function To_Word (Process : Process_ID) return Unsigned_64
     renames CuBit.Process_IDs.To_Word;
   function From_Word (Word : Unsigned_64) return Process_ID
     renames CuBit.Process_IDs.From_Word;
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
      label    : Unsigned_32;
      length   : Unsigned_8;
      protocol    : Protocol_Flags := 0;
      ipc      : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_16;
   end record with Size => 64;
   for MessageTag use record
      label    at 0 range 0 .. 31;
      length   at 4 range 0 .. 7;
      protocol    at 5 range 0 .. 3;
      ipc      at 5 range 4 .. 7;
      reserved at 6 range 0 .. 15;
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
      tag          : MessageTag;
      authorityTag : Unsigned_64 := 0;
      words        : MessageWords;
   end record;
   NULL_TAG : constant MessageTag := (label => 0, length => 0,
                                      protocol => 0, ipc => <>, reserved => 0);
   NULL_MESSAGE : constant Message := (NULL_TAG, 0, [others => 0]);
end CuBit.Messages;

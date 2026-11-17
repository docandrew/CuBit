--  Hosted stand-in for CuBit.Messages (tests/input-rings): only the message
--  types CuBit.Input encodes into, no system calls.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Process_IDs;

package CuBit.Messages is
   type Protocol_Flags is mod 2 ** 4;
   type IPC_Flags is mod 2 ** 4;
   No_IPC_Flags : constant IPC_Flags := 0;
   type MessageTag is record
      label    : Unsigned_32;
      length   : Unsigned_8;
      protocol : Protocol_Flags := 0;
      ipc      : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_16;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag          : MessageTag;
      authorityTag : Unsigned_64 := 0;
      words        : MessageWords;
   end record;
   DRIVER_KEYBOARD : constant Unsigned_64 := 1;
   DRIVER_MOUSE    : constant Unsigned_64 := 10;
   subtype Process_ID is CuBit.Process_IDs.Process_ID;
   No_Process : Process_ID renames CuBit.Process_IDs.No_Process;
   function "=" (Left, Right : Process_ID) return Boolean renames CuBit.Process_IDs."=";
   function To_Word (Process : Process_ID) return Unsigned_64 renames CuBit.Process_IDs.To_Word;
end CuBit.Messages;

with Interfaces; use Interfaces;
with CuBit.Process_IDs;
package CuBit.Messages is
   subtype CapabilitySlot is Unsigned_64 range 0 .. 63;
   SYSCALL_GETPID : constant := 1;
   SYSCALL_INSPECT_CAPABILITY : constant := 2;
   SYSCALL_POLICY_MINT_CAPABILITY_FOR_INCARNATION : constant := 120;
   SYSCALL_POLICY_DELEGATE_ENDPOINT : constant := 121;
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
   --  The runtime's Process_ID re-exports (KERN-003).
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
   Recipient_Generation : Unsigned_64 := 7;
   Fail_Delivery : Boolean := False;
   Wait_Forever : constant Unsigned_64 := Unsigned_64'Last;
   --  The runtime's call-deadline surface (Native_GPU_Calls): a mock reply
   --  never times out, so the deadline value is unused.
   REPLY_TIMEOUT : constant Unsigned_32 := 16#FFFF_0001#;
   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64 is (Milliseconds);
   function capCall (Slot : CapabilitySlot; Msg : in out Message;
                     Deadline : Unsigned_64) return MessageTag;
   function Syscall (Number : Unsigned_64;
     A, B, C, D, E, F : Unsigned_64 := 0) return Unsigned_64;
end CuBit.Messages;

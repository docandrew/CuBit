with Interfaces; use Interfaces;
with CuBit.Process_IDs;
package CuBit.Messages is
   function From_Word (Word : Unsigned_64) return CuBit.Process_IDs.Process_ID
     renames CuBit.Process_IDs.From_Word;
   function To_Word (ID : CuBit.Process_IDs.Process_ID) return Unsigned_64
     renames CuBit.Process_IDs.To_Word;
   function Is_Process (ID : CuBit.Process_IDs.Process_ID) return Boolean
     renames CuBit.Process_IDs.Is_Process;
   subtype CapabilitySlot is Unsigned_64 range 0 .. 63;
   SYSCALL_GETPID : constant := 1;
   SYSCALL_INSPECT_CAPABILITY : constant := 2;
   SYSCALL_POLICY_MINT_CAPABILITY_FOR_INCARNATION : constant := 120;
   SYSCALL_POLICY_DELEGATE_ENDPOINT : constant := 121;
   type Words is array (0 .. 5) of Unsigned_64;
   Inspection : Words := [1, 1, 0, 42, 0, 7];
   Application_Inspection : Words := [1, 9, 0, 42, 0, 7];
   -- Reply syscalls return 1 on success, unlike delegation's zero success.
   Save_Result, Reply_Result : Unsigned_64 := 1;
   Save_Count, Reply_Count : Natural := 0;
   Saved_Slot, Replied_Slot : Unsigned_64 := 0;
   Last_Operation : Unsigned_64 := 0;
   Last_Arguments : Words := [others => 0];
   Grant_Result : Unsigned_64 := 0;
   type MessageTag is record
      label : Unsigned_32;
      length, flags : Unsigned_8;
      reserved : Unsigned_16;
   end record;
   type Message_Words is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      words : Message_Words;
   end record;
   NULL_MESSAGE : constant Message := ((0, 0, 0, 0), [others => 0]);
   Last_Reply : Message := NULL_MESSAGE;
   function saveReplyCap (destSlot : Unsigned_64) return Unsigned_64;
   function replyCap (slot : CapabilitySlot; msg : Message) return Unsigned_64;
   COMPLETION_OK : constant := 0;
   type CompletionEntry is record
      token, from, status : Unsigned_64 := 0;
      msg : Message := NULL_MESSAGE;
      valid : Boolean := False;
   end record;
   Last_Submit : Message := NULL_MESSAGE;
   Last_Slot, Last_Token : Unsigned_64 := 0;
   Submit_Result : Boolean := True;
   function capSubmit (slot : CapabilitySlot; msg : Message;
                       token : Unsigned_64) return Boolean;
   function Syscall (Number : Unsigned_64;
     A, B, C, D, E, F : Unsigned_64 := 0) return Unsigned_64;
end CuBit.Messages;

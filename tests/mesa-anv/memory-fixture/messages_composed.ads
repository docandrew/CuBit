with Interfaces; use Interfaces;
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
   Recipient_Generation : Unsigned_64 := 7;
   Fail_Delivery : Boolean := False;
   function capCall (Slot : CapabilitySlot; Msg : in out Message) return MessageTag;
   function Syscall (Number : Unsigned_64;
     A, B, C, D, E, F : Unsigned_64 := 0) return Unsigned_64;
end CuBit.Messages;

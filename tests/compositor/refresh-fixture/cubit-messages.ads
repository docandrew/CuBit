with Interfaces; use Interfaces;
package CuBit.Messages is
   subtype CapabilitySlot is Unsigned_64 range 0 .. 63;
   CAP_SLOT_CONFIG : constant CapabilitySlot := 7;
   SYSCALL_SBRK : constant Unsigned_64 := 8;
   COMPLETION_OK : constant Unsigned_64 := 0;
   type MessageTag is record
      label : Unsigned_32 := 0;
      length, flags : Unsigned_8 := 0;
      reserved : Unsigned_16 := 0;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      authorityTag : Unsigned_64 := 0;
      words : MessageWords := [others => 0];
   end record;
   NULL_MESSAGE : constant Message := (others => <>);
   type CompletionEntry is record
      token : Unsigned_64 := 0;
      msg : Message;
      valid : Boolean := True;
      status : Unsigned_64 := 0;
   end record;
   function syscall (Number, Bytes : Unsigned_64) return Unsigned_64;
   function capSubmit (Slot : CapabilitySlot; Msg : Message; Token : Unsigned_64) return Boolean;
   procedure debugPrint (Text : String);
   Storage : aliased String (1 .. 12288) := [others => ASCII.NUL] with Alignment => 4096;
   Last_Message : Message;
   Last_Token : Unsigned_64 := 0;
   Submissions : Natural := 0;
   Fail_Submit, Fail_Allocate : Boolean := False;
end CuBit.Messages;

with System.Storage_Elements; use System.Storage_Elements;
package body CuBit.Messages is
   function syscall (Number, Bytes : Unsigned_64) return Unsigned_64 is
   begin
      pragma Assert (Number = SYSCALL_SBRK and Bytes = 12288);
      return (if Fail_Allocate then Unsigned_64'Last else Unsigned_64 (To_Integer (Storage'Address)));
   end syscall;
   function capSubmit (Slot : CapabilitySlot; Msg : Message; Token : Unsigned_64) return Boolean is
   begin
      pragma Assert (Slot = CAP_SLOT_CONFIG and Token > Last_Token and Token < Unsigned_64'Last);
      pragma Assert (Msg.tag.length = 4 and Msg.tag.flags = 0 and Msg.words (3) = 0);
      pragma Assert (Msg.words (0) = 8 and Msg.words (1) = 9);
      Last_Message := Msg;
      Last_Token := Token;
      Submissions := Submissions + 1;
      return not Fail_Submit;
   end capSubmit;
   procedure debugPrint (Text : String) is
   begin null; end debugPrint;
end CuBit.Messages;

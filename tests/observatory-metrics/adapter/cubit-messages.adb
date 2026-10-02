package body CuBit.Messages is
   function capSubmit (slot : CapabilitySlot; msg : Message; token : Unsigned_64) return Boolean is
   begin
      pragma Assert (slot = 17);
      Submits := Submits + 1; Sent := msg; Sent_Token := token;
      return Allow_Submit;
   end capSubmit;
end CuBit.Messages;

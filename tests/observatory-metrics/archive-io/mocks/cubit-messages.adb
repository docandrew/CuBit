with CuBit.Handles;
with CuBit.Handles.Fixture;
package body CuBit.Messages is
   function capSubmit (slot : CuBit.Handles.Endpoint; msg : Message; token : Unsigned_64) return Boolean is
   begin
      pragma Assert (CuBit.Handles."=" (slot, CuBit.Handles.Fixture.Endpoint_Of (17)));
      Submits := Submits + 1; Sent := msg; Sent_Token := token;
      return Allow_Submit;
   end capSubmit;
end CuBit.Messages;

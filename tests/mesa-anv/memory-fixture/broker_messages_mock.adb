package body CuBit.Messages is
   function capSubmit (target : CuBit.Handles.Endpoint; msg : Message;
                       token : Unsigned_64) return Boolean is
   begin
      Last_Target := target;
      Last_Submit := msg;
      Last_Token := token;
      return Submit_Result;
   end capSubmit;
end CuBit.Messages;

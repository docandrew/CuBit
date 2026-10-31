package CuBit.Handles.Services is
   package Logstore is
      type Optional_Endpoint (Present : Boolean := False) is record
         case Present is when True => Value : Endpoint; when False => null; end case;
      end record;
   end Logstore;
end CuBit.Handles.Services;

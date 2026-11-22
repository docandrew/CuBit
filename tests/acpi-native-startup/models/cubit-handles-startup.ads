package CuBit.Handles.Startup with Pure is
   type Optional_Endpoint (Present : Boolean := False) is record
      case Present is
         when True => Value : Endpoint;
         when False => null;
      end case;
   end record;
end CuBit.Handles.Startup;

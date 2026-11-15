package CuBit.Handles.Startup is
   type Optional_Interrupt (Present : Boolean := False) is record
      case Present is when True => Value : CuBit.Handles.Interrupt; when False => null; end case;
   end record;
end CuBit.Handles.Startup;

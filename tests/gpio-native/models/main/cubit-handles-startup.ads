package CuBit.Handles.Startup is
   type Optional_Device_Memory (Present : Boolean := False) is record
      case Present is when True => Value : Device_Memory; when False => null; end case;
   end record;
end CuBit.Handles.Startup;

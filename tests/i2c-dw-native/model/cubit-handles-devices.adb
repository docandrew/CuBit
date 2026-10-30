package body CuBit.Handles.Devices is
   use type Interfaces.Unsigned_64;
   function Map_Device
     (Region : Device_Memory; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean) return Boolean is
   begin
      if Region /= 1 or else Physical /= Expected_Physical or else
        Virtual /= Expected_Virtual or else Pages /= 1 or else Read_Only
      then raise Program_Error with "mapping changed authority/range/rights"; end if;
      Calls := Calls + 1;
      return Allow_Map;
   end Map_Device;
end CuBit.Handles.Devices;

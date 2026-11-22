with Interfaces;
package CuBit.Handles.Devices is
   Expected_Physical, Expected_Virtual : Interfaces.Unsigned_64 := 0;
   Allow_Map : Boolean := True;
   Calls : Natural := 0;
   Expected_Authority : Device_Memory := 1;
   function Map_Device
     (Region : Device_Memory; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean) return Boolean;
end CuBit.Handles.Devices;

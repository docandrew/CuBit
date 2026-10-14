with Interfaces; use Interfaces;
--  Hosted stand-in for CuBit.Handles.Devices: Map_Device checks the region,
--  addresses, chunk length and writable mode the GGTT mapping must use.
package CuBit.Handles.Devices is
   function Map_Device
     (Region : Device_Memory; Physical, Virtual, Pages : Unsigned_64;
      Read_Only : Boolean) return Boolean;
end CuBit.Handles.Devices;

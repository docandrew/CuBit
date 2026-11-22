with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles.Fixture;
with Intel_GPU_Supervisor;
package body CuBit.Handles.Devices is
   function Map_Device
     (Region : Device_Memory; Physical, Virtual, Pages : Unsigned_64;
      Read_Only : Boolean) return Boolean is
   begin
      pragma Assert (Region = CuBit.Handles.Fixture.Device_Memory_Of
                                (Intel_GPU_Supervisor.Region_Slot));
      pragma Assert (Physical = Base + 16#80_0000# + Unsigned_64 (Maps) * 2_097_152);
      pragma Assert (Virtual = 16#6400_0000# + Unsigned_64 (Maps) * 2_097_152);
      pragma Assert (Pages = 512 and not Read_Only);
      Maps := Maps + 1;
      return not (Mode = 6 and Maps = 2);
   end Map_Device;
end CuBit.Handles.Devices;

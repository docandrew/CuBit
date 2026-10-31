pragma Ada_2022;
with Interfaces;
with CuBit.Handles.IPC;
with CuBit.Handles.Devices;
package body GPIO_Kernel_Regions is
   use type CuBit.Handles.ABI.Kind;
   procedure Inspect
     (Authority : CuBit.Handles.Device_Memory; Result : out GPIO_Mapping_Policy.Description) is
      Item : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Authority);
   begin
      Result := (others => <>);
      if not Item.Current then return; end if;
      Result := (Current => True, Device_Memory => Item.Kind = CuBit.Handles.ABI.Device_Memory,
         Readable => Item.Held.Read, Writable => Item.Held.Write,
         First => GPIO_Mapping_Policy.Address (Item.Object),
         Bytes => GPIO_Mapping_Policy.Address (Item.Parameter));
   end Inspect;
   procedure Map
     (Authority : CuBit.Handles.Device_Memory;
      Physical, Virtual, Pages : GPIO_Mapping_Policy.Address; Mapped : out Boolean) is
   begin
      Mapped := CuBit.Handles.Devices.Map_Device
        (Authority, Interfaces.Unsigned_64 (Physical), Interfaces.Unsigned_64 (Virtual),
         Interfaces.Unsigned_64 (Pages), Read_Only => False);
   end Map;
end GPIO_Kernel_Regions;

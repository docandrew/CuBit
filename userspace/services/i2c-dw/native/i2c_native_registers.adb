pragma Ada_2022;
with Interfaces;
with CuBit.Handles.IPC;
with CuBit.Handles.Devices;
package body I2C_Native_Registers is
   use type CuBit.Handles.ABI.Kind;
   procedure Inspect
     (Authority : CuBit.Handles.Device_Memory; Result : out I2C_Mapping_Policy.Description) is
      Item : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Authority);
   begin
      Result := (others => <>);
      if not Item.Current then return; end if;
      Result := (Current => True, Device_Memory => Item.Kind = CuBit.Handles.ABI.Device_Memory,
         Readable => Item.Held.Read, Writable => Item.Held.Write,
         First => I2C_Mapping_Policy.Address (Item.Object),
         Bytes => I2C_Mapping_Policy.Address (Item.Parameter));
   end Inspect;
   procedure Map_One_Page
     (Authority : CuBit.Handles.Device_Memory;
      Physical, Virtual : I2C_Mapping_Policy.Address; Mapped : out Boolean) is
      Register_Page_Count : constant Interfaces.Unsigned_64 := 1;
   begin
      Mapped := CuBit.Handles.Devices.Map_Device
        (Authority, Interfaces.Unsigned_64 (Physical), Interfaces.Unsigned_64 (Virtual),
         Register_Page_Count, Read_Only => False);
   end Map_One_Page;
end I2C_Native_Registers;

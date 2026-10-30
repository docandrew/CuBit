pragma Ada_2022;
with CuBit.Handles;
with I2C_Mapping_Policy;
with I2C_Register_Window;
package I2C_Native_Registers is
   procedure Inspect
     (Authority : CuBit.Handles.Device_Memory; Result : out I2C_Mapping_Policy.Description);
   procedure Map_One_Page
     (Authority : CuBit.Handles.Device_Memory;
      Physical, Virtual : I2C_Mapping_Policy.Address; Mapped : out Boolean);
   package Window is new I2C_Register_Window
     (CuBit.Handles.Device_Memory, Inspect, Map_One_Page);
   -- Native entry passes Startup.Registers and trusted launch BAR facts to
   -- Window.Bind, then its six named callbacks to DW_I2C_Owner. Keep the handle
   -- alive for the entire mapped/controller lifetime. No handle word conversion.
end I2C_Native_Registers;

pragma Ada_2022;
with CuBit.Handles;
with GPIO_Mapping_Policy;
package GPIO_Kernel_Regions is
   procedure Inspect
     (Authority : CuBit.Handles.Device_Memory; Result : out GPIO_Mapping_Policy.Description);
   procedure Map
     (Authority : CuBit.Handles.Device_Memory;
      Physical, Virtual, Pages : GPIO_Mapping_Policy.Address; Mapped : out Boolean);
end GPIO_Kernel_Regions;

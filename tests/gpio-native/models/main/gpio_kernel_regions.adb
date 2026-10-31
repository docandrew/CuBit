with Main_Model;
package body GPIO_Kernel_Regions is
   use type GPIO_Mapping_Policy.Address;
   procedure Inspect (Authority : CuBit.Handles.Device_Memory; Result : out GPIO_Mapping_Policy.Description) is
      package P renames GPIO_Mapping_Policy;
   begin
      Main_Model.Check (Authority in 1 .. 4, "only named model memory capabilities");
      Main_Model.Metadata_Inspections := Main_Model.Metadata_Inspections + 1;
      Result := (True, True, True, True, 16#1000_0000# + P.Address (Authority) * 8192, 8192);
   end Inspect;
   procedure Map (Authority : CuBit.Handles.Device_Memory;
      Physical, Virtual, Pages : GPIO_Mapping_Policy.Address; Mapped : out Boolean) is
      pragma Unreferenced (Authority, Physical, Virtual, Pages);
   begin Mapped := True; end Map;
end GPIO_Kernel_Regions;

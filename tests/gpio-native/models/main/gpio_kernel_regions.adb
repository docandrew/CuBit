with Main_Model;
package body GPIO_Kernel_Regions is
   use type GPIO_Mapping_Policy.Address, Main_Model.Scenario, CuBit.Handles.Device_Memory;
   procedure Inspect (Authority : CuBit.Handles.Device_Memory; Result : out GPIO_Mapping_Policy.Description) is
      package P renames GPIO_Mapping_Policy;
   begin
      Main_Model.Check (Authority in 1 .. 4, "only named model memory capabilities");
      Main_Model.Metadata_Inspections := Main_Model.Metadata_Inspections + 1;
      Result := (True, True, True, True, 16#1000_0000# + P.Address (Authority) * 8192, 8192);
      if Authority=4 then
         if Main_Model.Current=Main_Model.Late_Read_Only then Result.Writable:=False;
         elsif Main_Model.Current=Main_Model.Overlapping_Community then Result.First:=16#1000_6000#;
         elsif Main_Model.Current=Main_Model.Unaligned_Community then Result.Bytes:=8193;
         end if;
      end if;
   end Inspect;
   procedure Map (Authority : CuBit.Handles.Device_Memory;
      Physical, Virtual, Pages : GPIO_Mapping_Policy.Address; Mapped : out Boolean) is
      pragma Unreferenced (Authority, Physical, Virtual, Pages);
   begin Mapped := True; end Map;
end GPIO_Kernel_Regions;

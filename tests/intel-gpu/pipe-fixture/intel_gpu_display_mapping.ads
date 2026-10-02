with Interfaces;
package Intel_GPU_Display_Mapping is
   Virtual_Base : constant Interfaces.Unsigned_64 := 16#6140_0000#;
   Mapping_Ready : Boolean := False;
   function Ready return Boolean is (Mapping_Ready);
end Intel_GPU_Display_Mapping;

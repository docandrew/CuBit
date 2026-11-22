with Intel_GPU_Capture_List;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
package Intel_GPU_ADLN_Capture with SPARK_Mode is
   subtype Class_ID is Natural range 0 .. 3;
   type Class_Pages is array (Class_ID) of Intel_GPU_Capture_List.Page;
   type Lists is record
      Valid : Boolean := False;
      Global : Intel_GPU_Capture_List.Page := [others => 0];
      Classes, Instances : Class_Pages := [others => [others => 0]];
   end record;
   -- PF only: render=0, video=1, enhancement=2, blitter=3. Absent
   -- classes encode empty pages; VF/unsupported class pointers are assembled
   -- separately using the backed null list. Engine offsets are RELATIVE.
   function Build
     (Inventory : Intel_GPU_ADLN_Inventory.Inventory;
      Topology : Intel_GPU_ADLN_Steering.Topology) return Lists;
end Intel_GPU_ADLN_Capture;

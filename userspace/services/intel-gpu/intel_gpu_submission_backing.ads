with Interfaces; use Interfaces;
package Intel_GPU_Submission_Backing with SPARK_Mode is
   -- Slices of the existing retained1MiB firmware allocation, not new grants.
   type Region is (Context_Image, Command_Ring, PML4, PDPT, PD, PT,
                   Batch_Buffer, Completion_Page, Engine_Status_Page,
                   Offscreen_Buffer, Render_State_Page, Shader_Page);
   type Region_Values is array (Region) of Unsigned_64;
   First : constant Unsigned_64 := 16#8C000#;
   After_Last : constant Unsigned_64 := 16#AD000#;
   Offscreen_GPU_VA : constant Unsigned_64 := 16#202000#;
   Completion_GPU_VA : constant Unsigned_64 := 16#201000#;
   Render_State_GPU_VA : constant Unsigned_64 := 16#206000#;
   Shader_GPU_VA : constant Unsigned_64 := 16#207000#;
   Offsets : constant Region_Values :=
     [16#8C000#,16#9C000#,16#A0000#,16#A1000#,16#A2000#,16#A3000#,
      16#A4000#,16#A5000#,16#A6000#,16#A7000#,16#AB000#,16#AC000#];
   Sizes : constant Region_Values :=
     [65536,16384,4096,4096,4096,4096,4096,4096,4096,16384,4096,4096];
   pragma Compile_Time_Error (After_Last > 1024 * 1024,
                              "submission backing exceeds retained allocation");
   function Valid_Layout return Boolean is
     (not (
      (for some R in Region => Offsets (R) mod 4096 /= 0 or else
        Sizes (R) mod 4096 /= 0 or else Sizes (R) = 0 or else
        Offsets (R) < First or else Offsets (R) + Sizes (R) > After_Last)) and then
      not (for some R in Region => (for some S in Region =>
        R /= S and then Offsets (R) < Offsets (S) + Sizes (S) and then
        Offsets (S) < Offsets (R) + Sizes (R))));
end Intel_GPU_Submission_Backing;

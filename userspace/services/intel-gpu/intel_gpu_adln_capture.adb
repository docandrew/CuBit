with Interfaces; use Interfaces;
with Intel_GPU_Capture_List; use Intel_GPU_Capture_List;
package body Intel_GPU_ADLN_Capture with SPARK_Mode is
   type Offsets is array (Positive range <>) of Unsigned_32;
   -- Linux v6.16 xe_lp_lists: global absolute MMIO, instance relative MMIO.
   Global_Offsets : constant Offsets :=
     [16#A188#, 16#40A0#, 16#40B0#, 16#4024#, 16#CEB8#, 16#CEBC#,
      16#43F4#, 16#CF68#, 16#CEC4#];
   Instance_Offsets : constant Offsets :=
     [16#50#, 16#B8#, 16#78#, 16#60#, 16#B0#, 16#64#, 16#68#,
      16#70#, 16#140#, 16#168#, 16#110#, 16#180#, 16#74#, 16#5C#,
      16#C0#, 16#6C#, 16#94#, 16#38#, 16#34#, 16#30#, 16#3C#,
      16#9C#, 16#244#, 16#80#, 16#29C#, 16#270#, 16#274#,
      16#278#, 16#27C#, 16#280#, 16#284#, 16#288#, 16#28C#];
   function Plain (Values : Offsets) return Page
     with Pre => Values'Length <= 255
   is
      Items : Descriptors (Values'Range);
   begin
      for I in Values'Range loop
         Items (I) := (Values (I), 0, 0);
      end loop;
      return Encode (Items).Bytes;
   end Plain;
   function Build
     (Inventory : Intel_GPU_ADLN_Inventory.Inventory;
      Topology : Intel_GPU_ADLN_Steering.Topology) return Lists
   is
      use Intel_GPU_ADLN_Inventory;
      Result : Lists;
      Render_List : Descriptors (1 .. 15) := [others => <>];
      Count : Natural := 3;
      Active : constant array (Class_ID) of Boolean :=
        [Inventory.Engines (Render),
         Inventory.Engines (Video_0) or Inventory.Engines (Video_2),
         Inventory.Engines (Enhance_0), Inventory.Engines (Copy)];
   begin
      if not Inventory.Valid or else not Topology.Valid or else
        not Active (0) or else not Active (3) or else
        Topology.DSS_Mask = 0 or else Topology.DSS_Mask > 63
      then return Result; end if;
      Result.Global := Plain (Global_Offsets);
      for C in Class_ID loop
         if Active (C) then Result.Instances (C) := Plain (Instance_Offsets); end if;
      end loop;
      Render_List (1) := (16#7100#, 0, 0);
      Render_List (2) := (16#7104#, 0, 0);
      Render_List (3) := (16#7108#, 0, 0);
      -- Gen12 LP exposes six DSS directly, not twelve split subslices.
      for DSS in Natural range 0 .. 5 loop
         pragma Loop_Invariant (Count in 3 .. 3 + 2 * DSS);
         if (Topology.DSS_Mask and Shift_Left (Unsigned_8 (1), DSS)) /= 0 then
            Render_List (Count + 1) := (16#E160#, 0, DSS);
            Render_List (Count + 2) := (16#E164#, 0, DSS);
            Count := Count + 2;
         end if;
      end loop;
      Result.Classes (0) := Encode (Render_List (1 .. Count)).Bytes;
      if Active (2) then
         Result.Classes (2) := Plain
           ([16#1CC000#, 16#1CD000#, 16#1CE000#, 16#1CF000#]);
      end if;
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Capture;

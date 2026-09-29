package body Intel_GPU_Combo_PHY with SPARK_Mode is
   -- Register definitions and sequencing reference: Linux v6.16,
   -- intel_combo_phy.c, intel_combo_phy_regs.h and i915_reg.h.
   function Read_Offset (Port : PHY; Item : Field) return Unsigned_32 is
      Base : constant Unsigned_32 := (if Port = A then 16#162000# else 16#6C000#);
   begin
      if Item = Misc then
         return (if Port = A then 16#64C00# else 16#64C04#);
      end if;
      return Base + (case Item is
         when TX_8 => 16#8A0#, when PCS_1 => 16#804#,
         when Comp_1 => 16#104#, when Comp_9 => 16#124#,
         when Comp_10 => 16#128#, when Comp_8 => 16#120#,
         when Comp_0 => 16#100#, when CL_5 => 16#14#,
         when Comp_3 => 16#10C#, when Misc => 0);
   end Read_Offset;

   function Write_Offset (Port : PHY; Item : Field) return Unsigned_32 is
   begin
      -- Group writes broadcast the lane-zero settings to all lanes.
      return Read_Offset (Port, Item) -
        (if Item = TX_8 or Item = PCS_1 then 16#200# else 0);
   end Write_Offset;

   function Prepare (Port : PHY; State : Snapshot) return Plan is
      Result : Plan;
      DW1 : Unsigned_32 := 0;
      DW9, DW10 : Unsigned_32;
   begin
      if (for some F in Field => State (F) = Unsigned_32'Last) then
         return Result;
      end if;
      case State (Comp_3) and 16#1F000000# is
         when 16#00000000# => DW9 := 16#62AB67BB#; DW10 := 16#51914F96#;
         when 16#01000000# => DW9 := 16#86E172C7#; DW10 := 16#77CA5EAB#;
         when 16#05000000# => DW9 := 16#93F87FE1#; DW10 := 16#8AE871C5#;
         when 16#02000000# => DW9 := 16#98FA82DD#; DW10 := 16#89E46DC1#;
         when 16#06000000# =>
            DW1 := 16#00440000#; DW9 := 16#9A00AB25#; DW10 := 16#8AE38FF1#;
         when others => Result.Status := Unknown_Process; return Result;
      end case;
      if (State (Misc) and 16#00800000#) = 0 and then
        (State (TX_8) and 16#E0000000#) = 16#A0000000# and then
        (State (PCS_1) and 16#00300000#) = 0 and then
        (State (Comp_1) and 16#00FF00FF#) = DW1 and then
        State (Comp_9) = DW9 and then State (Comp_10) = DW10 and then
        (Port = B or else (State (Comp_8) and 16#01000000#) /= 0) and then
        (State (Comp_0) and 16#80000000#) /= 0 and then
        (State (CL_5) and 16#10#) /= 0
      then
         Result.Status := Already_Ready;
         return Result;
      end if;
      Result.Status := Restore;
      Result.Writes (1) := (Misc, State (Misc) and not 16#00800000#);
      Result.Writes (2) := (TX_8, (State (TX_8) and not 16#60000000#) or 16#A0000000#);
      Result.Writes (3) := (PCS_1, State (PCS_1) and not 16#00300000#);
      Result.Writes (4) := (Comp_1, (State (Comp_1) and not 16#00FF00FF#) or DW1);
      Result.Writes (5) := (Comp_9, DW9);
      Result.Writes (6) := (Comp_10, DW10);
      if Port = A then
         Result.Writes (7) := (Comp_8, State (Comp_8) or 16#01000000#);
         Result.Count := 9;
      else
         Result.Count := 8;
      end if;
      Result.Writes (Result.Count - 1) := (Comp_0, State (Comp_0) or 16#80000000#);
      Result.Writes (Result.Count) := (CL_5, State (CL_5) or 16#10#);
      return Result;
   end Prepare;
end Intel_GPU_Combo_PHY;

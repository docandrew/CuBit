with Intel_GPU_PHY_Registers;
package body Intel_GPU_Combo_PHY with SPARK_Mode is
   -- Register definitions and sequencing reference: Linux v6.16,
   -- intel_combo_phy.c, intel_combo_phy_regs.h and i915_reg.h.
   function Same_Configuration (Item : Field; Left, Right : Unsigned_32) return Boolean is
      use Intel_GPU_PHY_Registers;
   begin
      case Item is
         when Misc =>
            declare
               L : constant Misc_Register := Decode (Left);
               R : constant Misc_Register := Decode (Right);
            begin
               return L.Comp_Power_Down = R.Comp_Power_Down;
            end;
         when TX_8 =>
            declare
               L : constant TX_Register := Decode (Left);
               R : constant TX_Register := Decode (Right);
            begin
               return L.Clock_Select = R.Clock_Select and then
                 L.Clock_Divider = R.Clock_Divider;
            end;
         when PCS_1 =>
            declare
               L : constant PCS_Register := Decode (Left);
               R : constant PCS_Register := Decode (Right);
            begin
               return L.DCC_Mode = R.DCC_Mode;
            end;
         when Comp_1 =>
            declare
               L : constant Comp1_Register := Decode (Left);
               R : constant Comp1_Register := Decode (Right);
            begin
               return L.P_High = R.P_High and then
                 L.P_Low = R.P_Low and then
                 L.N_High = R.N_High and then
                 L.N_Low = R.N_Low and then
                 L.PLVT_High = R.PLVT_High and then
                 L.PLVT_Low = R.PLVT_Low and then
                 L.NLVT_High = R.NLVT_High and then
                 L.NLVT_Low = R.NLVT_Low;
            end;
         when Comp_9 =>
            declare
               L : constant Comp9_Register := Decode (Left);
               R : constant Comp9_Register := Decode (Right);
            begin
               return L.P_High = R.P_High and then
                 L.P_Low = R.P_Low and then
                 L.N_High = R.N_High and then
                 L.N_Low = R.N_Low;
            end;
         when Comp_10 =>
            declare
               L : constant Comp10_Register := Decode (Left);
               R : constant Comp10_Register := Decode (Right);
            begin
               return L.PLVT_High = R.PLVT_High and then
                 L.PLVT_Low = R.PLVT_Low and then
                 L.NLVT_High = R.NLVT_High and then
                 L.NLVT_Low = R.NLVT_Low;
            end;
         when Comp_8 =>
            declare
               L : constant Comp8_Register := Decode (Left);
               R : constant Comp8_Register := Decode (Right);
            begin
               return L.Reference_Enable = R.Reference_Enable;
            end;
         when Comp_0 =>
            declare
               L : constant Comp0_Register := Decode (Left);
               R : constant Comp0_Register := Decode (Right);
            begin
               return L.Initialized = R.Initialized;
            end;
         when CL_5 =>
            declare
               L : constant CL_Register := Decode (Left);
               R : constant CL_Register := Decode (Right);
            begin
               return L.Power_Down_Enable = R.Power_Down_Enable;
            end;
         when Comp_3 =>
            declare
               L : constant Comp3_Register := Decode (Left);
               R : constant Comp3_Register := Decode (Right);
            begin
               return L.Process_Info = R.Process_Info and then
                 L.Voltage_Info = R.Voltage_Info;
            end;
      end case;
   end Same_Configuration;
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
      use Intel_GPU_PHY_Registers;
      Result : Plan;
      DW1 : Unsigned_32 := 0;
      DW9, DW10 : Unsigned_32;
      M : Misc_Register := Decode (State (Misc));
      TX : TX_Register := Decode (State (TX_8));
      PCS : PCS_Register := Decode (State (PCS_1));
      C1 : Comp1_Register := Decode (State (Comp_1));
      C0 : Comp0_Register := Decode (State (Comp_0));
      C8 : Comp8_Register := Decode (State (Comp_8));
      CL : CL_Register := Decode (State (CL_5));
      Info : constant Comp3_Register := Decode (State (Comp_3));
      References : Comp1_Register;
   begin
      if (for some F in Field => State (F) = Unsigned_32'Last) then
         return Result;
      end if;
      case Unsigned_32 (Info.Process_Info) * 4 + Unsigned_32 (Info.Voltage_Info) is
         when 0 => DW9 := 16#62AB67BB#; DW10 := 16#51914F96#;
         when 1 => DW9 := 16#86E172C7#; DW10 := 16#77CA5EAB#;
         when 5 => DW9 := 16#93F87FE1#; DW10 := 16#8AE871C5#;
         when 2 => DW9 := 16#98FA82DD#; DW10 := 16#89E46DC1#;
         when 6 =>
            DW1 := 16#00440000#; DW9 := 16#9A00AB25#; DW10 := 16#8AE38FF1#;
         when others => Result.Status := Unknown_Process; return Result;
      end case;
      if M.Comp_Power_Down = 0 and then
        TX.Clock_Select = 1 and then TX.Clock_Divider = 1 and then
        PCS.DCC_Mode = 0 and then
        Same_Configuration (Comp_1, State (Comp_1), DW1) and then
        Same_Configuration (Comp_9, State (Comp_9), DW9) and then
        Same_Configuration (Comp_10, State (Comp_10), DW10) and then
        (Port = B or else C8.Reference_Enable = 1) and then
        C0.Initialized = 1 and then CL.Power_Down_Enable = 1
      then
         Result.Status := Already_Ready;
         return Result;
      end if;
      Result.Status := Restore;
      M.Comp_Power_Down := 0;
      TX.Clock_Select := 1; TX.Clock_Divider := 1;
      PCS.DCC_Mode := 0;
      References := Decode (DW1);
      C1.P_High := References.P_High; C1.P_Low := References.P_Low;
      C1.N_High := References.N_High; C1.N_Low := References.N_Low;
      C1.PLVT_High := References.PLVT_High; C1.PLVT_Low := References.PLVT_Low;
      C1.NLVT_High := References.NLVT_High; C1.NLVT_Low := References.NLVT_Low;
      Result.Writes (1) := (Misc, Encode (M));
      Result.Writes (2) := (TX_8, Encode (TX));
      Result.Writes (3) := (PCS_1, Encode (PCS));
      Result.Writes (4) := (Comp_1, Encode (C1));
      Result.Writes (5) := (Comp_9, DW9);
      Result.Writes (6) := (Comp_10, DW10);
      if Port = A then
         C8.Reference_Enable := 1;
         Result.Writes (7) := (Comp_8, Encode (C8));
         Result.Count := 9;
      else
         Result.Count := 8;
      end if;
      C0.Initialized := 1; CL.Power_Down_Enable := 1;
      Result.Writes (Result.Count - 1) := (Comp_0, Encode (C0));
      Result.Writes (Result.Count) := (CL_5, Encode (CL));
      return Result;
   end Prepare;
end Intel_GPU_Combo_PHY;

package body Intel_GPU_Hotplug_IRQ with SPARK_Mode is
   use type PD.Port;
   Status_Mask : constant Unsigned_32 := 3;
   Short_Status : constant Unsigned_32 := 1;
   Long_Status : constant Unsigned_32 := 2;

   function Bit (Number : PD.Bit_Number) return Unsigned_32 is (Shift_Left (1, Number));

   function South_Bits (Ports : Port_Set) return Unsigned_32 is
      Result : Unsigned_32 := 0;
   begin
      for P in PD.Port loop
         if Ports (P) then
            Result := Result or Bit (PD.South_Live_Bit (P));
         end if;
      end loop;
      return Result;
   end South_Bits;

   function North_Bits (Ports : Port_Set) return Unsigned_32 is
      Result : Unsigned_32 := 0;
   begin
      for P in PD.Type_C_Port loop
         if Ports (P) then
            Result := Result or Bit (PD.DP_Alt_Live_Bit (P)) or Bit (PD.TBT_Live_Bit (P));
         end if;
      end loop;
      return Result;
   end North_Bits;

   --  Detection enables of Ports among Group.
   function Enables (Ports : Port_Set; Combo : Boolean) return Unsigned_32 is
      Result : Unsigned_32 := 0;
   begin
      for P in PD.Port loop
         if Ports (P) and then (P in PD.Combo_Port) = Combo then
            Result := Result or Bit (PD.Enable_Bit (P));
         end if;
      end loop;
      return Result;
   end Enables;

   procedure Read_Modify_Write (Offset : R.Register_Offset; Set : Unsigned_32; Old : out Unsigned_32) is
   begin
      Read (Offset, Old);
      --  Writing the status latches back clears them (write one to clear).
      Write (Offset, Old or Set);
   end Read_Modify_Write;

   procedure Set_Bits (Offset : R.Register_Offset; Set : Unsigned_32) is
      Old : Unsigned_32;
   begin
      Read (Offset, Old);
      Write (Offset, Old or Set);
   end Set_Bits;

   procedure Arm (Ports : Port_Set) is
      South : constant Unsigned_32 := South_Bits (Ports);
      North : constant Unsigned_32 := North_Bits (Ports);
      Old, IMR : Unsigned_32;
   begin
      --  gen3_assert_iir_is_zero: clear twice (the IIR double-buffers).
      Write (R.SDEIIR, All_Bits);
      Write (R.SDEIIR, All_Bits);
      Write (R.GEN11_DE_HPD_IIR, All_Bits);
      Write (R.GEN11_DE_HPD_IIR, All_Bits);
      Set_Bits (R.SHOTPLUG_CTL_DDI, Enables (Ports, Combo => True));
      Set_Bits (R.SHOTPLUG_CTL_TC, Enables (Ports, Combo => False));
      Set_Bits (R.GEN11_TC_HOTPLUG_CTL, Enables (Ports, Combo => False));
      Set_Bits (R.GEN11_TBT_HOTPLUG_CTL, Enables (Ports, Combo => False));
      Read (R.SDEIER, Old);
      Write (R.SDEIER, Old or South);
      Read (R.GEN11_DE_HPD_IER, Old);
      Write (R.GEN11_DE_HPD_IER, Old or North);
      Read (R.SDEIMR, IMR);
      Write (R.SDEIMR, IMR and not South);
      Read (R.GEN11_DE_HPD_IMR, IMR);
      Write (R.GEN11_DE_HPD_IMR, IMR and not North);
      Write (GEN11_DISPLAY_INT_CTL, Display_Enable);
      Write (GEN11_GFX_MSTR_IRQ, Master_Enable);
   end Arm;

   procedure Quiesce (Powered : Pipe_Set; Quiet : out Boolean) is
      Value : Unsigned_32;
      procedure Zero (Offset : R.Register_Offset) is
      begin
         Write (Offset, 0);
         Read (Offset, Value);
         Quiet := Quiet and then Value = 0;
      end Zero;
   begin
      Write (GEN11_GFX_MSTR_IRQ, 0);
      Read (GEN11_GFX_MSTR_IRQ, Value);
      Quiet := (Value and Master_Enable) = 0;
      Write (GEN11_DISPLAY_INT_CTL, 0);
      Read (GEN11_DISPLAY_INT_CTL, Value);
      Quiet := Quiet and then (Value and Display_Enable) = 0;
      for Item in GT_Enable loop
         Zero (GT_Enable_Register (Item));
      end loop;
      for Pipe in R.Transcoder loop
         if Powered (Pipe) then
            Zero (Pipe_IER (Pipe));
         end if;
      end loop;
      Zero (GEN8_DE_PORT_IER);
      Zero (GEN8_DE_MISC_IER);
      Zero (GEN11_GU_MISC_IER);
      Zero (R.GEN11_DE_HPD_IER);
      Zero (R.SDEIER);
   end Quiesce;

   function Pulse_Of (Control : Unsigned_32; Item : PD.Port) return Pulse is
      Field : constant Unsigned_32 :=
        Shift_Right (Control, R.Hotplug_Field_Bits * PD.Position (Item)) and Status_Mask;
   begin
      return (if (Field and Long_Status) /= 0 then Long_Pulse
              elsif (Field and Short_Status) /= 0 then Short_Pulse else No_Pulse);
   end Pulse_Of;

   procedure Handle (Fired : out Pulses; Unexpected : out Boolean) is
      Master, Display, IIR, Control, TBT : Unsigned_32;
   begin
      Fired := [others => No_Pulse];
      --  gen11_master_intr_disable: write 0, then read what was pending.
      Write (GEN11_GFX_MSTR_IRQ, 0);
      Read (GEN11_GFX_MSTR_IRQ, Master);
      Unexpected := (Master and not (Master_Enable or Master_Display)) /= 0;
      if (Master and Master_Display) /= 0 then
         Read (GEN11_DISPLAY_INT_CTL, Display);
         Write (GEN11_DISPLAY_INT_CTL, 0);
         Unexpected := Unexpected or else
           (Display and not (Display_Enable or Display_HPD or Display_PCH)) /= 0;
         if (Display and Display_HPD) /= 0 then
            Read (R.GEN11_DE_HPD_IIR, IIR);
            if IIR /= 0 then
               Write (R.GEN11_DE_HPD_IIR, IIR);
               Read_Modify_Write (R.GEN11_TC_HOTPLUG_CTL, 0, Control);
               Read_Modify_Write (R.GEN11_TBT_HOTPLUG_CTL, 0, TBT);
               for P in PD.Type_C_Port loop
                  if (IIR and (Bit (PD.DP_Alt_Live_Bit (P)) or Bit (PD.TBT_Live_Bit (P)))) /= 0 then
                     Fired (P) := Pulse'Max (Pulse_Of (Control, P), Pulse_Of (TBT, P));
                     if Fired (P) = No_Pulse then
                        Fired (P) := Long_Pulse;
                     end if;
                  end if;
               end loop;
            end if;
         end if;
         if (Display and Display_PCH) /= 0 then
            Read (R.SDEIIR, IIR);
            if IIR /= 0 then
               Write (R.SDEIIR, IIR);
               Read_Modify_Write (R.SHOTPLUG_CTL_DDI, 0, Control);
               Read_Modify_Write (R.SHOTPLUG_CTL_TC, 0, TBT);
               for P in PD.Port loop
                  if (IIR and Bit (PD.South_Live_Bit (P))) /= 0 then
                     Fired (P) := Pulse_Of ((if P in PD.Combo_Port then Control else TBT), P);
                     if Fired (P) = No_Pulse then
                        Fired (P) := Long_Pulse;
                     end if;
                  end if;
               end loop;
            end if;
         end if;
         Write (GEN11_DISPLAY_INT_CTL, Display_Enable);
      end if;
      Write (GEN11_GFX_MSTR_IRQ, Master_Enable);
   end Handle;

   procedure Mask (Port : PD.Port) is
      IMR : Unsigned_32;
      One : constant Port_Set := [for P in PD.Port => P = Port];
   begin
      Read (R.SDEIMR, IMR);
      Write (R.SDEIMR, IMR or South_Bits (One));
      if Port in PD.Type_C_Port then
         Read (R.GEN11_DE_HPD_IMR, IMR);
         Write (R.GEN11_DE_HPD_IMR, IMR or North_Bits (One));
      end if;
   end Mask;

   procedure Unmask (Port : PD.Port) is
      IMR : Unsigned_32;
      One : constant Port_Set := [for P in PD.Port => P = Port];
   begin
      Read (R.SDEIMR, IMR);
      Write (R.SDEIMR, IMR and not South_Bits (One));
      if Port in PD.Type_C_Port then
         Read (R.GEN11_DE_HPD_IMR, IMR);
         Write (R.GEN11_DE_HPD_IMR, IMR and not North_Bits (One));
      end if;
   end Unmask;
end Intel_GPU_Hotplug_IRQ;

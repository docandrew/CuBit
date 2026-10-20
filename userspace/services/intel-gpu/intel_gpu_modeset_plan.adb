with Intel_GPU_Combo_WRPLL;
package body Intel_GPU_Modeset_Plan with SPARK_Mode is
   package W renames Intel_GPU_Combo_WRPLL;
   use type CuBit.Monitor_EDID.Sync_Kind, D.Signal_Mode, D.Bits_Per_Component;

   Buffer_Field : constant Unsigned_32 := 16#FFF#;
   Buffer_End_Shift : constant := 16;

   function Allocation_Of (Buffer_Word : Unsigned_32) return Allocation is
      First : constant Natural := Natural (Buffer_Word and Buffer_Field);
      Last : constant Natural := Natural (Shift_Right (Buffer_Word, Buffer_End_Shift) and Buffer_Field);
   begin
      if Buffer_Word = 0 or else Last < First then
         return 0;
      end if;
      return Last - First + 1;
   end Allocation_Of;

   function Level_0_Blocks (Size : Allocation) return Natural is
      Blocks : Natural;
   begin
      if Size < 3 then
         return 0;
      end if;
      Blocks := (Size - 1) * 10 / 11;
      while Blocks > 1 and then Blocks + (Blocks + 9) / 10 + 1 > Size loop
         pragma Loop_Invariant (Blocks <= Size);
         pragma Loop_Variant (Decreases => Blocks);
         Blocks := Blocks - 1;
      end loop;
      return Natural'Max (1, Blocks);
   end Level_0_Blocks;

   function Conservative_Watermarks (Buffer_Word : Unsigned_32; Stride_Bytes : Positive) return Watermarks is
      Size : constant Allocation := Allocation_Of (Buffer_Word);
      Blocks : constant Natural := Natural'Min (WM_Blocks_Max, Level_0_Blocks (Size));
      Per_Line : constant Positive := (Stride_Bytes + DDB_Block_Bytes - 1) / DDB_Block_Bytes + 1;
      Lines : constant Natural := Natural'Min (WM_Lines_Max, (Blocks + Per_Line - 1) / Per_Line);
      First_Level : constant Unsigned_32 :=
        (if Blocks = 0 then 0
         else WM_Enable or Shift_Left (Unsigned_32 (Lines), WM_Lines_Shift) or Unsigned_32 (Blocks));
   begin
      return [Level_0 => First_Level, SAGV => First_Level, SAGV_Transition => First_Level, others => 0];
   end Conservative_Watermarks;

   function Why_Not (Mode : CuBit.Monitor_EDID.Timing; Pitch : Positive) return Refusal is
     (if Natural (Mode.Clock) * 10 not in W.TMDS_kHz then No_PLL_Dividers
      elsif Pitch < Mode.Width * 4 then Pitch_Too_Small
      else None);

   --  A TMDS port's signal for the sink (HDMI or DVI); other modes kept.
   function Link_Mode (Firmware : D.Signal_Mode; HDMI_Sink : Boolean) return D.Signal_Mode is
     (if Firmware not in D.HDMI | D.DVI then Firmware elsif HDMI_Sink then D.HDMI else D.DVI);

   function Target
     (Firmware : Configuration; Mode : CuBit.Monitor_EDID.Timing; Pitch : Positive;
      Reference : D.Reference_kHz; Halved : Boolean; SCDC_Sink, HDMI_Sink : Boolean)
      return Target_Result
   is
      Clock : constant Natural := Natural (Mode.Clock) * 10;
      Scrambled : constant Boolean := Clock > 340_000;
      F : D.DDI_Function := D.Decode_Function (Firmware.Function_Word);
      Result : Configuration := Firmware;
   begin
      if Clock not in W.TMDS_kHz or else Pitch < Mode.Width * 4 or else
        not F.Port.Selected or else F.Mode = D.Unknown or else F.Depth = D.Unknown
      then
         return (Ready => False);
      end if;
      declare
         PLL : constant W.Search_Result := W.Search (Clock, Reference, Halved);
      begin
         if not PLL.Found then
            return (Ready => False);
         end if;
         Result.CFGCR0 := D.Encode_CFGCR0 (PLL.Dividers);
         Result.CFGCR1 := D.Encode_CFGCR1 (PLL.Dividers);
      end;
      Result.Timing := T.Encode (T.From_EDID (Mode));
      F.Enabled := True;
      F.Mode := Link_Mode (F.Mode, HDMI_Sink);
      if Mode.Sync.Kind = CuBit.Monitor_EDID.Digital_Separate then
         F.Hsync_High := Mode.Sync.Horizontal_Positive;
         F.Vsync_High := Mode.Sync.Vertical_Positive;
      end if;
      F.High_TMDS_Rate := Scrambled;
      F.Scrambling := Scrambled;
      Result.Function_Word := D.Encode_Function (F);
      Result.Infoframes := 0;
      Result.Plane_Stride := Unsigned_32 (Pitch / Stride_Unit);
      Result.Plane_Position := 0;
      Result.Plane_Size := Shift_Left (Unsigned_32 (Mode.Height - 1), 16) or Unsigned_32 (Mode.Width - 1);
      Result.Plane_Offset := 0;
      Result.Plane_WM := Conservative_Watermarks (Firmware.Plane_Buffer, Pitch);
      Result.Write_SCDC := SCDC_Sink;
      Result.TMDS_Config := (if Scrambled then TMDS_Ratio_1_40 or TMDS_Scrambling else 0);
      return (Ready => True, Value => Result);
   end Target;
end Intel_GPU_Modeset_Plan;

with Intel_GPU_Display_Registers;
package body Intel_GPU_HDMI_Modeset is
   package R renames Intel_GPU_Display_Registers;

   --  Deadlines (i915 uses 1 ms for PLL power/lock, 100 ms for the pipe;
   --  these allow margin and are logged when hit).
   PLL_Deadline_Us : constant := 5_000;
   Pipe_Deadline_Us : constant := 100_000;
   Buffer_Deadline_Us : constant := 10_000;
   Frame_Deadline_Us : constant := 200_000;
   --  HDMI 2.0: TMDS stays off at least 1 ms after a clock-ratio change.
   SCDC_Settle_Pauses : constant := 2;
   Frames_To_Watch : constant := 3;

   Pipe_Page_First : constant := 16#7_0000#;
   DE_Pipe_IIR_A : constant := 16#4_4408#;
   DE_Pipe_IRQ_Stride : constant := 16#10#;
   FIFO_Underrun : constant Unsigned_32 := 16#8000_0000#;

   function Name (Item : Step) return String is
     (case Item is
        when None => "none", when Plane_Off => "plane-off", when Pipe_Off => "pipe-off",
        when Function_Off => "ddi-function-off", when Sink_Scrambling_Off => "sink-scrambling-off",
        when Infoframes_Off => "infoframes-off", when Buffer_Off => "ddi-buffer-off",
        when Transcoder_Clock_Off => "transcoder-clock-off", when Port_Clock_Off => "port-clock-off",
        when PLL_Disable => "pll-disable", when PLL_Power_Off => "pll-power-off",
        when PLL_Power_On => "pll-power-on", when PLL_Configure => "pll-configure",
        when PLL_Lock => "pll-lock", when Port_Clock_Select => "port-clock-select",
        when Port_Clock_On => "port-clock-on", when Transcoder_Clock => "transcoder-clock",
        when Infoframes => "infoframes", when Timings => "timings", when Plane => "plane",
        when Function_On => "ddi-function-on", when Pipe_On => "pipe-on",
        when Sink_TMDS_Config => "sink-tmds-config", when Buffer_On => "ddi-buffer-on",
        when Frame_Counter => "frame-counter", when Scanline => "scanline",
        when Underrun => "fifo-underrun");

   function Hex (Value : Unsigned_32) return String is
      Digits_Text : constant String := "0123456789ABCDEF";
      Text : String (1 .. 8);
      Rest : Unsigned_32 := Value;
   begin
      for I in reverse Text'Range loop
         Text (I) := Digits_Text (Natural (Rest and 15) + 1);
         Rest := Shift_Right (Rest, 4);
      end loop;
      return Text;
   end Hex;

   function Pipe_Base (Item : P.Output) return Unsigned_32 is
     (Pipe_Page_First + R.Pipe_Stride * R.Transcoder'Pos (Item.Transcoder));
   function Trans_Base (Item : P.Output) return Unsigned_32 is (R.Transcoder_Base (Item.Transcoder));
   function Transconf (Item : P.Output) return Unsigned_32 is
     (R.TRANSCONF + R.Pipe_Stride * R.Transcoder'Pos (Item.Transcoder));
   function Clock_Select (Item : P.Output) return Unsigned_32 is
     (R.TRANS_CLK_SEL_A + R.Clock_Select_Stride * R.Transcoder'Pos (Item.Transcoder));
   function Pipe_IIR (Item : P.Output) return Unsigned_32 is
     (DE_Pipe_IIR_A + DE_Pipe_IRQ_Stride * R.Transcoder'Pos (Item.Transcoder));
   function PHY (Item : P.Output) return Natural is (R.DDI_Port'Pos (Item.Port));
   function Clock_Off_Bit (Item : P.Output) return Unsigned_32 is
     (Shift_Left (1, R.DPCLKA_Clock_Off_First + PHY (Item)));
   function Select_Mask (Item : P.Output) return Unsigned_32 is
     (Shift_Left (R.DPCLKA_Select_Mask, R.DPCLKA_Select_Bits * PHY (Item)));
   function Select_Value (Item : P.Output) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (R.Combo_PLL'Pos (Item.PLL)), R.DPCLKA_Select_Bits * PHY (Item)));

   function Capture (Item : P.Output) return P.Configuration is
      TB : constant Unsigned_32 := Trans_Base (Item);
      PB : constant Unsigned_32 := Pipe_Base (Item);
      Result : P.Configuration;
   begin
      Result.CFGCR0 := Read (R.DPLL_CFGCR0 (Item.PLL));
      Result.CFGCR1 := Read (R.DPLL_CFGCR1 (Item.PLL));
      Result.Timing := (H_Total => Read (TB + R.TRANS_HTOTAL), H_Blank => Read (TB + R.TRANS_HBLANK),
                        H_Sync => Read (TB + R.TRANS_HSYNC), V_Total => Read (TB + R.TRANS_VTOTAL),
                        V_Blank => Read (TB + R.TRANS_VBLANK), V_Sync => Read (TB + R.TRANS_VSYNC),
                        Source => Read (TB + R.PIPESRC));
      Result.Function_Word := Read (TB + R.TRANS_DDI_FUNC_CTL);
      Result.Transcoder_Configuration := Read (Transconf (Item));
      Result.Buffer_Control := Read (R.DDI_Buffer_Control (Item.Port));
      Result.Infoframes := Read (TB + P.VIDEO_DIP_CTL);
      Result.Plane_Control := Read (PB + P.PLANE_CTL);
      Result.Plane_Stride := Read (PB + P.PLANE_STRIDE);
      Result.Plane_Position := Read (PB + P.PLANE_POS);
      Result.Plane_Size := Read (PB + P.PLANE_SIZE);
      Result.Plane_Offset := Read (PB + P.PLANE_OFFSET);
      Result.Plane_Surface := Read (PB + P.PLANE_SURF);
      Result.Plane_Buffer := Read (PB + P.PLANE_BUF_CFG);
      for Slot in P.Watermark_Slot loop
         Result.Plane_WM (Slot) := Read (PB + P.Watermark_Offset (Slot));
      end loop;
      --  Restoring the firmware mode re-enables scrambling only if it was on.
      Result.Write_SCDC := (Result.Function_Word and R.DDI_HDMI_Scrambling) /= 0;
      Result.TMDS_Config :=
        (if Result.Write_SCDC then P.TMDS_Ratio_1_40 or P.TMDS_Scrambling else 0);
      return Result;
   end Capture;

   ---------------------------------------------------------------------------
   --  One sequence run. OK goes False at the first failed step (Failed names
   --  it); Force runs every step anyway (the disable half of a rollback).
   ---------------------------------------------------------------------------
   type Run is record
      OK : Boolean := True;
      Force : Boolean := False;
      Failed : Step := None;
   end record;

   procedure Fail (State : in out Run; At_Step : Step; Why : String) is
   begin
      Log ("intel-gpu: modeset " & Name (At_Step) & " FAILED: " & Why);
      if State.OK then
         State.OK := False;
         State.Failed := At_Step;
      end if;
   end Fail;

   function Active (State : Run) return Boolean is (State.OK or else State.Force);

   procedure Put (State : in out Run; At_Step : Step; Offset, Value : Unsigned_32) is
      Written : Boolean;
   begin
      if not Active (State) then
         return;
      end if;
      Write (Offset, Value, Written);
      if not Written then
         Fail (State, At_Step, "write refused [" & Hex (Offset) & "]");
         return;
      end if;
      Log ("intel-gpu: modeset " & Name (At_Step) & " [" & Hex (Offset) & "]=" & Hex (Value) &
           " read=" & Hex (Read (Offset)));
   end Put;

   --  Wait until (Read (Offset) and Mask) /= 0 equals Set.
   procedure Wait_For (State : in out Run; At_Step : Step; Offset, Mask : Unsigned_32;
                       Set : Boolean; Deadline_Us : Unsigned_64) is
      Started : constant Unsigned_64 := Now_Us;
      Now : Unsigned_64;
      Value : Unsigned_32;
   begin
      if not Active (State) then
         return;
      end if;
      loop
         Value := Read (Offset);
         if Value /= Unsigned_32'Last and then ((Value and Mask) /= 0) = Set then
            Log ("intel-gpu: modeset " & Name (At_Step) & " ok [" & Hex (Offset) & "]=" & Hex (Value) &
                 " after_us=" & Unsigned_64'Image (Now_Us - Started));
            return;
         end if;
         Now := Now_Us;
         if Started = Unsigned_64'Last or else Now = Unsigned_64'Last or else Now < Started or else
           Now - Started >= Deadline_Us
         then
            Fail (State, At_Step, "timeout [" & Hex (Offset) & "]=" & Hex (Value) &
                  " mask=" & Hex (Mask) & " want_set=" & Boolean'Image (Set));
            return;
         end if;
         Pause;
      end loop;
   end Wait_For;

   procedure Wait_Frames (State : in out Run; Item : P.Output; Count : Positive) is
      Offset : constant Unsigned_32 := Pipe_Base (Item) + P.PIPE_FRMCOUNT;
      First : constant Unsigned_32 := Read (Offset);
      Started : constant Unsigned_64 := Now_Us;
      Now : Unsigned_64;
   begin
      if not Active (State) then
         return;
      end if;
      loop
         exit when Read (Offset) - First >= Unsigned_32 (Count);
         Now := Now_Us;
         if Started = Unsigned_64'Last or else Now = Unsigned_64'Last or else Now < Started or else
           Now - Started >= Frame_Deadline_Us
         then
            Fail (State, Frame_Counter, "no vblank: frmcount=" & Hex (Read (Offset)) & " start=" & Hex (First));
            return;
         end if;
         Pause;
      end loop;
   end Wait_Frames;

   procedure SCDC (State : in out Run; At_Step : Step; Value : Unsigned_8) is
      Done : Boolean;
   begin
      if not Active (State) then
         return;
      end if;
      Write_TMDS_Config (Value, Done);
      Log ("intel-gpu: modeset " & Name (At_Step) & " scdc tmds_config=" & Unsigned_8'Image (Value) &
           " done=" & Boolean'Image (Done));
      if not Done then
         Fail (State, At_Step, "SCDC write not acknowledged");
      end if;
   end SCDC;

   procedure Disable (Item : P.Output; Current : P.Configuration; State : in out Run) is
      TB : constant Unsigned_32 := Trans_Base (Item);
      PB : constant Unsigned_32 := Pipe_Base (Item);
      Routing : Unsigned_32;
   begin
      Put (State, Plane_Off, PB + P.PLANE_CTL, Current.Plane_Control and not P.Plane_Enable);
      Put (State, Plane_Off, PB + P.PLANE_SURF, Current.Plane_Surface);
      Wait_Frames (State, Item, 1);
      Put (State, Pipe_Off, Transconf (Item), Current.Transcoder_Configuration and not R.Transcoder_Enable);
      Wait_For (State, Pipe_Off, Transconf (Item), R.Transcoder_State, False, Pipe_Deadline_Us);
      Put (State, Function_Off, TB + R.TRANS_DDI_FUNC_CTL,
           Current.Function_Word and not
             (R.DDI_Function_Enable or Shift_Left (R.DDI_Select_Mask, R.DDI_Select_Shift) or
              Shift_Left (R.DDI_Mode_Mask, R.DDI_Mode_Shift)));
      if Current.Write_SCDC and then Current.TMDS_Config /= 0 then
         SCDC (State, Sink_Scrambling_Off, 0);
      end if;
      Put (State, Infoframes_Off, TB + P.VIDEO_DIP_CTL, 0);
      Put (State, Buffer_Off, R.DDI_Buffer_Control (Item.Port), Current.Buffer_Control and not R.DDI_Buffer_Enable);
      Wait_For (State, Buffer_Off, R.DDI_Buffer_Control (Item.Port), R.DDI_Buffer_Idle, True, Buffer_Deadline_Us);
      Put (State, Transcoder_Clock_Off, Clock_Select (Item), 0);
      Routing := Read (R.ICL_DPCLKA_CFGCR0);
      Put (State, Port_Clock_Off, R.ICL_DPCLKA_CFGCR0, Routing or Clock_Off_Bit (Item));
      Put (State, PLL_Disable, R.DPLL_Enable (Item.PLL), Read (R.DPLL_Enable (Item.PLL)) and not R.PLL_Enable);
      Wait_For (State, PLL_Disable, R.DPLL_Enable (Item.PLL), R.PLL_Lock, False, PLL_Deadline_Us);
      Put (State, PLL_Power_Off, R.DPLL_Enable (Item.PLL), Read (R.DPLL_Enable (Item.PLL)) and not R.PLL_Power_Enable);
      Wait_For (State, PLL_Power_Off, R.DPLL_Enable (Item.PLL), R.PLL_Power_State, False, PLL_Deadline_Us);
   end Disable;

   procedure Enable (Item : P.Output; Wanted : P.Configuration; State : in out Run) is
      TB : constant Unsigned_32 := Trans_Base (Item);
      PB : constant Unsigned_32 := Pipe_Base (Item);
      Routing : Unsigned_32;
      Before, After : Unsigned_32;
   begin
      Put (State, PLL_Power_On, R.DPLL_Enable (Item.PLL), Read (R.DPLL_Enable (Item.PLL)) or R.PLL_Power_Enable);
      Wait_For (State, PLL_Power_On, R.DPLL_Enable (Item.PLL), R.PLL_Power_State, True, PLL_Deadline_Us);
      Put (State, PLL_Configure, R.DPLL_CFGCR0 (Item.PLL), Wanted.CFGCR0);
      Put (State, PLL_Configure, R.DPLL_CFGCR1 (Item.PLL), Wanted.CFGCR1);
      Put (State, PLL_Lock, R.DPLL_Enable (Item.PLL), Read (R.DPLL_Enable (Item.PLL)) or R.PLL_Enable);
      Wait_For (State, PLL_Lock, R.DPLL_Enable (Item.PLL), R.PLL_Lock, True, PLL_Deadline_Us);
      --  Select the PLL, then (a separate write) ungate the clock.
      Routing := Read (R.ICL_DPCLKA_CFGCR0);
      Put (State, Port_Clock_Select, R.ICL_DPCLKA_CFGCR0,
           (Routing and not Select_Mask (Item)) or Select_Value (Item) or Clock_Off_Bit (Item));
      Routing := Read (R.ICL_DPCLKA_CFGCR0);
      Put (State, Port_Clock_On, R.ICL_DPCLKA_CFGCR0, Routing and not Clock_Off_Bit (Item));
      Put (State, Transcoder_Clock, Clock_Select (Item),
           Shift_Left (Unsigned_32 (R.DDI_Port'Pos (Item.Port)) + 1, R.Transcoder_Clock_Shift));
      Put (State, Infoframes, TB + P.VIDEO_DIP_CTL, Wanted.Infoframes);
      Put (State, Timings, TB + R.PIPESRC, Wanted.Timing.Source);
      Put (State, Timings, TB + R.TRANS_HTOTAL, Wanted.Timing.H_Total);
      Put (State, Timings, TB + R.TRANS_HBLANK, Wanted.Timing.H_Blank);
      Put (State, Timings, TB + R.TRANS_HSYNC, Wanted.Timing.H_Sync);
      Put (State, Timings, TB + R.TRANS_VTOTAL, Wanted.Timing.V_Total);
      Put (State, Timings, TB + R.TRANS_VBLANK, Wanted.Timing.V_Blank);
      Put (State, Timings, TB + R.TRANS_VSYNC, Wanted.Timing.V_Sync);
      Put (State, Plane, PB + P.PLANE_STRIDE, Wanted.Plane_Stride);
      Put (State, Plane, PB + P.PLANE_POS, Wanted.Plane_Position);
      Put (State, Plane, PB + P.PLANE_SIZE, Wanted.Plane_Size);
      Put (State, Plane, PB + P.PLANE_OFFSET, Wanted.Plane_Offset);
      for Slot in P.Watermark_Slot loop
         Put (State, Plane, PB + P.Watermark_Offset (Slot), Wanted.Plane_WM (Slot));
      end loop;
      Put (State, Plane, PB + P.PLANE_CTL, Wanted.Plane_Control);
      Put (State, Plane, PB + P.PLANE_SURF, Wanted.Plane_Surface);
      Put (State, Function_On, TB + R.TRANS_DDI_FUNC_CTL, Wanted.Function_Word);
      --  Clear a stale FIFO underrun (write one to clear), then start the pipe.
      Put (State, Pipe_On, Pipe_IIR (Item), FIFO_Underrun);
      Put (State, Pipe_On, Transconf (Item), Wanted.Transcoder_Configuration or R.Transcoder_Enable);
      Wait_For (State, Pipe_On, Transconf (Item), R.Transcoder_State, True, Pipe_Deadline_Us);
      if Wanted.Write_SCDC then
         SCDC (State, Sink_TMDS_Config, Wanted.TMDS_Config);
         for I in 1 .. SCDC_Settle_Pauses loop
            Pause;
         end loop;
      end if;
      Put (State, Buffer_On, R.DDI_Buffer_Control (Item.Port), Wanted.Buffer_Control or R.DDI_Buffer_Enable);
      Wait_For (State, Buffer_On, R.DDI_Buffer_Control (Item.Port), R.DDI_Buffer_Idle, False, Buffer_Deadline_Us);
      Wait_Frames (State, Item, Frames_To_Watch);
      if Active (State) then
         Before := Read (PB + P.PIPEDSL);
         Pause;
         After := Read (PB + P.PIPEDSL);
         Log ("intel-gpu: modeset scanline " & Hex (Before) & " -> " & Hex (After) &
              " surflive=" & Hex (Read (PB + P.PLANE_SURFLIVE)));
         if Before = After then
            Fail (State, Scanline, "PIPEDSL not moving");
         end if;
         if (Read (Pipe_IIR (Item)) and FIFO_Underrun) /= 0 then
            Fail (State, Underrun, "pipe IIR=" & Hex (Read (Pipe_IIR (Item))));
         end if;
      end if;
   end Enable;

   procedure Change
     (Item : P.Output; Current, Wanted : P.Configuration;
      Result : out Outcome; Failed : out Step)
   is
      Forward : Run;
      Back : Run;
   begin
      Log ("intel-gpu: modeset begin transcoder " & R.Transcoder'Image (Item.Transcoder) &
           " port " & R.DDI_Port'Image (Item.Port) & " " & R.Combo_PLL'Image (Item.PLL) &
           " cfgcr0 " & Hex (Current.CFGCR0) & "->" & Hex (Wanted.CFGCR0) &
           " cfgcr1 " & Hex (Current.CFGCR1) & "->" & Hex (Wanted.CFGCR1) &
           " htotal " & Hex (Current.Timing.H_Total) & "->" & Hex (Wanted.Timing.H_Total) &
           " vtotal " & Hex (Current.Timing.V_Total) & "->" & Hex (Wanted.Timing.V_Total));
      Disable (Item, Current, Forward);
      if Forward.OK then
         Enable (Item, Wanted, Forward);
      end if;
      Failed := Forward.Failed;
      if Forward.OK then
         Result := Applied;
         Log ("intel-gpu: modeset applied");
         return;
      end if;
      Log ("intel-gpu: modeset rolling back after " & Name (Forward.Failed));
      Back.Force := True;
      Disable (Item, Wanted, Back);
      Back := (OK => True, Force => False, Failed => None);
      Enable (Item, Current, Back);
      if Back.OK then
         Result := Rolled_Back;
         Log ("intel-gpu: modeset rolled back to the firmware mode");
      else
         Result := Rollback_Failed;
         Log ("intel-gpu: modeset ROLLBACK FAILED at " & Name (Back.Failed) & " (output dark)");
      end if;
   end Change;
end Intel_GPU_HDMI_Modeset;

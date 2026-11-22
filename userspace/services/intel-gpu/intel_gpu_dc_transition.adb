package body Intel_GPU_DC_Transition is
   use Intel_GPU_DC_State;
   use type Exit_DC.Outcome;
   Attempted, Baseline_Valid : Boolean := False;
   Baseline : Snapshot := (others => 0);
   type Stage is (Admission, Baseline_Read, Baseline_Check, Control_Exit,
                  Preserve_Before_PHY, PHY_Restore, Preserve_After_PHY, Finished);
   Last_Stage : Stage := Admission;
   Last_Exit : Exit_DC.Report;
   function Diagnostic return String is
     ("stage=" & (case Last_Stage is
        when Admission => "admission", when Baseline_Read => "baseline-read",
        when Baseline_Check => "baseline-check", when Control_Exit => "control-exit",
        when Preserve_Before_PHY => "preserve-before-PHY",
        when PHY_Restore => "PHY-restore", when Preserve_After_PHY => "preserve-after-PHY",
        when Finished => "finished") & " exit=" & (case Last_Exit.Status is
        when Exit_DC.Rejected => "not-run", when Exit_DC.Invalid_MMIO => "invalid-MMIO",
        when Exit_DC.Clock_Unavailable => "clock-unavailable",
        when Exit_DC.Write_Failed => "write-failed",
        when Exit_DC.Unstable_Register => "unstable-register",
        when Exit_DC.Delay_Failed => "delay-failed",
        when Exit_DC.Restore_Failed => "restore-failed", when Exit_DC.Ready => "ready"));
   function Guarded_Read return Interfaces.Unsigned_32 is
   begin
      if not PW1_Held then return Interfaces.Unsigned_32'Last; end if;
      return Read_Control;
   end Guarded_Read;
   procedure Guarded_Write (Value : Interfaces.Unsigned_32; Success : out Boolean) is
   begin
      Success := False;
      if not PW1_Held then return; end if;
      Write_Control (Value, Success);
   end Guarded_Write;
   procedure Restore_And_Validate (Prior : Interfaces.Unsigned_32; Success : out Boolean) is
      pragma Unreferenced (Prior);
      Current : Snapshot;
      OK : Boolean;
   begin
      Success := False;
      if not Baseline_Valid or else not PW1_Held then return; end if;
      Last_Stage := Preserve_Before_PHY;
      Capture (Current, OK);
      if not OK or else Check (Baseline, Current) /= Preserved or else not PW1_Held then return; end if;
      Last_Stage := PHY_Restore;
      Restore_PHYs (OK);
      if not OK or else not PW1_Held then return; end if;
      -- PHY restoration must not silently alter the retained clocks/buffers.
      Last_Stage := Preserve_After_PHY;
      Capture (Current, OK);
      Success := OK and then Check (Baseline, Current) = Preserved and then PW1_Held;
   end Restore_And_Validate;
   procedure Execute (Authorized : Boolean; Poll_Limit : Positive; Result : out Outcome) is
      OK : Boolean;
   begin
      Result := Rejected;
      if Attempted or else not Authorized or else not PW1_Held then return; end if;
      Attempted := True;
      Last_Stage := Baseline_Read;
      Capture (Baseline, OK);
      if not OK or else not PW1_Held then Result := Baseline_Unavailable; return; end if;
      Last_Stage := Baseline_Check;
      if Check (Baseline, Baseline) /= Preserved then Result := Baseline_Unsettled; return; end if;
      Baseline_Valid := True;
      Last_Stage := Control_Exit;
      Exit_DC.Execute (True, True, Poll_Limit, Last_Exit);
      Result := (if Last_Exit.Status = Exit_DC.Ready and then PW1_Held then Ready else Transition_Failed);
      if Result = Ready then Last_Stage := Finished; end if;
   end Execute;
end Intel_GPU_DC_Transition;

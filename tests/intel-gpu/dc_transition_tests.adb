with Ada.Text_IO; with Interfaces; with Intel_GPU_DC_State;
with Intel_GPU_DC_Transition;
procedure DC_Transition_Tests is
   use Interfaces; use Intel_GPU_DC_State;
   procedure Test (Mode : Natural) is
      Control : Unsigned_32 := 16#4000000B#;
      Time : Unsigned_64 := 0;
      Captures, Writes, Restores : Natural := 0;
      Held : Boolean := True;
      function Power return Boolean is (Held);
      procedure Capture (State : out Snapshot; OK : out Boolean) is
      begin
         Captures := Captures + 1; State := (others => 0); OK := True;
         if Mode = 1 and Captures = 1 then OK := False; end if;
         if Mode = 2 and Captures = 1 then State (Buffer_0) := 16#80000000#; end if;
         if Mode = 3 and Captures = 2 then State (Clock_Control) := 1; end if;
         if Mode = 4 and Captures = 3 then State (Clock_Control) := 1; end if;
         if Mode = 6 and Captures = 1 then Held := False; end if;
      end Capture;
      function Read_Control return Unsigned_32 is (Control);
      procedure Write_Control (Value : Unsigned_32; Success : out Boolean) is
      begin
         pragma Assert (Captures >= 1); Writes := Writes + 1;
         Control := Value; Success := True;
         if Mode = 7 then Held := False; end if;
      end Write_Control;
      function Now_Us return Unsigned_64 is
      begin Time := Time + 100; return Time; end Now_Us;
      procedure Pause is begin null; end Pause;
      procedure Restore_PHYs (Success : out Boolean) is
      begin
         pragma Assert ((Control and 16#4000000B#) = 0 and Captures = 2);
         Restores := Restores + 1; Success := Mode /= 5;
      end Restore_PHYs;
      package T is new Intel_GPU_DC_Transition
        (Power, Capture, Read_Control, Write_Control, Now_Us, Pause, Restore_PHYs);
      use type T.Outcome;
      R : T.Outcome;
      Saved_Writes : Natural;
   begin
      T.Execute (False, 20, R);
      pragma Assert (R = T.Rejected and Captures = 0);
      T.Execute (True, 20, R);
      case Mode is
         when 0 => pragma Assert (R = T.Ready and Restores = 1 and Captures = 3);
         when 1 | 6 => pragma Assert (R = T.Baseline_Unavailable and Writes = 0);
         when 2 => pragma Assert (R = T.Baseline_Unsettled and Writes = 0);
         when 3 => pragma Assert (R = T.Transition_Failed and Restores = 0);
         when 4 | 5 => pragma Assert (R = T.Transition_Failed and Restores = 1);
         when others => pragma Assert (R = T.Transition_Failed and Restores = 0);
      end case;
      pragma Assert (T.Diagnostic = (case Mode is
        when 0 => "stage=finished exit=ready",
        when 1 | 6 => "stage=baseline-read exit=not-run",
        when 2 => "stage=baseline-check exit=not-run",
        when 3 => "stage=preserve-before-PHY exit=restore-failed",
        when 4 => "stage=preserve-after-PHY exit=restore-failed",
        when 5 => "stage=PHY-restore exit=restore-failed",
        when others => "stage=control-exit exit=write-failed"));
      Saved_Writes := Writes;
      T.Execute (True, 20, R);
      pragma Assert (R = T.Rejected and Writes = Saved_Writes);
   end Test;
begin
   for Mode in 0 .. 7 loop Test (Mode); end loop;
   Ada.Text_IO.Put_Line ("DC transition composition: baseline, PHY ordering, preservation, power failure PASS");
end DC_Transition_Tests;

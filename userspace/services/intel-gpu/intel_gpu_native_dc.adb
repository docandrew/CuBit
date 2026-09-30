with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_DC_State;
with Intel_GPU_DC_Transition;
with Intel_GPU_Native_DC_State;
with Intel_GPU_Native_Combo_Restore;
package body Intel_GPU_Native_DC is
   use Interfaces;
   Active, Attempted, Owner_Held, Retained : Boolean := False;
   function Held return Boolean is (Retained);
   function Available return Boolean is
     (Active and then Owner_Held and then PW1_Held and then
      Display_Pages_Ready and then PHY_Pages_Ready);
   function Read_Control return Unsigned_32 is
   begin
      if not Available then return Unsigned_32'Last; end if;
      declare
         Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (16#60045504#);
      begin return Value; end;
   end Read_Control;
   function DC_Disabled return Boolean is
      Value : constant Unsigned_32 := Read_Control;
   begin return Value /= Unsigned_32'Last and then (Value and 16#4000000B#) = 0; end;
   procedure Write_Control (Value : Unsigned_32; Success : out Boolean) is
      Prior : constant Unsigned_32 := Read_Control;
      Allowed : constant Unsigned_32 := 16#6000000B#;
   begin
      Success := False;
      -- Only clear DC enable/status bits. Reject changes to retained latch
      -- bits and any proposal to set a bit, even if an earlier sample differed.
      if Prior = Unsigned_32'Last or else Value = Unsigned_32'Last or else
        ((Prior xor Value) and not Allowed) /= 0 or else
        (Value and not Prior) /= 0 or else not Available
      then return; end if;
      declare
         Target : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (16#61400504#);
      begin Target := Value; end;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      Success := True;
      -- Exit's seven matching reads provide posted-write readback, not a
      -- substitute for PHY restoration or the CDCLK/DBUF preservation check.
   end Write_Control;
   package Reader is new Intel_GPU_Native_DC_State (Available);
   procedure Capture (State : out Intel_GPU_DC_State.Snapshot; OK : out Boolean) is
      use type Reader.Outcome;
      Sample : constant Reader.Observation := Reader.Capture (Owner_Held);
   begin State := Sample.Values; OK := Sample.Status = Reader.Collected; end;
   package PHYs is new Intel_GPU_Native_Combo_Restore
     (Available, DC_Disabled, PHY_Pages_Ready);
   function PHY_Diagnostic return String is (PHYs.Diagnostic);
   procedure Restore_PHYs (Success : out Boolean) is
      Status : constant String := PHYs.Execute (Owner_Held);
      pragma Unreferenced (Status);
   begin Success := PHYs.Last_Succeeded; end;
   procedure Pause is
   begin System.Machine_Code.Asm ("pause", Volatile => True); end;
   package Transition is new Intel_GPU_DC_Transition
     (Available, Capture, Read_Control, Write_Control, Now_Us, Pause, Restore_PHYs);
   function Execute (Owner : Boolean) return String is
      Result : Transition.Outcome;
      use type Transition.Outcome;
   begin
      if Attempted or Active then return "already-attempted"; end if;
      Attempted := True; Owner_Held := Owner; Active := True;
      Transition.Execute (Owner, 100_000, Result);
      Retained := Result = Transition.Ready;
      Active := False;
      return (case Result is
        when Transition.Rejected => "rejected",
        when Transition.Baseline_Unavailable => "baseline-unavailable",
        when Transition.Baseline_Unsettled => "baseline-unsettled",
        when Transition.Transition_Failed => "transition-failed",
        when Transition.Ready => "ready") & " " & Transition.Diagnostic;
   end Execute;
end Intel_GPU_Native_DC;

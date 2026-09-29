with Interfaces; with Intel_GPU_DC_State; with Intel_GPU_DC_Exit;
generic
   -- Caller serializes the whole transition, retains resources on failure and
   -- coordinates inherited firmware. Callbacks are bounded and nonraising.
   with function PW1_Held return Boolean;
   with procedure Capture (State : out Intel_GPU_DC_State.Snapshot; OK : out Boolean);
   with function Read_Control return Interfaces.Unsigned_32;
   with procedure Write_Control (Value : Interfaces.Unsigned_32; Success : out Boolean);
   with function Now_Us return Interfaces.Unsigned_64;
   with procedure Pause;
   with procedure Restore_PHYs (Success : out Boolean);
package Intel_GPU_DC_Transition is
   type Outcome is (Rejected, Baseline_Unavailable, Baseline_Unsettled,
                    Transition_Failed, Ready);
   procedure Execute (Authorized : Boolean; Poll_Limit : Positive; Result : out Outcome);
   -- Retained last stage and low-level exit result; no MMIO or retry.
   function Diagnostic return String;
private
   procedure Restore_And_Validate (Prior : Interfaces.Unsigned_32; Success : out Boolean);
   function Guarded_Read return Interfaces.Unsigned_32;
   procedure Guarded_Write (Value : Interfaces.Unsigned_32; Success : out Boolean);
   package Exit_DC is new Intel_GPU_DC_Exit
     (Guarded_Read, Guarded_Write, Now_Us, Pause, Restore_And_Validate);
end Intel_GPU_DC_Transition;

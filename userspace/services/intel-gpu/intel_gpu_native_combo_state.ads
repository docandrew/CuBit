with Intel_GPU_Combo_PHY;
with Interfaces;
generic
   with function Power_Held return Boolean;
package Intel_GPU_Native_Combo_State is
   type Outcome is (Rejected, Read_Failed, Changing, Collected);
   type Failure_Reason is (None, Power_Lost, Invalid_MMIO, Unstable);
   type Observation is record
      Status : Outcome := Rejected;
      Reads : Natural range 0 .. 20 := 0;
      Values : Intel_GPU_Combo_PHY.Snapshot := (others => 0);
      Reason : Failure_Reason := None;
      Field_Known : Boolean := False;
      Offset, First_Value, Second_Value : Interfaces.Unsigned_32 := 0;
      Failed_Pass : Natural range 0 .. 2 := 0;
   end record;
   function Diagnostic (Value : Observation) return String;
   -- Boot-only ADL-N: caller retains exclusive display ownership and mapped
   -- BAR0 at 60000000. Power_Held must cover this PHY's register access.
   -- Two matching samples detect change, but are NOT an atomic snapshot or
   -- proof that the PHY is ready. This adapter performs no register writes.
   function Capture (Owner : Boolean; Port : Intel_GPU_Combo_PHY.PHY)
     return Observation;
end Intel_GPU_Native_Combo_State;

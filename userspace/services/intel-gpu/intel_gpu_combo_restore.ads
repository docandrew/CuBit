with Interfaces; with Intel_GPU_Combo_PHY;
generic
   -- Nonraising bounded callbacks. Begin obtains exclusive display ownership,
   -- retained power and DC-off; End ends serialization, NOT retained power.
   -- Neither firmware nor another task may change the PHYs within this scope.
   with function Begin_Scope return Boolean;
   with procedure End_Scope;
   with function Held return Boolean;
   with procedure Read_State
     (Port : Intel_GPU_Combo_PHY.PHY;
      State : out Intel_GPU_Combo_PHY.Snapshot; Success : out Boolean);
   with procedure Write_Register
     (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
   with procedure Finish_Writes (Success : out Boolean);
package Intel_GPU_Combo_Restore is
   type Outcome is (Rejected, Read_Failed, Invalid_State, Changed,
                    Power_Lost, Write_Failed, Visibility_Failed,
                    Verification_Failed, Ready);
   type Report is record
      Status : Outcome := Rejected;
      Writes_Attempted : Natural range 0 .. 17 := 0;
      Port : Intel_GPU_Combo_PHY.PHY := Intel_GPU_Combo_PHY.A;
   end record;
   function Diagnostic (Value : Report) return String;
   -- One admitted attempt per instance, including failures. Never replay a
   -- partial sequence or release uncertain hardware resources automatically.
   procedure Execute (Authorized : Boolean; Result : out Report);
end Intel_GPU_Combo_Restore;

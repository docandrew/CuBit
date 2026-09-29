with Interfaces;
generic
   -- Bounded nonraising callbacks under exclusive display ownership and a
   -- retained PW1 reference. No other owner may enable DC during this scope.
   with function Read_Control return Interfaces.Unsigned_32;
   with procedure Write_Control (Value : Interfaces.Unsigned_32; Success : out Boolean);
   with function Now_Us return Interfaces.Unsigned_64;
   with procedure Pause;
   -- Must validate retained CDCLK/DBUF state and restore required combo-PHY
   -- state. Must not enable DC states. Not an optional/no-op success hook.
   with procedure Restore_And_Validate
     (Prior_Control : Interfaces.Unsigned_32; Success : out Boolean);
package Intel_GPU_DC_Exit is
   type Outcome is (Rejected, Invalid_MMIO, Clock_Unavailable, Write_Failed,
                    Unstable_Register, Delay_Failed, Restore_Failed, Ready);
   type Report is record
      Status : Outcome := Rejected;
      Prior, Target : Interfaces.Unsigned_32 := 0;
      Delay_Polls : Natural := 0;
   end record;
   -- ADL-N boot-only, one attempt, no rollback/release. Native callbacks and
   -- coordination with inherited DMC/PSR state are caller obligations. A MMIO
   -- write may stall in hardware; software budgets cannot preempt that stall.
   -- Now_Us returns U64'Last if unavailable. Ready retains the DC-off scope;
   -- any failure after writes must retain resources for explicit recovery.
   procedure Execute (Authorized, PW1_Held : Boolean;
                      Poll_Limit : Positive; Result : out Report);
end Intel_GPU_DC_Exit;

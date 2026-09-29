with Interfaces;
package Intel_GPU_GGTT_Mapping is
   Virtual_Base : constant Interfaces.Unsigned_64 := 16#6400_0000#;
   function Ready return Boolean;
   function Bytes return Interfaces.Unsigned_64;
   -- Trusted local lifecycle state, not caller-supplied IPC claims. Invoke
   -- only after successful native reset with ownership/forcewake retained.
   -- One attempt, including failures and partial mappings; no teardown yet.
   -- Ready means a CPU mapping only, NOT permission to reuse any GPU address.
   function Prepare
     (Owner, Reset_Complete : Boolean;
      Register_Base, Table_Bytes : Interfaces.Unsigned_64) return String;
end Intel_GPU_GGTT_Mapping;

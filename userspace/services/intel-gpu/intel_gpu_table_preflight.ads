generic
   with function Current return Boolean;
   with function Valid_Table (Ordinal : Positive) return Boolean;
package Intel_GPU_Table_Preflight is
   type Phase is (Idle, Running, Complete, Failed);
   type Controller is limited private;
   procedure Start (State : in out Controller; Count : Natural; Accepted : out Boolean);
   procedure Step (State : in out Controller);
   procedure Cancel (State : in out Controller);
   function Status (State : Controller) return Phase;
   -- Serialized retained-image owner only. Current authenticates the captured
   -- image/session/root/revision/ledger generation across every yield/callback.
   -- At most32 table callbacks per step. Completion is preflight evidence only;
   -- callers must reauthenticate exact mappings immediately before GPU writes.
private
   type Controller is limited record
      Value : Phase := Idle;
      Count, Cursor : Natural := 0;
      Busy : Boolean := False;
   end record;
end Intel_GPU_Table_Preflight;

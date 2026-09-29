with Intel_GPU_DC_State;
generic
   with function PW1_Held return Boolean;
package Intel_GPU_Native_DC_State is
   type Outcome is (Rejected, Read_Failed, Changing, Collected);
   type Observation is record
      Status : Outcome := Rejected;
      Reads : Natural range 0 .. 14 := 0;
      Values : Intel_GPU_DC_State.Snapshot := [others => 0];
   end record;
   -- Serialized boot-only ADL-N observation. Owner is retained local display
   -- ownership, not an untrusted IPC argument. No write or power transition.
   -- Matching samples detect changes but are not an atomic hardware snapshot.
   function Capture (Owner : Boolean) return Observation;
end Intel_GPU_Native_DC_State;

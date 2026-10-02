with Interfaces;
with Intel_GPU_Device_Query;
package Intel_GPU_Memory_Admission with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_16;
   use type Intel_GPU_Device_Query.Memory_Contract;
   -- Applicable only to the service's owned system-RAM arena: CPU PAT0 WB
   -- allocations and matching WB grants, GPU PPGTT PAT0 WB. NOT aperture,
   -- imported or firmware memory. Source contract: TGL PRM Vol6-5.23
   -- pp16-22; Mesa26.2.3 GFX12_PAT_ENTRIES; Linux v6.16 ADLN LLC/PAT path.
   -- Normal system-RAM effective WB type is a platform prerequisite, not
   -- something established by this Boolean predicate or by its SPARK proof.
   type Evidence is record
      Device : Interfaces.Unsigned_16 := 0;
      Runtime_Admitted : Boolean := False;
      -- Current owner predicate includes PAT/MOCS/GT/render setup, held
      -- power and reset/mapping ownership. Do not pass a historical snapshot.
      Owner_Held : Boolean := False;
      -- Authenticated caller's private render session, not device inventory.
      Session_Healthy : Boolean := False;
      Faulted : Boolean := True;
      -- Additional boot regression checks on the owned PPGTT backing.
      -- Passing these is NOT a proof of general cache coherence. GPU->CPU
      -- must compare the no-flush sample with the expected rendered content,
      -- not merely two equally stale observations.
      CPU_To_GPU_Checked : Boolean := False;
      GPU_To_CPU_Checked : Boolean := False;
   end record;
   function Live (State : Evidence) return Boolean is
     (State.Device = 16#46D2# and then State.Runtime_Admitted and then
      State.Owner_Held and then State.Session_Healthy and then not State.Faulted);
   function Policy (State : Evidence)
      return Intel_GPU_Device_Query.Memory_Contract
   with Post =>
     (if not Live (State) then Policy'Result = Intel_GPU_Device_Query.Not_Admitted
      elsif State.CPU_To_GPU_Checked and State.GPU_To_CPU_Checked then
        Policy'Result = Intel_GPU_Device_Query.Owned_WB_Coherent
      else Policy'Result = Intel_GPU_Device_Query.Owned_WB_Explicit_Maintenance);
end Intel_GPU_Memory_Admission;

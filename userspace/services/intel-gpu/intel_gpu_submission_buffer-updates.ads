with Intel_GPU_VM_Materialize;
generic
   with function Exclusive return Boolean;
   -- Caller holds submission exclusion, completed GPU flush/drain, scheduling
   -- disable acknowledgment, reset serialization and required forcewake.
   with function Flush_Page (CPU : Interfaces.Unsigned_64) return Boolean;
package Intel_GPU_Submission_Buffer.Updates is
   package Tables is new Intel_GPU_VM_Materialize (VM, Owner_Ready, Flush_Page);
   procedure Publish_Boot_Tables
     (Object : in out Buffer_State; Candidate : VM.Image;
      Backing : Tables.Mapping_View; Success : out Boolean);
   -- One bootstrap update attempt, including rejected preflight. Uses retained
   -- source/root, never replaces the context root address. Trusted caller owns
   -- all candidate mappings and retains every generation. No IPC authority,
   -- allocation, reclamation, invalidation or resume is supplied by this call.
   -- Success MUST be followed by translation invalidation before GPU resume.
   -- Failure quarantines this update path; hardware may have partial writes.
   function Failed (Object : Buffer_State) return Boolean;
end Intel_GPU_Submission_Buffer.Updates;

generic
   with function Now_Us return Unsigned_64;
   -- Trusted, serialized dispatcher evidence: all work for this session has
   -- completed and no deferred publisher can still modify its context/VM.
   -- Absence of a table VM-update hold alone does not establish this.
   with function Work_Drained (Session : Unsigned_64) return Boolean;
package Intel_GPU_Context_Table.Draining is
   type Retirement_State is
     (No_Context, Admission_Open, Pending, Disabled, Deregistered, Uncertain);
   -- Trusted dispatcher observation of context ID for Session, never
   -- authentication of a caller-supplied session. Disabled is scheduling
   -- disable, not GuC deregistration. Deregistered requires the matching GuC
   -- completion under current ownership; a finished quarantine is NOT
   -- success. No_Context only means this table has no record of Session at
   -- ID, not absence of pending allocation or registration elsewhere.
   -- Deregistered is what Release requires before the ID is reused.
   function Observe (Object : Table; ID : Unsigned_32; Session : Unsigned_64)
      return Retirement_State;
   -- One bounded pass; caller continues dispatching CT events between ticks.
   -- After disable, deregister only with trusted drain evidence and no hold.
   -- Disable and deregister share one fixed deadline/poll budget.
   -- No memory/ID reclamation, including on deregister completion. Frozen/bad
   -- clocks, timeout and uncertain sends quarantine, never retry publication.
   procedure Tick (Object : in out Table; New_Fault : out Boolean);
end Intel_GPU_Context_Table.Draining;

with Interfaces; use Interfaces;
generic
   -- One VM, serialized event loop. Owner_Ready must include permanent session
   -- retirement/device loss and held reset/invalidation serialization. Every
   -- submission entrypoint must use Can_Submit, including during callbacks.
   -- Callbacks may pump events but must not start another update or bypass
   -- admission. They are bounded/nonraising and retain backing on failure.
   with function Owner_Ready return Boolean;
   with procedure Drain (Success : out Boolean);
   -- Drain: finish prior work + GPU flush barrier, acknowledge scheduling
   -- disable for ALL contexts using this VM, retain exclusion/forcewake.
   with procedure Publish (Success : out Boolean);
   -- Publish: validate candidate generation/backing, materialize new children,
   -- then update stable-root entries with required CPU visibility/readback.
   with procedure Invalidate (Success : out Boolean);
   -- Invalidate: actual bounded hardware invalidation, not IPC acceptance.
   with procedure Resume (Success : out Boolean);
   -- Resume: restore the intended scheduling state. A running context needs
   -- acknowledged enable, not merely a queued request. A submit-on-demand
   -- context may remain acknowledged disabled. In both cases retain software
   -- exclusion until the committed image is adopted by the caller.
package Intel_GPU_VM_Update is
   type Phase is (Idle, Draining, Publishing, Invalidating, Resuming, Quarantined);
   type Result is (Rejected, Complete, Ownership_Lost, Drain_Failed,
                   Publication_Failed, Invalidation_Failed, Resume_Failed);
   type State is limited private;
   function Current_Phase (Object : State) return Phase;
   function Generation (Object : State) return Unsigned_64;
   function Can_Submit (Object : State) return Boolean;
   procedure Execute (Object : in out State; Expected_Generation : Unsigned_64;
                      Status : out Result);
   -- A stale generation or nested attempt is rejected without callbacks.
   -- Once drain starts, any failure permanently closes admission. No rollback
   -- or page reclamation is inferred. Owner retains both generations and must
   -- handle cleanup/reset separately, including uncertain resume outcomes.
   procedure Fail (Object : in out State);
private
   type State is limited record
      Value : Phase := Idle;
      Epoch : Unsigned_64 := 0;
   end record;
end Intel_GPU_VM_Update;

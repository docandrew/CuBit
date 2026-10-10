with Interfaces;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
generic
   -- Serialized, bounded, nonraising callbacks. Exclusive channel ownership;
   -- caller assigns fast-request wire IDs and reserves four G2H words.
   -- Queue receives no context-local transaction identity.
   with function Owner_Ready return Boolean;
   with procedure Queue
     (Payload : Intel_GPU_GuC_Context_Event.Words;
      Result : out Intel_GPU_GuC_Context_Lifecycle.Send_Result);
   with procedure Retain
     (Payload : Intel_GPU_GuC_Context_Event.Words;
      Fence : Interfaces.Unsigned_16; Success : out Boolean);
package Intel_GPU_GuC_Context_Session is
   type Session is limited private;
   type Result is (Rejected, Backpressure, Queued, Handled, Retained, Faulted);
   function State (Object : Session) return Intel_GPU_GuC_Context_Lifecycle.Phase;
   function Can_Run_And_Retire (Object : Session) return Boolean;
   -- Resting Enabled (GuC-resident) or Disabled with nothing in flight.
   function Can_Submit (Object : Session) return Boolean;
   procedure Initialize
     (Object : in out Session; ID : Interfaces.Unsigned_32;
      GPU_Start, Pin_Bias : Interfaces.Unsigned_64;
      Quantum_Us, Preemption_Us : Interfaces.Unsigned_32;
      Preempt_To_Idle : Boolean);
   procedure Submit (Object : in out Session;
                     Action : Intel_GPU_GuC_Context_Lifecycle.Operation;
                     Status : out Result);
   -- Notify already-enabled GuC after publishing new single-LRC work.
   -- Tail_Published is a trusted local publication result, not an IPC grant.
   -- Retain batch/ring backing until a separate GPU completion is observed.
   procedure Notify_Work (Object : in out Session; Tail_Published : Boolean;
                          Status : out Result);
   -- Trusted dispatcher facts, never client-supplied flags. Caller has closed
   -- admission and drained every in-flight work reference before this call.
   -- Its channel reservation must cover CT+HXG+ID (three DWORDs); the session's
   -- existing four-word exclusive control allowance is sufficient. Keep the
   -- context and all backing until completion and later retirement gates.
   procedure Deregister
     (Object : in out Session; Admission_Closed, Work_Drained : Boolean;
      Status : out Result);
   -- Owned, CT-validated frame only. Unrelated valid messages are retained;
   -- overflow, malformed messages and delivered failures quarantine the session.
   -- The transport dispatcher quarantines all sessions on fast-request failure.
   procedure Dispatch (Object : in out Session;
                       Payload : Intel_GPU_GuC_Context_Event.Words;
                       Fence : Interfaces.Unsigned_16; Status : out Result);
   -- Caller must impose a fixed monotonic deadline while pending, invoke Fail
   -- on timeout/transport loss, and keep servicing late errors while enabled.
   -- This nonblocking session never polls, frees backing or proves completion.
   procedure Fail (Object : in out Session);
private
   type Session is limited record
      Life : Intel_GPU_GuC_Context_Lifecycle.Context;
      ID, Quantum, Preemption : Interfaces.Unsigned_32 := 0;
      GPU, Bias : Interfaces.Unsigned_64 := 0;
      Forced : Boolean := False;
   end record;
end Intel_GPU_GuC_Context_Session;

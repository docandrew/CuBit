with Interfaces;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
generic
   -- Serialized, bounded, nonraising callbacks. Exclusive channel ownership;
   -- caller reserves four unique fences and four G2H words for this lifetime.
   with function Owner_Ready return Boolean;
   with procedure Queue
     (Payload : Intel_GPU_GuC_Context_Event.Words;
      Fence : Interfaces.Unsigned_16;
      Result : out Intel_GPU_GuC_Context_Lifecycle.Send_Result);
   with procedure Retain
     (Payload : Intel_GPU_GuC_Context_Event.Words;
      Fence : Interfaces.Unsigned_16; Success : out Boolean);
package Intel_GPU_GuC_Context_Session is
   type Session is limited private;
   type Result is (Rejected, Backpressure, Queued, Handled, Retained, Faulted);
   function State (Object : Session) return Intel_GPU_GuC_Context_Lifecycle.Phase;
   procedure Initialize
     (Object : in out Session; ID : Interfaces.Unsigned_32;
      GPU_Start, Pin_Bias : Interfaces.Unsigned_64;
      Fence_Base : Interfaces.Unsigned_16;
      Quantum_Us, Preemption_Us : Interfaces.Unsigned_32;
      Preempt_To_Idle : Boolean);
   procedure Submit (Object : in out Session;
                     Action : Intel_GPU_GuC_Context_Lifecycle.Operation;
                     Status : out Result);
   -- Owned, CT-validated frame only. Unrelated valid messages are retained;
   -- overflow, malformed messages and matching failures quarantine the session.
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

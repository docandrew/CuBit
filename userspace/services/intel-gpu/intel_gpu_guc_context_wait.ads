with Interfaces;
with Intel_GPU_GuC_Context_Session;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_CT_Receive;
with Intel_GPU_GuC_Context_Event;
generic
   with package Driver is new Intel_GPU_GuC_Context_Session (<>);
   with package Receiver is new Intel_GPU_GuC_CT_Receive (<>);
   with function Owner_Ready return Boolean;
   with procedure Poll (Item : out Receiver.Message; Status : out Receiver.Result);
   with function Now_Us return Interfaces.Unsigned_64;
   with procedure Pause;
   -- Shared-channel dispatch must service other contexts while this one waits.
   -- The callback must not treat another context's acknowledgment as Object's.
   with procedure Dispatch
     (Object : in out Driver.Session;
      Payload : Intel_GPU_GuC_Context_Event.Words;
      Fence : Interfaces.Unsigned_16; Status : out Driver.Result);
package Intel_GPU_GuC_Context_Wait is
   type Result is (Rejected, Complete, Ownership_Lost, Invalid_Clock,
                   Timed_Out, Send_Failed, Receive_Failed, Context_Failed);
   -- Serialized, bounded, nonraising callbacks. Only Enable/Disable, from
   -- their prerequisite states. Retries only proven nonpublished backpressure.
   -- Fixed one-second deadline includes callbacks; Poll_Limit bounds a frozen
   -- clock. Failure quarantines and retains all context/transport backing.
   -- Complete means scheduling acknowledged, NOT a completed GPU workload.
   procedure Execute
     (Object : in out Driver.Session;
      Action : Intel_GPU_GuC_Context_Lifecycle.Operation;
      Poll_Limit : Positive; Status : out Result);
end Intel_GPU_GuC_Context_Wait;

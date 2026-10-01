with Interfaces;
with Intel_GPU_GuC_Context_Session;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_CT_Receive;
with Intel_GPU_GuC_Context_Event;
generic
   with package Driver is new Intel_GPU_GuC_Context_Session (<>);
   with package Receiver is new Intel_GPU_GuC_CT_Receive (<>);
   type Context_Type is limited private;
   with function State (Object : Context_Type) return Intel_GPU_GuC_Context_Lifecycle.Phase;
   with procedure Submit (Object : in out Context_Type;
     Action : Intel_GPU_GuC_Context_Lifecycle.Operation; Status : out Driver.Result);
   with procedure Fail_Context (Object : in out Context_Type);
   with function Owner_Ready return Boolean;
   with procedure Poll (Item : out Receiver.Message; Status : out Receiver.Result);
   with function Now_Us return Interfaces.Unsigned_64;
   with procedure Pause;
   with procedure Dispatch (Object : in out Context_Type;
     Payload : Intel_GPU_GuC_Context_Event.Words;
     Fence : Interfaces.Unsigned_16; Status : out Driver.Result);
package Intel_GPU_Context_Wait_Core is
   type Result is (Rejected, Complete, Ownership_Lost, Invalid_Clock,
                   Timed_Out, Send_Failed, Receive_Failed, Context_Failed);
   -- Serialized callbacks; context storage may be a session or owning table.
   -- Dispatch services the entire channel; State must report only the target.
   -- One-second deadline plus poll budget, not GPU workload completion.
   procedure Execute (Object : in out Context_Type;
     Action : Intel_GPU_GuC_Context_Lifecycle.Operation;
     Poll_Limit : Positive; Status : out Result);
end Intel_GPU_Context_Wait_Core;

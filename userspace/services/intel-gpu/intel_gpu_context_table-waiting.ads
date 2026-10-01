with Intel_GPU_GuC_CT_Receive;
generic
   with package Receiver is new Intel_GPU_GuC_CT_Receive (<>);
   with procedure Poll (Item : out Receiver.Message; Status : out Receiver.Result);
   with function Now_Us return Unsigned_64;
   with procedure Pause;
package Intel_GPU_Context_Table.Waiting is
   type Result is (Rejected, Complete, Ownership_Lost, Invalid_Clock,
                   Timed_Out, Send_Failed, Receive_Failed, Context_Failed);
   -- Serialized shared-channel wait. Dispatches every received context event,
   -- but only the selected context's state completes this operation. A wait
   -- failure quarantines the table; unrelated context faults stay isolated.
   procedure Execute (Object : in out Table; ID : Unsigned_32;
     Action : Intel_GPU_GuC_Context_Lifecycle.Operation;
     Poll_Limit : Positive; Status : out Result);
end Intel_GPU_Context_Table.Waiting;

with Intel_GPU_Context_Wait_Core;
package body Intel_GPU_Context_Table.Waiting is
   procedure Execute (Object : in out Table; ID : Unsigned_32;
     Action : Intel_GPU_GuC_Context_Lifecycle.Operation;
     Poll_Limit : Positive; Status : out Result) is
      function Target_State (Item : Table) return Intel_GPU_GuC_Context_Lifecycle.Phase is
        (State (Item, ID));
      procedure Target_Submit (Item : in out Table;
        Operation : Intel_GPU_GuC_Context_Lifecycle.Operation; Outcome : out Driver.Result) is
      begin Submit (Item, ID, Operation, Outcome); end Target_Submit;
      procedure Dispatch_All (Item : in out Table;
        Payload : Intel_GPU_GuC_Context_Event.Words;
        Fence : Unsigned_16; Outcome : out Driver.Result) is
         Delivered_ID : Unsigned_32;
         Delivery : Dispatch_Result;
      begin
         Dispatch (Item, Payload, Fence, Delivered_ID, Delivery);
         Outcome := (case Delivery is
           when Delivered => Driver.Handled,
           when Retained => Driver.Retained,
           when Context_Fault =>
             (if Delivered_ID = ID then Driver.Faulted else Driver.Handled),
           when Transport_Fault => Driver.Faulted);
      end Dispatch_All;
      package Core is new Intel_GPU_Context_Wait_Core
        (Driver, Receiver, Table, Target_State, Target_Submit, Fail,
         Owner_Ready, Poll, Now_Us, Pause, Dispatch_All);
      Outcome : Core.Result;
   begin
      Core.Execute (Object, Action, Poll_Limit, Outcome);
      Status := (case Outcome is
        when Core.Rejected => Rejected, when Core.Complete => Complete,
        when Core.Ownership_Lost => Ownership_Lost,
        when Core.Invalid_Clock => Invalid_Clock, when Core.Timed_Out => Timed_Out,
        when Core.Send_Failed => Send_Failed, when Core.Receive_Failed => Receive_Failed,
        when Core.Context_Failed => Context_Failed);
   end Execute;
end Intel_GPU_Context_Table.Waiting;

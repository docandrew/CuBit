with Intel_GPU_Context_Wait_Core;
package body Intel_GPU_GuC_Context_Wait is
   package Core is new Intel_GPU_Context_Wait_Core
     (Driver, Receiver, Driver.Session, Driver.State, Driver.Submit, Driver.Fail,
      Owner_Ready, Poll, Now_Us, Pause, Dispatch);
   procedure Execute
     (Object : in out Driver.Session;
      Action : Intel_GPU_GuC_Context_Lifecycle.Operation;
      Poll_Limit : Positive; Status : out Result) is
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
end Intel_GPU_GuC_Context_Wait;

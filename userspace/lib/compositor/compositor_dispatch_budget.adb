package body Compositor_Dispatch_Budget with SPARK_Mode is
   function New_Input return Input_Batch is ((others => <>));
   procedure Begin_Input_Phase (S : in out Input_Batch; Now : Tick) is
   begin
      S.Start := Now;
      S.Charged := False;
      S.Opened := S.Opened + 1;
   end Begin_Input_Phase;
   function Can_Input (S : Input_Batch; Now : Tick) return Boolean is
     (S.Opened > 0 and then S.Used < Event_Limit and then
      (not S.Charged or else Within (S.Start, Now, Input_Phase_Us)));
   procedure Charge_Input (S : in out Input_Batch) is
   begin
      S.Used := S.Used + 1;
      S.Charged := True;
   end Charge_Input;
   function New_Requests (Now : Tick) return Request_Batch is ((0, Now));
   function Can_Request
     (S : Request_Batch; Now : Tick; Frame_Pending : Boolean) return Boolean is
     (S.Used < (if Frame_Pending then Frame_Request_Limit else Idle_Request_Limit)
      and then (S.Used = 0 or else Within (S.Start, Now, Request_Phase_Us)));
   procedure Charge_Request (S : in out Request_Batch) is
   begin
      S.Used := S.Used + 1;
   end Charge_Request;
   function New_Completions (Now : Tick) return Completion_Batch is ((0, Now));
   function Can_Complete (S : Completion_Batch; Now : Tick) return Boolean is
     (S.Used < Completion_Limit and then
      (S.Used = 0 or else Within (S.Start, Now, Completion_Phase_Us)));
   procedure Charge_Completion (S : in out Completion_Batch) is
   begin
      S.Used := S.Used + 1;
   end Charge_Completion;
end Compositor_Dispatch_Budget;

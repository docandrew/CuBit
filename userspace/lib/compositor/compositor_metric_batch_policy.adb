package body Compositor_Metric_Batch_Policy with SPARK_Mode is
   procedure Accepted (S : in out State; Now : Tick) is
   begin
      S := (S.Records + 1, (if S.Records = 0 then Now else S.Started), S.Urgent);
   end Accepted;
   procedure Accepted_Group (S : in out State) is
   begin
      S.Records := S.Records + 4;
   end Accepted_Group;
   procedure Request_Flush (S : in out State) is
   begin
      S.Urgent := True;
   end Request_Flush;
   procedure Submitted (S : out State) is
   begin
      S := (0, Compositor_Elapsed.Unavailable, False);
   end Submitted;
end Compositor_Metric_Batch_Policy;

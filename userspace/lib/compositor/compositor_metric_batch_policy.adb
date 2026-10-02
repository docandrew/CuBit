package body Compositor_Metric_Batch_Policy with SPARK_Mode is
   procedure Accepted (S : in out State; Now : Tick) is
   begin
      S := (S.Records + 1, (if S.Records = 0 then Now else S.Started));
   end Accepted;
   procedure Submitted (S : out State) is
   begin
      S := (0, Compositor_Elapsed.Unavailable);
   end Submitted;
end Compositor_Metric_Batch_Policy;

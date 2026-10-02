package body Compositor_Stage_Metrics with SPARK_Mode is
   function Declaration (Item : Stage) return Records.Metric_Record is
      Name : constant Records.Metric_Name :=
        (case Item is
         when Input_Dispatch =>
           (Bytes => [100, 101, 115, 107, 116, 111, 112, 46, 105, 110, 112, 117, 116, 95, 100, 105, 115, 112, 97, 116, 99, 104, others => 0], Length => 22),
         when Request_Dispatch =>
           (Bytes => [100, 101, 115, 107, 116, 111, 112, 46, 114, 101, 113, 117, 101, 115, 116, 95, 100, 105, 115, 112, 97, 116, 99, 104, others => 0], Length => 24),
         when Scene_Draw =>
           (Bytes => [100, 101, 115, 107, 116, 111, 112, 46, 115, 99, 101, 110, 101, 95, 100, 114, 97, 119, others => 0], Length => 18),
         when Submit_Call =>
           (Bytes => [100, 101, 115, 107, 116, 111, 112, 46, 115, 117, 98, 109, 105, 116, 95, 99, 97, 108, 108, others => 0], Length => 19));
   begin
      return (Records.Describe, Key (Item), Records.Latency, Records.Microseconds, Name);
   end Declaration;
   function Prepare (Item : Stage; First, Last : Tick) return Sample is
      Duration : constant Compositor_Elapsed.Sample := Compositor_Elapsed.Measure (First, Last);
   begin
      if not Duration.Valid then return (Valid => False); end if;
      return (True, (Records.Latency, Key (Item), Last, Duration.Microseconds, 0));
   end Prepare;
end Compositor_Stage_Metrics;

package body Compositor_Work_Metrics with SPARK_Mode is
   function Declaration (Item : Work_Kind) return R.Metric_Record is
      Name : constant R.Metric_Name :=
        (case Item is
         when Scene_Pixels => (Bytes => [100, 101, 115, 107, 116, 111, 112, 46, 115, 99, 101, 110, 101, 95, 112, 105, 120, 101, 108, 115, others => 0], Length => 20),
         when Repair_Pixels => (Bytes => [100, 101, 115, 107, 116, 111, 112, 46, 114, 101, 112, 97, 105, 114, 95, 112, 105, 120, 101, 108, 115, others => 0], Length => 21));
   begin
      return (R.Describe, Key (Item), R.Counter, R.Count, Name);
   end Declaration;
   function Prepare (Item : Work_Kind; Pixels, Now : Tick) return Sample is
   begin
      if Now = Compositor_Elapsed.Unavailable then return (Valid => False); end if;
      return (True, (R.Counter, Key (Item), Now, Pixels, 0));
   end Prepare;
end Compositor_Work_Metrics;

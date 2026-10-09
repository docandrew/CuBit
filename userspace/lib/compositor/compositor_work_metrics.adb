package body Compositor_Work_Metrics with SPARK_Mode is
   function Declaration (Item : Work_Kind) return R.Metric_Record is
      Name : constant R.Metric_Name :=
        (case Item is
         when Scene_Pixels => (Bytes => [100, 101, 115, 107, 116, 111, 112, 46, 115, 99, 101, 110, 101, 95, 112, 105, 120, 101, 108, 115, others => 0], Length => 20),
         when Repair_Pixels => (Bytes => [100, 101, 115, 107, 116, 111, 112, 46, 114, 101, 112, 97, 105, 114, 95, 112, 105, 120, 101, 108, 115, others => 0], Length => 21),
         when GPU_Readback_Bytes => (Bytes => [100, 101, 115, 107, 116, 111, 112, 46, 103, 112, 117, 95, 114, 101, 97, 100, 98, 97, 99, 107, 95, 98, 121, 116, 101, 115, others => 0], Length => 26),
         when CPU_Copy_Bytes => (Bytes => [100, 101, 115, 107, 116, 111, 112, 46, 99, 112, 117, 95, 99, 111, 112, 121, 95, 98, 121, 116, 101, 115, others => 0], Length => 22));
   begin
      return (R.Describe, Key (Item), R.Counter, (if Item in GPU_Readback_Bytes | CPU_Copy_Bytes then R.Bytes else R.Count), Name);
   end Declaration;
   function Prepare (Item : Work_Kind; Amount, Now : Tick) return Sample is
   begin
      if Now = Compositor_Elapsed.Unavailable then return (Valid => False); end if;
      return (True, (R.Counter, Key (Item), Now, Amount, 0));
   end Prepare;
end Compositor_Work_Metrics;

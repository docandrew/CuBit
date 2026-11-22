package body Intel_GPU_ADS_Policies with SPARK_Mode is
   function Encode (Allow_Engine_Reset : Boolean) return Policy_Bytes is
      Result : Policy_Bytes := [others => 0];
   begin
      -- submission_queue_depth[16] and reserved[4] remain zero, as in
      -- upstream's initially zeroed allocation. DPC promote time = 500000us.
      Result (64) := 16#20#;
      Result (65) := 16#A1#;
      Result (66) := 16#07#;
      Result (68) := 1; -- is_valid, not publication synchronization
      Result (72) := 15; -- max_num_work_items
      Result (76) := (if Allow_Engine_Reset then 0 else 1);
      return Result;
   end Encode;
end Intel_GPU_ADS_Policies;

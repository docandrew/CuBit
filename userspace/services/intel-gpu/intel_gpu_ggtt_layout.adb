package body Intel_GPU_GGTT_Layout with SPARK_Mode is
   function Plan (Table_Bytes, Pin_Bias : Unsigned_64) return Layout is
      Total, First, Split : Unsigned_64;
   begin
      if Table_Bytes not in 2_097_152 | 4_194_304 | 8_388_608 then
         return (others => <>);
      end if;
      Total := Table_Bytes / 8 * 4096;
      Split := Total - Upload_Reservation_Bytes;
      if Pin_Bias >= Split then return (others => <>); end if;
      First := Unsigned_64'Max (4096, (Pin_Bias + 4095) / 4096 * 4096);
      if First >= Split then return (others => <>); end if;
      return (True, Total, First, Split, Split, Total - 4096, Total - 4096);
   end Plan;
end Intel_GPU_GGTT_Layout;

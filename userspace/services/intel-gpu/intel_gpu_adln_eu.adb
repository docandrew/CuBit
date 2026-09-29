package body Intel_GPU_ADLN_EU with SPARK_Mode is
   function Decode (Slice_Enable, DSS_Enable, EU_Disable : Unsigned_32)
     return Topology is
      Result : Topology;
      DSS_Count : Natural range 0 .. 6 := 0;
      EU_Count : Natural range 0 .. 16 := 0;
   begin
      if Slice_Enable = Unsigned_32'Last or DSS_Enable = Unsigned_32'Last or
        EU_Disable = Unsigned_32'Last or (Slice_Enable and 255) /= 1
      then return Result; end if;
      Result.DSS_Mask := Unsigned_8 (DSS_Enable and 63);
      for I in Natural range 0 .. 5 loop
         pragma Loop_Invariant (DSS_Count <= I);
         if (Result.DSS_Mask and Shift_Left (Unsigned_8'(1), I)) /= 0 then
            DSS_Count := DSS_Count + 1;
         end if;
      end loop;
      -- Linux v6.16 intel_sseu.c gen12_sseu_info_init: same pair mask
      -- applies to each enabled DSS on this single-slice platform.
      for I in Natural range 0 .. 7 loop
         pragma Loop_Invariant (EU_Count <= 2 * I);
         if (EU_Disable and Shift_Left (Unsigned_32'(1), I)) = 0 then
            Result.EU_Mask := Result.EU_Mask or
              Shift_Left (Unsigned_16'(3), 2 * I);
            EU_Count := EU_Count + 2;
         end if;
      end loop;
      Result.Total_EUs := DSS_Count * EU_Count;
      Result.Valid := Result.DSS_Mask /= 0 and Result.EU_Mask /= 0 and
        Result.Total_EUs > 0;
      return Result;
   end Decode;
end Intel_GPU_ADLN_EU;

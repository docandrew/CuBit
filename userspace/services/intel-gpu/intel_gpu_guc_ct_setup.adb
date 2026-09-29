package body Intel_GPU_GuC_CT_Setup with SPARK_Mode is
   function Prepare (GPU_Start, Backing_Bytes, Pin_Bias : Unsigned_64) return Plan is
      Limit : constant Unsigned_64 := 16#FEE00000#;
      Result : Plan;
      function Address_Request (Key : Unsigned_32; Offset : Unsigned_64) return Request
        with Pre => Key <= 65535 and then Offset <= 12288 and then
          GPU_Start <= Limit - Required_Bytes;
      function Address_Request (Key : Unsigned_32; Offset : Unsigned_64) return Request is
        (Length => 4, Data => [16#0508#, Key * 65536 + 2,
                              Unsigned_32 (GPU_Start + Offset), 0]);
      function Size_Request (Key, Bytes : Unsigned_32) return Request is
        -- SELF_CFG has a fixed four-word envelope even for a one-word KLV.
        -- KLV_LEN counts value words, not the request envelope length.
        (Length => 4, Data => [16#0508#, Key * 65536 + 1, Bytes, 0]);
   begin
      if Pin_Bias = 0 or else Pin_Bias mod 4096 /= 0 or else
        GPU_Start < Pin_Bias or else GPU_Start >= Limit or else
        GPU_Start mod 4096 /= 0 or else Backing_Bytes < Required_Bytes or else
        Backing_Bytes mod 4096 /= 0 or else Backing_Bytes > Limit - GPU_Start
      then return Result; end if;
      -- i915 registers receive first, each descriptor then ring then size.
      Result.Register_Buffers :=
        [Address_Request (16#0906#, 4096), Address_Request (16#0905#, 12288),
         Size_Request (16#0907#, 16384), Address_Request (16#0903#, 0),
         Address_Request (16#0902#, 8192), Size_Request (16#0904#, 4096)];
      Result.Enable := (Length => 2, Data => [16#4509#, 1, 0, 0]);
      Result.Valid := True;
      return Result;
   end Prepare;
end Intel_GPU_GuC_CT_Setup;

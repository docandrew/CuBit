package body Intel_GPU_ADLN_LRC_Initial with SPARK_Mode is
   function Build (Ring_GPU, Root_DMA : Unsigned_64;
                   Ring_Log2 : Ring_Size_Log2) return Initial_State is
      Result : Initial_State;
   begin
      if not Admissible (Ring_GPU, Root_DMA, Ring_Log2) then return Result; end if;
      Result.Registers := Intel_GPU_ADLN_LRC_Template.Build;
      -- Context control: inhibit synchronous switch + first engine restore.
      Result.Registers (3) := 16#0009_0009#;
      Result.Registers (5) := 0; -- ring head
      Result.Registers (7) := 0; -- ring tail
      Result.Registers (9) := Unsigned_32 (Ring_GPU);
      Result.Registers (11) := (Shift_Left (Unsigned_32'(1), Ring_Log2) - 4096) or 1;
      Result.Registers (35) := 0; -- initial timestamp
      Result.Registers (49) := 0; -- root high; initial DMA policy below4GiB
      Result.Registers (51) := Unsigned_32 (Root_DMA);
      -- Gen12 single-slice RPCS: enable, slice-count enable, count1.
      Result.Registers (67) := 16#8004_1000#;
      -- MI_MODE: write-mask bit8, value bit8 clear (STOP_RING).
      Result.Registers (97) := 16#0100_0000#;
      Result.Prepared := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_LRC_Initial;

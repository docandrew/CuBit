package body Intel_GPU_ADLN_Steering with SPARK_Mode is
   function Decode (Slice_Enable, DSS_Enable, L3_Disable : Unsigned_32)
     return Topology is
      Result : Topology;
   begin
      if Slice_Enable = Unsigned_32'Last or DSS_Enable = Unsigned_32'Last or
        L3_Disable = Unsigned_32'Last or (Slice_Enable and 255) /= 1
      then
         return Result;
      end if;
      Result.DSS_Mask := Unsigned_8 (DSS_Enable and 63);
      Result.L3_Mask := Unsigned_8 ((not L3_Disable) and 15);
      if Result.DSS_Mask = 0 or Result.L3_Mask = 0 then return Result; end if;
      -- Lowest enabled DSS is mandatory for the render-power-gated minconfig.
      for I in 0 .. 5 loop
         if (Result.DSS_Mask and Shift_Left (Unsigned_8'(1), I)) /= 0 then
            Result.Default_Instance := I;
            exit;
         end if;
      end loop;
      for I in 0 .. 3 loop
         if (Result.L3_Mask and Shift_Left (Unsigned_8'(1), I)) /= 0 then
            Result.L3_Instance := I;
            exit;
         end if;
      end loop;
      Result.Separate_L3 :=
        (Result.L3_Mask and Shift_Left (Unsigned_8'(1), Result.Default_Instance)) = 0;
      Result.Valid := True;
      return Result;
   end Decode;
   function Decode_Stable (First, Second : Fuse_Snapshot) return Topology is
   begin
      if First /= Second then return (others => <>); end if;
      return Decode (First.Slice_Enable, First.DSS_Enable, First.L3_Disable);
   end Decode_Stable;
   function Instance (Value : Topology; Offset : Unsigned_32) return Natural is
     (if Value.Separate_L3 and Offset in 16#B100# .. 16#B3FF#
      then Value.L3_Instance else Value.Default_Instance);
end Intel_GPU_ADLN_Steering;

package body Intel_GPU_ADS_Layout with SPARK_Mode is
   function Plan (Register_Bytes, Golden_Bytes, Workaround_Bytes,
                  Capture_Bytes, Private_Bytes, Backing : Unsigned_64) return Layout
   is
      Value : Layout;
      Cursor, Padding : Unsigned_64 := 0;
   begin
      if Register_Bytes mod 16 /= 0 or else Workaround_Bytes mod 4 /= 0 then
         return (others => <>);
      end if;
      -- 16 classes * 32 instances; packed ABI, no native record padding.
      Value.Bytes := [4572, 96, 640, 16384, Register_Bytes, Golden_Bytes,
                      Workaround_Bytes, Capture_Bytes, Private_Bytes];
      for S in Section loop
         pragma Loop_Invariant (Cursor <= Limit);
         if S >= Golden_Contexts then
            Padding := (4096 - Cursor mod 4096) mod 4096;
            if Padding > Limit - Cursor then return (others => <>); end if;
            Cursor := Cursor + Padding;
         end if;
         if Value.Bytes (S) > Limit - Cursor then return (others => <>); end if;
         Value.Offset (S) := Cursor;
         Cursor := Cursor + Value.Bytes (S);
      end loop;
      Padding := (4096 - Cursor mod 4096) mod 4096;
      if Padding > Limit - Cursor then return (others => <>); end if;
      Value.Total := Cursor + Padding;
      if Value.Total > Backing or else not Sound (Value) then
         return (others => <>);
      end if;
      Value.Valid := True;
      return Value;
   end Plan;
end Intel_GPU_ADS_Layout;

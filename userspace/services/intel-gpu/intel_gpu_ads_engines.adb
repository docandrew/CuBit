package body Intel_GPU_ADS_Engines with SPARK_Mode is
   use Intel_GPU_ADLN_Inventory;
   function Encode (Description : Inventory) return Encoding is
      Result : Encoding;
      Video_Index : Natural range 0 .. 1 := 0;
   begin
      if not Description.Valid or else not Description.Engines (Render) or else
        not Description.Engines (Copy)
      then
         return Result;
      end if;
      -- GuC classes: render0, video1, enhance2, blitter3. Not CuBit enum order.
      Result.Bytes (0) := 0;
      Result.Bytes (96) := 0;
      Result.Bytes (512) := 1;
      Result.Bytes (524) := 1;
      if Description.Engines (Video_0) then
         Result.Bytes (32) := 0;
         Result.Bytes (516) := 1;
         Video_Index := 1;
      end if;
      if Description.Engines (Video_2) then
         Result.Bytes (32 + Video_Index) := 2;
         Result.Bytes (516) := Result.Bytes (516) or 4;
      end if;
      if Description.Engines (Enhance_0) then
         Result.Bytes (64) := 0;
         Result.Bytes (520) := 1;
      end if;
      Result.Valid := True;
      return Result;
   end Encode;
end Intel_GPU_ADS_Engines;

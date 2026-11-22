package body Intel_GPU_ADLN_Drawing_Rectangle with SPARK_Mode is
   function Build (Width, Height : Natural) return Image is
      Result : Image;
   begin
      if Width not in 1 .. 16_384 or else Height not in 1 .. 16_384 then
         return Result;
      end if;
      Result.Data :=
        [Encode (Header'(others => <>)),
         Encode (Coordinates'(others => <>)),
         Encode (Coordinates'(X => Unsigned_16 (Width - 1),
                              Y => Unsigned_16 (Height - 1))),
         Encode (Origin'(others => <>))];
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Drawing_Rectangle;

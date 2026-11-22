package body Client_Signed_Clip with SPARK_Mode is
   function Edge (Value, Offset, Limit : Coordinate) return Coordinate is
      Translated : constant Long_Long_Integer :=
        Long_Long_Integer (Value) - Long_Long_Integer (Offset);
   begin
      if Limit <= 0 or else Translated <= 0 then
         return 0;
      elsif Translated >= Long_Long_Integer (Limit) then
         return Limit;
      else
         return Coordinate (Translated);
      end if;
   end Edge;
end Client_Signed_Clip;

pragma Ada_2022;

package body Charts_Time_Axis with SPARK_Mode is

   function Shifted (V : Viewport; Columns_Moved : Integer) return Viewport is
      Ceiling : constant Column_Number := Maximum_Time_Us / V.Width - 1;
      Result : Viewport := V;
   begin
      if Columns_Moved >= 0 then
         declare
            Moved : constant Unsigned_64 := Unsigned_64 (Columns_Moved);
         begin
            Result.Last := (if Moved > Ceiling - V.Last then Ceiling else V.Last + Moved);
         end;
      else
         declare
            Moved : constant Unsigned_64 := Unsigned_64 (-(Long_Long_Integer (Columns_Moved)));
         begin
            Result.Last := (if Moved > V.Last then 0 else V.Last - Moved);
         end;
      end if;
      return Result;
   end Shifted;

   function Tick_Step (W : Column_Width; Minimum_Pixels : Tick_Pixels) return Unsigned_64 is
      Wanted : constant Unsigned_64 := W * Unsigned_64 (Minimum_Pixels);
   begin
      for I in Tick_Index loop
         if Steps (I) >= Wanted then
            return Steps (I);
         end if;
         pragma Loop_Invariant (for all J in 1 .. I => Steps (J) < Wanted);
      end loop;
      return Steps (Tick_Steps);
   end Tick_Step;
end Charts_Time_Axis;

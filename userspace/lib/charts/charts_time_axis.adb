pragma Ada_2022;

package body Charts_Time_Axis with SPARK_Mode is

   function X_Of (V : Viewport; C : Column_Number) return Natural is
      subtype Column_Word is Unsigned_64 range 0 .. Maximum_Columns;
      Back : constant Unsigned_64 := V.Last - C;
      Back_Word : constant Column_Word := (if Back < Unsigned_64 (V.Columns) then Back else 0);
      --  In Natural from here (the word's subtype carries the bound across).
      Behind : constant Natural := Natural'Min (Natural (Back_Word), V.Columns - 1);
   begin
      return V.Columns - 1 - Behind;
   end X_Of;

   function Shifted (V : Viewport; Columns_Moved : Integer) return Viewport is
      Quotient : constant Unsigned_64 := Maximum_Time_Us / V.Width;
      Ceiling : constant Column_Number := Quotient - 1;
      Result : Viewport := V;
   begin
      --  Valid (V): V.Last < Quotient, so Quotient >= 1 and V.Last <= Ceiling.
      pragma Assert (V.Last < Quotient);
      pragma Assert (V.Last <= Ceiling);
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
      pragma Assert (Result.Last <= Ceiling);
      pragma Assert (Result.Width = V.Width);
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

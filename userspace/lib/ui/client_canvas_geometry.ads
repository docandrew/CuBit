-- Pixel boundaries for logical cells. Ceil at both edges partitions adjacent
-- cells without overlap, including fractional-density nested canvas origins.
package Client_Canvas_Geometry with SPARK_Mode, Pure is
   subtype Logical_Edge is Natural range 0 .. 65_535;
   subtype Component is Positive range 1 .. 16;
   subtype Physical_Edge is Natural range 0 .. 1_048_560;
   function Clamped_End (Start, Length, Limit : Natural) return Natural
     with Post => Clamped_End'Result <= Limit and
       Clamped_End'Result = (if Start >= Limit then Limit
         elsif Length >= Limit - Start then Limit else Start + Length);
   function Edge (Value : Logical_Edge; Numerator, Denominator : Component)
     return Physical_Edge
     with Post => Edge'Result = (Value * Numerator + Denominator - 1) / Denominator and
       Edge'Result * Denominator >= Value * Numerator and
       (if Edge'Result > 0 then (Edge'Result - 1) * Denominator < Value * Numerator);
   function Relative
     (Origin, Value : Logical_Edge; Numerator, Denominator : Component)
      return Physical_Edge
     with Pre => Value <= Logical_Edge'Last - Origin,
       Post => Relative'Result = Edge (Origin + Value, Numerator, Denominator) -
         Edge (Origin, Numerator, Denominator);
   function Sample
     (Origin, Length : Logical_Edge; Pixel : Physical_Edge;
      Numerator, Denominator : Component) return Logical_Edge
     with Pre => Length > 0 and then Length <= Logical_Edge'Last - Origin and then
       Pixel < Relative (Origin, Length, Numerator, Denominator),
       Post => Sample'Result < Length and
         Sample'Result = ((Edge (Origin, Numerator, Denominator) + Pixel) *
           Denominator) / Numerator - Origin;
end Client_Canvas_Geometry;

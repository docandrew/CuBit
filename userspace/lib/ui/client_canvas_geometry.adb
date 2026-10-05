package body Client_Canvas_Geometry with SPARK_Mode is
   --  Proved free of run-time errors; tests/ui-raster/run.sh re-proves every
   --  unit carrying this pragma and fails on any unproved check.
   pragma Suppress (All_Checks);
   function Clamped_End (Start, Length, Limit : Natural) return Natural is
     (if Start >= Limit then Limit elsif Length >= Limit - Start then Limit
      else Start + Length);
   function Edge (Value : Logical_Edge; Numerator, Denominator : Component)
     return Physical_Edge is
     ((Value * Numerator + Denominator - 1) / Denominator);
   function Relative
     (Origin, Value : Logical_Edge; Numerator, Denominator : Component)
      return Physical_Edge is
     (Edge (Origin + Value, Numerator, Denominator) - Edge (Origin, Numerator, Denominator));
   function Sample
     (Origin, Length : Logical_Edge; Pixel : Physical_Edge;
      Numerator, Denominator : Component) return Logical_Edge is
      pragma Unreferenced (Length);
   begin
      return ((Edge (Origin, Numerator, Denominator) + Pixel) * Denominator) /
        Numerator - Origin;
   end Sample;
end Client_Canvas_Geometry;

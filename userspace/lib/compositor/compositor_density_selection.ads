with CuBit.Display_Geometry;
package Compositor_Density_Selection with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   use type G.Logical_Coordinate;
   Maximum_Outputs : constant := 16;
   subtype Output_Index is Positive range 1 .. Maximum_Outputs;
   type Outputs is array (Output_Index) of G.Output;
   -- LCM(1..16) gives an exact integer order for every supported rational scale.
   function Rank (Scale : G.UI_Scale) return Positive is
     (Positive (Scale.Numerator) * (720_720 / Positive (Scale.Denominator)))
     with Post => Rank'Result * Positive (Scale.Denominator) =
       Positive (Scale.Numerator) * 720_720;
   function Intersects (Screen : G.Output; Window : G.Logical_Rectangle) return Boolean;
   function Choose
     (Screens : Outputs; Count, Primary : Output_Index;
      Window : G.Logical_Rectangle) return Output_Index
     with Pre => Primary <= Count,
       Post => Choose'Result <= Count and then
         (if (for some I in 1 .. Count => Intersects (Screens (I), Window)) then
            Intersects (Screens (Choose'Result), Window) and then
            (for all I in 1 .. Count =>
               (if Intersects (Screens (I), Window) then
                  Rank (Screens (Choose'Result).Scale) >= Rank (Screens (I).Scale)))
          else Choose'Result = Primary);
end Compositor_Density_Selection;

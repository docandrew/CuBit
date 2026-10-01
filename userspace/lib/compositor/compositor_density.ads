--  Native-density backing-storage admission. No allocation, FFI or authority.
--  Logical extents remain independent from the resulting physical pixel grid.
package Compositor_Density with SPARK_Mode, Pure is
   subtype Extent is Positive range 1 .. 65_535;
   subtype Scale_Component is Positive range 1 .. 16;
   type Scale is record
      Numerator, Denominator : Scale_Component := 1;
   end record;
   subtype Row_Alignment is Positive range 4 .. 4_096
     with Dynamic_Predicate => Row_Alignment mod 4 = 0;
   subtype Scaled_Extent is Positive range 1 .. 65_535 * 16;
   function Pixels (Logical : Extent; Density : Scale) return Scaled_Extent
     with Post =>
       Pixels'Result =
         (Logical * Density.Numerator + Density.Denominator - 1) /
           Density.Denominator and then
       Pixels'Result * Density.Denominator >= Logical * Density.Numerator and then
       (Pixels'Result - 1) * Density.Denominator < Logical * Density.Numerator;

   type Admission is (Accepted, Extent_Exceeded, Budget_Exceeded);
   type Layout (Status : Admission := Budget_Exceeded) is record
      case Status is
         when Accepted =>
            Width, Height : Extent;
            Pitch, Bytes : Positive;
         when others => null;
      end case;
   end record;
   function Plan
     (Logical_Width, Logical_Height : Extent;
      Density : Scale; Byte_Budget : Natural;
      Alignment : Row_Alignment := 4) return Layout
     with Post =>
       (if Pixels (Logical_Width, Density) > Extent'Last or else
           Pixels (Logical_Height, Density) > Extent'Last
        then Plan'Result.Status = Extent_Exceeded
        else Plan'Result.Status /= Extent_Exceeded and then
          (Plan'Result.Status = Accepted) =
            (Long_Long_Integer
               (((Pixels (Logical_Width, Density) * 4 + Alignment - 1) /
                    Alignment) * Alignment) *
               Long_Long_Integer (Pixels (Logical_Height, Density)) <=
                 Long_Long_Integer (Byte_Budget))) and then
       (if Plan'Result.Status = Accepted then
          Plan'Result.Width = Pixels (Logical_Width, Density) and then
          Plan'Result.Height = Pixels (Logical_Height, Density) and then
          Plan'Result.Pitch >= Plan'Result.Width * 4 and then
          Plan'Result.Pitch - Plan'Result.Width * 4 < Alignment and then
          Plan'Result.Pitch mod Alignment = 0 and then
          Plan'Result.Pitch mod 4 = 0 and then
          Long_Long_Integer (Plan'Result.Bytes) =
            Long_Long_Integer (Plan'Result.Pitch) *
              Long_Long_Integer (Plan'Result.Height) and then
          Plan'Result.Bytes <= Byte_Budget);
end Compositor_Density;

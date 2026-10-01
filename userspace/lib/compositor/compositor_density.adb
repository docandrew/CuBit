package body Compositor_Density with SPARK_Mode is
   function Pixels (Logical : Extent; Density : Scale) return Scaled_Extent is
     ((Logical * Density.Numerator + Density.Denominator - 1) /
        Density.Denominator);

   function Plan
     (Logical_Width, Logical_Height : Extent;
      Density : Scale; Byte_Budget : Natural;
      Alignment : Row_Alignment := 4) return Layout
   is
      W : constant Scaled_Extent := Pixels (Logical_Width, Density);
      H : constant Scaled_Extent := Pixels (Logical_Height, Density);
   begin
      if W > Extent'Last or else H > Extent'Last then
         return (Status => Extent_Exceeded);
      end if;
      declare
         Row_Bytes : constant Positive := W * 4;
         Pitch : constant Positive :=
           ((Row_Bytes + Alignment - 1) / Alignment) * Alignment;
         Bytes : constant Long_Long_Integer :=
           Long_Long_Integer (Pitch) * Long_Long_Integer (H);
      begin
         if Bytes > Long_Long_Integer (Byte_Budget) then
            return (Status => Budget_Exceeded);
         end if;
         return (Accepted, W, H, Pitch, Positive (Bytes));
      end;
   end Plan;
end Compositor_Density;

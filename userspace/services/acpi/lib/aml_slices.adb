package body AML_Slices with SPARK_Mode is
   function Select_Range
     (Source_Length : Extent; Start, Count : AML_Decode.Integer_Value)
      return Slice_Range
   is
      Offset, Remaining : Extent;
   begin
      if Start >= AML_Decode.Integer_Value (Source_Length) then
         return (Offset => Source_Length, Length => 0);
      end if;
      Offset := Extent (Start);
      Remaining := Source_Length - Offset;
      if Count >= AML_Decode.Integer_Value (Remaining) then
         return (Offset => Offset, Length => Remaining);
      end if;
      return (Offset => Offset, Length => Extent (Count));
   end Select_Range;
end AML_Slices;

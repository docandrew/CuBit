with AML_Decode;
with AML_Objects;
package AML_Slices with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   subtype Extent is Natural range 0 .. AML_Objects.Max_Bytes;
   type Slice_Range is record
      Offset : Extent := 0;
      Length : Extent := 0;
   end record;
   -- Callers perform AML integer-width normalization. This policy accepts
   -- every unsigned value without narrowing it until it is in the source.
   function Select_Range
     (Source_Length : Extent; Start, Count : AML_Decode.Integer_Value)
      return Slice_Range
     with Global => null,
       Post =>
         Select_Range'Result.Offset <= Source_Length
         and then Select_Range'Result.Length <= Source_Length - Select_Range'Result.Offset
         and then AML_Decode.Integer_Value (Select_Range'Result.Offset) =
           AML_Decode.Integer_Value'Min (Start, AML_Decode.Integer_Value (Source_Length))
         and then AML_Decode.Integer_Value (Select_Range'Result.Length) =
           AML_Decode.Integer_Value'Min
             (Count, AML_Decode.Integer_Value (Source_Length) -
               AML_Decode.Integer_Value'Min (Start, AML_Decode.Integer_Value (Source_Length)));
end AML_Slices;

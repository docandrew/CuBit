pragma Ada_2022;
with AML_Decode;

-- Pure framing and sizing. No backing allocation or fixed Content array.
package AML_Buffer_Layouts with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Status;
   Buffer_Length_Mask : constant AML_Decode.Integer_Value := 16#FFFF_FFFF#;
   subtype Buffer_Length is AML_Decode.Integer_Value range 0 .. Buffer_Length_Mask;
   type Layout_Result (Kind : AML_Decode.Status := AML_Decode.Truncated) is record
      case Kind is
         when AML_Decode.Accepted =>
            Consumed : Positive;
            -- Offset is relative to Data'First, independent of its lower bound.
            Initializer_Offset : Natural;
            Initializer_Length : Natural;
            Requested_Length : Buffer_Length;
            Effective_Length : Buffer_Length;
         when others => null;
      end case;
   end record;
   function With_Count
     (Data : AML_Decode.Bytes; Count_Value : AML_Decode.Integer_Value;
      Count_Consumed : Natural) return Layout_Result
     with Post => (if With_Count'Result.Kind = AML_Decode.Accepted then
       AML_Decode.Check_Buffer_Count_Span (Data, Count_Consumed).Kind = AML_Decode.Accepted
       and then With_Count'Result.Consumed = AML_Decode.Check_Buffer_Count_Span (Data, Count_Consumed).Consumed
       and then With_Count'Result.Initializer_Offset = AML_Decode.Check_Buffer_Count_Span (Data, Count_Consumed).Raw_Offset
       and then With_Count'Result.Consumed <= Data'Length
       and then With_Count'Result.Initializer_Offset <= With_Count'Result.Consumed
       and then With_Count'Result.Initializer_Length =
         With_Count'Result.Consumed - With_Count'Result.Initializer_Offset
       and then With_Count'Result.Requested_Length = (Count_Value and Buffer_Length_Mask)
       and then With_Count'Result.Effective_Length = AML_Decode.Integer_Value'Max
         (With_Count'Result.Requested_Length,
          AML_Decode.Integer_Value (With_Count'Result.Initializer_Length)));
   -- Independent accepted-result relation: decode package framing and its
   -- literal integer count, never call Read or With_Count.
   function Literal_Layout_Matches
     (Data : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Layout : Layout_Result) return Boolean with Ghost,
        Annotate => (GNATprove, Inline_For_Proof);
   function Read (Data : AML_Decode.Bytes; Width : AML_Decode.Integer_Width)
      return Layout_Result
     with Post => (if Read'Result.Kind = AML_Decode.Accepted then
       Literal_Layout_Matches (Data, Width, Read'Result)
       and then Read'Result.Consumed <= Data'Length
       and then Read'Result.Initializer_Offset <= Read'Result.Consumed
       and then Read'Result.Initializer_Length = Read'Result.Consumed - Read'Result.Initializer_Offset
       and then Read'Result.Effective_Length = AML_Decode.Integer_Value'Max
         (Read'Result.Requested_Length, AML_Decode.Integer_Value (Read'Result.Initializer_Length)));
end AML_Buffer_Layouts;

with AML_Decode;
generic
   Max_Output_Length : Natural;
package AML_Explicit_Formatting with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Integer_Width;
   type Format_Mode is (Decimal_Format, Hexadecimal_Format);
   subtype Extent is Natural range 0 .. Max_Output_Length;
   Decimal_Radix : constant AML_Decode.Integer_Value := 10;
   Hexadecimal_Radix : constant AML_Decode.Integer_Value := 16;
   Max_Integer_Decimal_Digits : constant := 20;
   Hex_Prefix_Length : constant := 2;
   Hex_Byte_Digits : constant := 2;
   Max_Decimal_Byte_Digits : constant := 3;
   Separator_Length : constant := 1;
   Max_Encoded_Byte_Width : constant := Hex_Prefix_Length + Hex_Byte_Digits + Separator_Length;
   -- Each byte contributes at most this width, including its separator.
   -- This bound makes every required-length sum/product fit Natural before
   -- comparison with the independently chosen output capacity.
   Max_Input_Length : constant := Natural'Last / Max_Encoded_Byte_Width;
   type Build_Status is (Built, Length_Limit);
   type Result (Status : Build_Status := Length_Limit; Length : Extent := 0) is record
      case Status is
         when Built => Data : AML_Decode.Bytes (1 .. Length);
         when Length_Limit => null;
      end case;
   end record;
   function Width_Value (Value : AML_Decode.Integer_Value; Width : AML_Decode.Integer_Width)
      return AML_Decode.Integer_Value is
     (if Width = AML_Decode.Bits_32 then Value and 16#FFFF_FFFF# else Value);
   function Integer_Length (Mode : Format_Mode; Width : AML_Decode.Integer_Width;
      Value : AML_Decode.Integer_Value) return Positive
     with Global => null, Post => Integer_Length'Result <= Max_Integer_Decimal_Digits;
   function Buffer_Length (Mode : Format_Mode; Data : AML_Decode.Bytes) return Natural
     with Global => null, Pre => Data'Length <= Max_Input_Length,
       Post => Buffer_Length'Result <= Data'Length * Max_Encoded_Byte_Width;
   -- Independent recognizers parse output tokens; they never call the builders.
   function Integer_Encoding (Mode : Format_Mode; Width : AML_Decode.Integer_Width;
      Value : AML_Decode.Integer_Value; Data : AML_Decode.Bytes) return Boolean
     with Ghost, Global => null;
   function Buffer_Encoding (Mode : Format_Mode; Source, Data : AML_Decode.Bytes) return Boolean
     with Ghost, Global => null;
   function From_Integer (Mode : Format_Mode; Width : AML_Decode.Integer_Width;
      Value : AML_Decode.Integer_Value) return Result
     with Global => null,
       Post =>
         (From_Integer'Result.Status = Built) = (Integer_Length (Mode, Width, Value) <= Max_Output_Length)
         and then (if From_Integer'Result.Status = Built then
           From_Integer'Result.Length = Integer_Length (Mode, Width, Value)
           and then Integer_Encoding (Mode, Width, Value, From_Integer'Result.Data)
         else From_Integer'Result.Length = 0);
   function From_Buffer (Mode : Format_Mode; Data : AML_Decode.Bytes) return Result
     with Global => null, Pre => Data'Length <= Max_Input_Length,
       Post =>
         (From_Buffer'Result.Status = Built) = (Buffer_Length (Mode, Data) <= Max_Output_Length)
         and then (if From_Buffer'Result.Status = Built then
           From_Buffer'Result.Length = Buffer_Length (Mode, Data)
           and then Buffer_Encoding (Mode, Data, From_Buffer'Result.Data)
         else From_Buffer'Result.Length = 0);
end AML_Explicit_Formatting;

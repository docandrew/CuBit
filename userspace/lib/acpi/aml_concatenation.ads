pragma Ada_2022;
with AML_Decode;
generic
   Max_Result_Length : Positive;
package AML_Concatenation with SPARK_Mode, Pure is
   use type AML_Decode.Byte;
   type Input_Kind is (Integer_Input, String_Input, Buffer_Input);
   type Output_Kind is (String_Output, Buffer_Output);
   type Build_Status is (Built, Empty_Buffer, Length_Limit);
   subtype Result_Length is Natural range 0 .. Max_Result_Length;
   subtype Output_Bytes is AML_Decode.Bytes (1 .. Max_Result_Length);
   type Result (Status : Build_Status := Length_Limit) is record
      case Status is
         when Built =>
            Kind : Output_Kind;
            Length : Result_Length;
            Data : Output_Bytes := [others => 0];
         when others => null;
      end case;
   end record;
   -- String arrays contain declared bytes, excluding their final terminator.
   -- Number is used only for Integer_Input; its Data array is ignored.
   -- The first input selects conversion; all unused output bytes remain zero.
   function Build
     (Width : AML_Decode.Integer_Width;
      Left_Kind : Input_Kind; Left_Number : AML_Decode.Integer_Value;
      Left_Data : AML_Decode.Bytes;
      Right_Kind : Input_Kind; Right_Number : AML_Decode.Integer_Value;
      Right_Data : AML_Decode.Bytes) return Result
     with Post => (if Build'Result.Status = Built then
       (for all I in Build'Result.Data'Range =>
          (if I > Build'Result.Length then Build'Result.Data (I) = 0)));
end AML_Concatenation;

pragma Ada_2022;
with AML_Decode;
with AML_Coercions;
package AML_Mixed_Comparison with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   type Input_Kind is (Integer_Input, String_Input, Buffer_Input);
   Max_Input_Length : constant := 65_536;
   subtype Input_Length is Natural range 0 .. Max_Input_Length;
   Max_View_Length : constant := Max_Input_Length * 5 - 1;
   subtype View_Length is Natural range 0 .. Max_View_Length;
   type Comparison_Status is (Compared, Empty_Buffer);
   subtype Comparison_Value is Integer range -1 .. 1;
   type Result is record
      Status : Comparison_Status := Empty_Buffer;
      Value : Comparison_Value := 0;
   end record;
   -- Integers are already coerced to the active AML width by the caller.
   -- Data is the declared payload (String excludes its terminator).
   -- No storage allocation or namespace authority is involved.
   function Compare
     (Width : AML_Decode.Integer_Width;
      Left_Kind : Input_Kind; Left_Number : AML_Decode.Integer_Value;
      Left_Data : AML_Decode.Bytes;
      Right_Kind : Input_Kind; Right_Number : AML_Decode.Integer_Value;
      Right_Data : AML_Decode.Bytes) return Result
   with Pre => Left_Data'Length <= Max_Input_Length
     and then Right_Data'Length <= Max_Input_Length
     and then Left_Number <= AML_Coercions.Maximum (Width)
     and then Right_Number <= AML_Coercions.Maximum (Width),
     Post => (if Compare'Result.Status = Empty_Buffer then
       Compare'Result.Value = 0)
       and then (Compare'Result.Status = Empty_Buffer) =
         (Left_Kind = Integer_Input and then Right_Kind = Buffer_Input
          and then Right_Data'Length = 0);
end AML_Mixed_Comparison;

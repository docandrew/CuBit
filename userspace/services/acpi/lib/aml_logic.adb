pragma Ada_2022;
with AML_Integers;
package body AML_Logic with SPARK_Mode is
   function Apply
     (Op : AML_Decode.Byte; Left, Right : AML_Decode.Integer_Value;
      Width : AML_Decode.Integer_Width) return AML_Decode.Integer_Value is
     (if Truth (Op, Left, Right) then
         AML_Integers.Normalize (AML_Decode.Integer_Value'Last, Width)
      else 0);
end AML_Logic;

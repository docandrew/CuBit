pragma Ada_2022;
with AML_Decode;
package AML_Logic with SPARK_Mode, Pure is
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Width;
   use type AML_Decode.Integer_Value;
   function Supported (Op : AML_Decode.Byte) return Boolean is
     (Op in 16#90# .. 16#95#);
   function Unary (Op : AML_Decode.Byte) return Boolean is (Op = 16#92#);
   --  Integer operands only. AML string/buffer comparison and conversions
   --  require the object evaluator. Logical operators have no target operand.
   function Truth
     (Op : AML_Decode.Byte; Left, Right : AML_Decode.Integer_Value)
      return Boolean is
     (case Op is
         when 16#90# => Left /= 0 and Right /= 0,
         when 16#91# => Left /= 0 or Right /= 0,
         when 16#92# => Right = 0,
         when 16#93# => Left = Right,
         when 16#94# => Left > Right,
         when others => Left < Right)
     with Pre => Supported (Op);
   function Apply
     (Op : AML_Decode.Byte; Left, Right : AML_Decode.Integer_Value;
      Width : AML_Decode.Integer_Width) return AML_Decode.Integer_Value
     with Pre => Supported (Op),
          Post => Apply'Result =
            (if Truth (Op, Left, Right) then
                (if Width = AML_Decode.Bits_32 then 16#FFFF_FFFF#
                 else AML_Decode.Integer_Value'Last)
             else 0);
end AML_Logic;

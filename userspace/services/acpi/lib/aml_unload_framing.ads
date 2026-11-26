with AML_Decode;
package AML_Unload_Framing with SPARK_Mode, Pure is
   use type AML_Decode.Byte;
   Unload_Extension : constant AML_Decode.Byte := 16#2A#;
   type Operand_Lead is (Truncated, Invalid, Value_Operand, Debug_Operand);
   -- This is a lead classification, not full operand parsing. Value_Operand
   -- still goes through the ordinary SuperName resolver before type refusal.
   function Inspect (Code : AML_Decode.Bytes) return Operand_Lead
     with Global => null,
       Post => Inspect'Result =
         (if Code'Length = 0 then Truncated
          elsif Code (Code'First) = AML_Decode.Extended_Op then
            (if Code'Length = 1 then Truncated
             elsif Code (Code'First + 1) = AML_Decode.Debug_Extension then Debug_Operand
             else Invalid)
          elsif Code (Code'First) in 0 | 16#41# .. 16#5A# | 16#5F#
            | 16#5C# | 16#5E# | 16#2E# | 16#2F#
            | 16#60# .. 16#6E# | 16#71# | 16#83# | 16#88#
          then Value_Operand else Invalid);
end AML_Unload_Framing;

package body AML_Unload_Framing with SPARK_Mode is
   function Inspect (Code : AML_Decode.Bytes) return Operand_Lead is
   begin
      if Code'Length = 0 then return Truncated; end if;
      if Code (Code'First) = AML_Decode.Extended_Op then
         if Code'Length = 1 then return Truncated; end if;
         if Code (Code'First + 1) = AML_Decode.Debug_Extension then
            return Debug_Operand;
         end if;
         return Invalid;
      end if;
      if Code (Code'First) in 0 | 16#41# .. 16#5A# | 16#5F#
        | 16#5C# | 16#5E# | 16#2E# | 16#2F#
        | 16#60# .. 16#6E# | 16#71# | 16#83# | 16#88#
      then return Value_Operand; end if;
      return Invalid;
   end Inspect;
end AML_Unload_Framing;

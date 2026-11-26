with AML_Unload_Framing;
package body AML_Unload_Statement with SPARK_Mode is
   use type AML_Decode.Byte;
   function Prepare
     (Code : AML_Decode.Bytes; Offset, Limit, Charged, Budget : Natural) return Plan
   is
      Cursor : Natural;
      Lead : AML_Unload_Framing.Operand_Lead;
   begin
      if Offset = Limit then return (Offset, Charged, Truncated); end if;
      Cursor := Offset + 1;
      if Code (Code'First + Offset) /= AML_Unload_Framing.Unload_Extension then
         return (Cursor, Charged, Invalid_Operand);
      end if;
      if Cursor = Limit then return (Cursor, Charged, Truncated); end if;
      Lead := AML_Unload_Framing.Inspect
        (Code (Code'First + Cursor .. Code'First + (Limit - 1)));
      case Lead is
         when AML_Unload_Framing.Truncated => return (Cursor, Charged, Truncated);
         when AML_Unload_Framing.Invalid => return (Cursor, Charged, Invalid_Operand);
         when AML_Unload_Framing.Value_Operand => return (Cursor, Charged, Evaluate_Operand);
         when AML_Unload_Framing.Debug_Operand =>
            if Budget - Charged < 2 then return (Cursor, Charged, Budget_Exceeded); end if;
            return (Cursor + 2, Charged + 2, Complete_Invalid_Handle);
      end case;
   end Prepare;
end AML_Unload_Statement;

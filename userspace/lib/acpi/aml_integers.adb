pragma Ada_2022;
package body AML_Integers with SPARK_Mode is
   function Bit_Set (Value : AML_Decode.Integer_Value; Position : Set_Bit_Position)
     return Boolean is
   begin
      return (Value and Interfaces.Shift_Left (1, Position - 1)) /= 0;
   end Bit_Set;
   function Find_Set (Value : AML_Decode.Integer_Value; Highest : Boolean)
     return Bit_Position is
      Result : Bit_Position := 0;
   begin
      for I in 1 .. 64 loop
         if Bit_Set (Value, I) and then
           (Highest or else Result = 0)
         then Result := I; end if;
         pragma Loop_Invariant (Correct_Position (Value, Highest, Result, I));
      end loop;
      return Result;
   end Find_Set;
   function Apply
     (Op : AML_Decode.Byte; Left, Right : AML_Decode.Integer_Value;
      Width : AML_Decode.Integer_Width) return AML_Decode.Integer_Value
   is
      Result : AML_Decode.Integer_Value;
   begin
      case Op is
         when 16#72# => Result := Left + Right;
         when 16#74# => Result := Left - Right;
         when 16#77# => Result := Left * Right;
         when 16#78# => Result := Left / Right;
         when 16#85# => Result := Left mod Right;
         when 16#79# =>
            Result := (if Right >= AML_Decode.Integer_Value (Bit_Width (Width)) then 0
                       else Interfaces.Shift_Left (Left, Natural (Right)));
         when 16#7A# =>
            Result := (if Right >= AML_Decode.Integer_Value (Bit_Width (Width)) then 0
                       else Interfaces.Shift_Right (Left, Natural (Right)));
         when 16#7B# => Result := Left and Right;
         when 16#7C# => Result := not (Left and Right);
         when 16#7D# => Result := Left or Right;
         when 16#7E# => Result := not (Left or Right);
         when 16#80# => Result := not Right;
         when 16#81# => Result := AML_Decode.Integer_Value (Find_Set (Right, True));
         when 16#82# => Result := AML_Decode.Integer_Value (Find_Set (Right, False));
         when others => Result := Left xor Right;
      end case;
      return Normalize (Result, Width);
   end Apply;
end AML_Integers;

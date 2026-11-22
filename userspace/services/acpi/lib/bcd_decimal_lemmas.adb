pragma Ada_2022;
package body BCD_Decimal_Lemmas with SPARK_Mode is
   procedure Divide_Step (Value : Decimal_Number; Factor : Decimal_Factor) is
      Quotient : constant Decimal_Number := Value / Factor;
      Remainder : constant Decimal_Number := Value mod Factor;
      Next_Quotient : constant Decimal_Number := Quotient / Decimal_Radix;
      Next_Remainder : constant Decimal_Number := Quotient mod Decimal_Radix;
   begin
      pragma Assert (Value = Quotient * Factor + Remainder);
      pragma Assert (Quotient = Next_Quotient * Decimal_Radix + Next_Remainder);
      pragma Assert (Next_Remainder * Factor + Remainder < Factor * Decimal_Radix);
      pragma Assert (Value = Next_Quotient * (Factor * Decimal_Radix)
        + (Next_Remainder * Factor + Remainder));
   end Divide_Step;
   procedure Divide_Word_Step (Value : Word; Factor : Word_Factor) is
      Quotient : constant Word := Value / Factor;
      Remainder : constant Word := Value mod Factor;
      Next_Quotient : constant Word := Quotient / Decimal_Radix;
      Next_Remainder : constant Word := Quotient mod Decimal_Radix;
   begin
      pragma Assert (Value = Quotient * Factor + Remainder);
      pragma Assert (Quotient = Next_Quotient * Decimal_Radix + Next_Remainder);
      pragma Assert (Next_Remainder * Factor + Remainder < Factor * Decimal_Radix);
      pragma Assert (Value = Next_Quotient * (Factor * Decimal_Radix)
        + (Next_Remainder * Factor + Remainder));
   end Divide_Word_Step;
   procedure Append_Bounded (Number : Word; Digit : Decimal_Digit; Factor : Word_Factor) is
   begin
      pragma Assert (Number * Decimal_Radix + Digit < Factor * Decimal_Radix);
   end Append_Bounded;
   procedure Divide_Radix_First(Value:Word;Factor:Word_Factor) is
      Quotient:constant Word:=Value/Decimal_Radix;
      Remainder:constant Word:=Value mod Decimal_Radix;
      Next_Quotient:constant Word:=Quotient/Factor;
      Next_Remainder:constant Word:=Quotient mod Factor;
   begin
      pragma Assert(Value=Quotient*Decimal_Radix+Remainder);
      pragma Assert(Quotient=Next_Quotient*Factor+Next_Remainder);
      pragma Assert(Next_Remainder*Decimal_Radix+Remainder<Factor*Decimal_Radix);
      pragma Assert(Value=Next_Quotient*(Factor*Decimal_Radix)+(Next_Remainder*Decimal_Radix+Remainder));
   end Divide_Radix_First;
   procedure Append_Preserves_Quotient(Number:Word;Digit:Decimal_Digit;Factor:Word_Factor) is
      Next:constant Word:=Number*Decimal_Radix+Digit;
   begin
      Append_Bounded(Number,Digit,Word(Decimal_Factor'Last));
      Divide_Radix_First(Next,Factor);
   end Append_Preserves_Quotient;
end BCD_Decimal_Lemmas;

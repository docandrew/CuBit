pragma Ada_2022;
with BCD_Binary_Lemmas;
with BCD_Decimal_Lemmas;
package body AML_BCD with SPARK_Mode is
   type Digit_Witness is array(Digit_Index) of BCD_Decimal_Lemmas.Decimal_Digit;
   procedure Decimal_Witness(Value:AML_Decode.Integer_Value;Cache:out Digit_Witness) with Ghost,
     Post=>(for all P in Digit_Index=>Cache(P)=(Value/Decimal_Power(P)) mod Decimal_Radix);
   procedure Decimal_Witness(Value:AML_Decode.Integer_Value;Cache:out Digit_Witness) is
   begin Cache:=[for P in Digit_Index=>(Value/Decimal_Power(P)) mod Decimal_Radix];end Decimal_Witness;
   procedure Binary_Witness(Value:AML_Decode.Integer_Value;Width:AML_Decode.Integer_Width;Cache:out Digit_Witness) with Ghost,
     Pre=>Valid_BCD(Value,Width),
     Post=>(for all P in Digit_Index=>(if P<Active_Digits(Width) then Cache(P)=AML_Decode.Integer_Value(Digit(Value,P))));
   procedure Binary_Witness(Value:AML_Decode.Integer_Value;Width:AML_Decode.Integer_Width;Cache:out Digit_Witness) is
   begin Cache:=[for P in Digit_Index=>(if P<Active_Digits(Width) then AML_Decode.Integer_Value(Digit(Value,P)) else 0)];end Binary_Witness;
   procedure Link_Digits (Value : AML_Decode.Integer_Value) with Ghost,
     Post => (for all Position in Digit_Index =>
       AML_Decode.Integer_Value (Digit (Value, Position)) =
         BCD_Binary_Lemmas.At_Digit (Value, Position));
   procedure Link_Digits (Value : AML_Decode.Integer_Value) is
   begin
      null;
   end Link_Digits;
   function Preserve_One_Digit(Witness:Digit_Witness;Number:AML_Decode.Integer_Value;
      Index,Position:Digit_Index;Width:AML_Decode.Integer_Width) return Boolean with Ghost,
      Pre=>Index<=Position and then Position<Active_Digits(Width)
        and then Number<Decimal_Power(Digit_Index(Active_Digits(Width)-Index-1))
        and then (if Position>Index then
          (Number/Decimal_Power(Digit_Index(Position-Index-1))) mod Decimal_Radix=Witness(Position)),
      Post=>Preserve_One_Digit'Result and then
        ((Number*Decimal_Radix+Witness(Index))/Decimal_Power(Digit_Index(Position-Index))) mod Decimal_Radix=Witness(Position);
   function Preserve_One_Digit(Witness:Digit_Witness;Number:AML_Decode.Integer_Value;
      Index,Position:Digit_Index;Width:AML_Decode.Integer_Width) return Boolean is
      New_Digit:constant BCD_Decimal_Lemmas.Decimal_Digit:=Witness(Index);
      Next:constant AML_Decode.Integer_Value:=Number*Decimal_Radix+New_Digit;
   begin
      BCD_Decimal_Lemmas.Append_Bounded(Number,New_Digit,Decimal_Power(Digit_Index(Active_Digits(Width)-Index-1)));
      if Position=Index then
         pragma Assert(Decimal_Power(Digit_Index(Position-Index))=1);
         pragma Assert(Next mod Decimal_Radix=Witness(Position));
         pragma Assert_And_Cut((Next/Decimal_Power(Digit_Index(Position-Index))) mod Decimal_Radix=Witness(Position));
         return True;
      else
         BCD_Decimal_Lemmas.Append_Preserves_Quotient(Number,New_Digit,Decimal_Power(Digit_Index(Position-Index-1)));
         pragma Assert(Decimal_Power(Digit_Index(Position-Index))=Decimal_Power(Digit_Index(Position-Index-1))*Decimal_Radix);
         pragma Assert(Next/Decimal_Power(Digit_Index(Position-Index))=Number/Decimal_Power(Digit_Index(Position-Index-1)));
         pragma Assert_And_Cut((Next/Decimal_Power(Digit_Index(Position-Index))) mod Decimal_Radix=Witness(Position));
         return True;
      end if;
   end Preserve_One_Digit;
   procedure Append_Leading_Digit(Witness:Digit_Witness;Number:AML_Decode.Integer_Value;
      Index:Digit_Index;Width:AML_Decode.Integer_Width) with Ghost,
      Pre=>Index<Active_Digits(Width)
        and then Number<Decimal_Power(Digit_Index(Active_Digits(Width)-Index-1))
        and then (for all P in Digit_Index=>
          (if P>Index and P<Active_Digits(Width) then
            (Number/Decimal_Power(Digit_Index(P-Index-1))) mod Decimal_Radix=Witness(P))),
      Post=>Number*Decimal_Radix+Witness(Index)<
          Decimal_Power(Digit_Index(Active_Digits(Width)-Index-1))*Decimal_Radix
        and then (for all P in Digit_Index=>
          (if P>=Index and P<Active_Digits(Width) then
            ((Number*Decimal_Radix+Witness(Index))/Decimal_Power(Digit_Index(P-Index))) mod Decimal_Radix=
              Witness(P)));
   procedure Append_Leading_Digit(Witness:Digit_Witness;Number:AML_Decode.Integer_Value;
      Index:Digit_Index;Width:AML_Decode.Integer_Width) is
   begin
      BCD_Decimal_Lemmas.Append_Bounded(Number,Witness(Index),Decimal_Power(Digit_Index(Active_Digits(Width)-Index-1)));
      pragma Assert(for all P in Digit_Index=>
        (if P>=Index and P<Active_Digits(Width) then Preserve_One_Digit(Witness,Number,Index,P,Width)));
   end Append_Leading_Digit;
   procedure Transfer_Active_Digits(Number,Value:AML_Decode.Integer_Value;Cache:Digit_Witness;
      Width:AML_Decode.Integer_Width) with Ghost,
      Pre=>(for all P in Digit_Index=>(if P<Active_Digits(Width) then
        (Number/Decimal_Power(P)) mod Decimal_Radix=Cache(P)
        and then Cache(P)=AML_Decode.Integer_Value(Digit(Value,P)))),
      Post=>(for all P in 0..Active_Digits(Width)-1=>
        (Number/Decimal_Power(Digit_Index(P))) mod Decimal_Radix=AML_Decode.Integer_Value(Digit(Value,P)));
   procedure Transfer_Active_Digits(Number,Value:AML_Decode.Integer_Value;Cache:Digit_Witness;
      Width:AML_Decode.Integer_Width) is
      pragma Unreferenced(Number,Cache);
   begin null;end Transfer_Active_Digits;
   function Decode_Valid_BCD
     (Value : AML_Decode.Integer_Value;
      Width : AML_Decode.Integer_Width) return AML_Decode.Integer_Value
     with Pre => Valid_BCD (Value, Width),
       Post => Decode_Valid_BCD'Result <= Decimal_Maximum (Width)
         and then (for all I in 0 .. Active_Digits (Width) - 1 =>
           (Decode_Valid_BCD'Result / Decimal_Power (Digit_Index (I))) mod Decimal_Radix =
             AML_Decode.Integer_Value (Digit (Value, I)));
   function Decode_Valid_BCD
     (Value : AML_Decode.Integer_Value;
      Width : AML_Decode.Integer_Width) return AML_Decode.Integer_Value
   is
      Number : AML_Decode.Integer_Value := 0;
      Cache : Digit_Witness with Ghost;
   begin
      Binary_Witness(Value,Width,Cache);
      -- Most significant digit first, independent of ACPICA's weighted sum.
      for I in reverse Digit_Index range 0..Digit_Index(Active_Digits(Width)-1) loop
         Append_Leading_Digit(Cache,Number,I,Width);
         pragma Assert(AML_Decode.Integer_Value(Digit(Value,I))=Cache(I));
         Number := Number * Decimal_Radix + AML_Decode.Integer_Value (Digit (Value, I));
         pragma Loop_Invariant(Number<Decimal_Power(Digit_Index(Active_Digits(Width)-I-1))*Decimal_Radix
           and then (for all P in Digit_Index=>
             (if P>=I and P<Active_Digits(Width) then
               (Number/Decimal_Power(Digit_Index(P-I))) mod Decimal_Radix=Cache(P))));
      end loop;
      pragma Assert(for all P in 0..Active_Digits(Width)-1=>
        (Number/Decimal_Power(Digit_Index(P))) mod Decimal_Radix=Cache(Digit_Index(P)));
      Transfer_Active_Digits(Number,Value,Cache,Width);
      pragma Assert_And_Cut(Valid_BCD(Value,Width) and then Number<=Decimal_Maximum(Width)
        and then (for all P in 0..Active_Digits(Width)-1=>
          (Number/Decimal_Power(Digit_Index(P))) mod Decimal_Radix=AML_Decode.Integer_Value(Digit(Value,P))));
      return Number;
   end Decode_Valid_BCD;

   function Converted_BCD (Value : AML_Decode.Integer_Value;
      Width : AML_Decode.Integer_Width) return Result
     with Pre => Valid_BCD (Value, Width),
       Post => Converted_BCD'Result.Status = Converted
         and then Converted_BCD'Result.Value <= Decimal_Maximum (Width)
         and then (for all I in 0 .. Active_Digits (Width) - 1 =>
           (Converted_BCD'Result.Value / Decimal_Power (Digit_Index (I))) mod Decimal_Radix =
             AML_Decode.Integer_Value (Digit (Value, I)));
   function Converted_BCD (Value : AML_Decode.Integer_Value;
      Width : AML_Decode.Integer_Width) return Result is
   begin
      return (Converted, Decode_Valid_BCD (Value, Width));
   end Converted_BCD;
   function From_BCD (Value : AML_Decode.Integer_Value;
                      Width : AML_Decode.Integer_Width) return Result is
     (if Valid_BCD (Value, Width) then Converted_BCD (Value, Width)
      else (Numeric_Overflow, 0));
   function To_BCD (Value : AML_Decode.Integer_Value;
                    Width : AML_Decode.Integer_Width) return Result
   is
      Packed : AML_Decode.Integer_Value := 0;
      Cache : Digit_Witness with Ghost;
   begin
      if Value > Decimal_Maximum (Width) then
         return (Numeric_Overflow, 0);
      end if;
      Decimal_Witness(Value,Cache);
      for I in Digit_Index range 0 .. Digit_Index (Active_Digits (Width) - 1) loop
         pragma Assert((Value/Decimal_Power(I)) mod Decimal_Radix=Cache(I));
         BCD_Binary_Lemmas.Insert_Empty_Digit
           (Packed, (Value / Decimal_Power (I)) mod Decimal_Radix, I);
         if Width = AML_Decode.Bits_32 then
            BCD_Binary_Lemmas.Preserve_Narrow
              (Packed, (Value / Decimal_Power (I)) mod Decimal_Radix, I);
         end if;
         Packed := Packed or Interfaces.Shift_Left
           ((Value / Decimal_Power (I)) mod Decimal_Radix, Nibble_Bits * I);
         pragma Loop_Invariant
           ((for all Position in Digit_Index =>
             (if Position <= I then BCD_Binary_Lemmas.At_Digit (Packed, Position) =
               Cache(Position)
              else BCD_Binary_Lemmas.At_Digit (Packed, Position) = 0))
           and then (if Width = AML_Decode.Bits_32 then Packed <= 16#FFFF_FFFF#));
      end loop;
      Link_Digits (Packed);
      return (Converted, Packed);
   end To_BCD;
end AML_BCD;

pragma Ada_2022;
with AML_Decode;
with Interfaces;
package AML_Integers with SPARK_Mode, Pure is
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Integer_Width;
   function Supported (Op : AML_Decode.Byte) return Boolean is
     (Op in 16#72# | 16#74# | 16#77# .. 16#82# | 16#85#);
   function Unary (Op : AML_Decode.Byte) return Boolean is (Op in 16#80# .. 16#82#);
   function Bit_Width (Width : AML_Decode.Integer_Width) return Positive is
     (if Width = AML_Decode.Bits_32 then 32 else 64);
   function Normalize
     (Value : AML_Decode.Integer_Value; Width : AML_Decode.Integer_Width)
      return AML_Decode.Integer_Value is
     (if Width = AML_Decode.Bits_32 then Value and 16#FFFF_FFFF# else Value);
   subtype Bit_Position is Natural range 0 .. 64;
   -- Positions are one-based from the least significant bit; zero means no
   -- set bit. Inputs retain their raw integer value, like other AML operands.
   subtype Set_Bit_Position is Bit_Position range 1 .. 64;
   function Bit_Set (Value : AML_Decode.Integer_Value; Position : Set_Bit_Position)
     return Boolean with Post => Bit_Set'Result =
       ((Value and Interfaces.Shift_Left (1, Position - 1)) /= 0);
   function Correct_Position
     (Value : AML_Decode.Integer_Value; Highest : Boolean;
      Position, Through_Bit : Bit_Position) return Boolean is
     (Position <= Through_Bit
      and then (if Position /= 0 then
        Bit_Set (Value, Position))
      and then (for all I in 1 .. Through_Bit =>
        (if Bit_Set (Value, I) then
          Position /= 0 and then (if Highest then Position >= I else Position <= I))));
   function Find_Set (Value : AML_Decode.Integer_Value; Highest : Boolean)
     return Bit_Position with
     Post => Correct_Position (Value, Highest, Find_Set'Result, 64);
   --  Ada Unsigned_64 operations specify modulo 2**64 arithmetic. Applying
   --  the 32-bit mask specifies modulo 2**32 results for a legacy DSDT.
   --  This contract is for integer operands only, not AML coercion/targets.
   --  Unary operators take their operand in Right. Shift counts are not
   --  masked; counts at least the table bit width produce zero.
   function Apply
     (Op : AML_Decode.Byte; Left, Right : AML_Decode.Integer_Value;
      Width : AML_Decode.Integer_Width) return AML_Decode.Integer_Value
     with Pre => Supported (Op) and then (if Op in 16#78# | 16#85# then Right /= 0),
          Post =>
            Apply'Result = Normalize
              ((case Op is
                  when 16#72# => Left + Right,
                  when 16#74# => Left - Right,
                  when 16#77# => Left * Right,
                  when 16#78# => Left / Right,
                  when 16#85# => Left mod Right,
                  when 16#79# =>
                    (if Right >= AML_Decode.Integer_Value (Bit_Width (Width)) then 0
                     else Interfaces.Shift_Left (Left, Natural (Right))),
                  when 16#7A# =>
                    (if Right >= AML_Decode.Integer_Value (Bit_Width (Width)) then 0
                     else Interfaces.Shift_Right (Left, Natural (Right))),
                  when 16#7B# => Left and Right,
                  when 16#7C# => not (Left and Right),
                  when 16#7D# => Left or Right,
                  when 16#7E# => not (Left or Right),
                  when 16#80# => not Right,
                  when 16#81# => AML_Decode.Integer_Value (Find_Set (Right, True)),
                  when 16#82# => AML_Decode.Integer_Value (Find_Set (Right, False)),
                  when others => Left xor Right), Width)
            and then (if Width = AML_Decode.Bits_32 then
                        Apply'Result <= 16#FFFF_FFFF#);
end AML_Integers;

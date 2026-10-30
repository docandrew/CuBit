with AML_Coercions.Strings;
package body AML_Explicit_Formatting with SPARK_Mode is
   use type AML_Decode.Byte;
   ASCII_Zero : constant AML_Decode.Byte := Character'Pos ('0');
   ASCII_Nine : constant AML_Decode.Byte := Character'Pos ('9');
   ASCII_A : constant AML_Decode.Byte := Character'Pos ('A');
   ASCII_F : constant AML_Decode.Byte := Character'Pos ('F');
   Prefix_X : constant AML_Decode.Byte := Character'Pos ('x');
   Comma : constant AML_Decode.Byte := Character'Pos (',');
   Two_Digit_Threshold : constant AML_Decode.Byte := 10;
   Three_Digit_Threshold : constant AML_Decode.Byte := 100;
   subtype Digit_Count is Positive range 1 .. Max_Integer_Decimal_Digits;
   function Digit_Length (Value : AML_Decode.Integer_Value; Base : AML_Decode.Integer_Value) return Digit_Count
     with Pre => Base in Decimal_Radix | Hexadecimal_Radix
   is
      N : AML_Decode.Integer_Value := Value;
      Count : Digit_Count := 1;
   begin
      while N >= Base loop N := N / Base; Count := Count + 1; end loop;
      return Count;
   end Digit_Length;
   function Integer_Length (Mode : Format_Mode; Width : AML_Decode.Integer_Width;
      Value : AML_Decode.Integer_Value) return Positive is
     (if Mode = Decimal_Format then Digit_Length (Width_Value (Value, Width), Decimal_Radix)
      else Hex_Prefix_Length + Digit_Length (Width_Value (Value, Width), Hexadecimal_Radix));
   function Buffer_Length (Mode : Format_Mode; Data : AML_Decode.Bytes) return Natural is
      Length : Natural := 0;
   begin
      if Data'Length = 0 then return 0; end if;
      if Mode = Hexadecimal_Format then return Data'Length * Max_Encoded_Byte_Width - Separator_Length; end if;
      for B of Data loop
         Length := Length + (if B >= Three_Digit_Threshold then Max_Decimal_Byte_Digits elsif B >= Two_Digit_Threshold then Max_Decimal_Byte_Digits - 1 else 1);
      end loop;
      return Length + (Data'Length - Separator_Length);
   end Buffer_Length;
   procedure Emit (Value : AML_Decode.Integer_Value; Mode : Format_Mode; Data : out AML_Decode.Bytes) is
      N : AML_Decode.Integer_Value := Value;
      Base : constant AML_Decode.Integer_Value := (if Mode = Decimal_Format then Decimal_Radix else Hexadecimal_Radix);
   begin
      for I in reverse Data'Range loop
         Data (I) := AML_Coercions.Strings.Hex (AML_Decode.Byte (N mod Base));
         N := N / Base;
      end loop;
   end Emit;
   function From_Integer (Mode : Format_Mode; Width : AML_Decode.Integer_Width;
      Value : AML_Decode.Integer_Value) return Result
   is
      Length : constant Natural := Integer_Length (Mode, Width, Value);
   begin
      if Length > Max_Output_Length then return (Status => Length_Limit, Length => 0); end if;
      return R : Result (Built, Length) do
         if Mode = Hexadecimal_Format then
            R.Data (1) := ASCII_Zero; R.Data (Hex_Prefix_Length) := Prefix_X;
            Emit (Width_Value (Value, Width), Mode, R.Data (Hex_Prefix_Length + 1 .. Length));
         else Emit (Width_Value (Value, Width), Mode, R.Data); end if;
      end return;
   end From_Integer;
   function From_Buffer (Mode : Format_Mode; Data : AML_Decode.Bytes) return Result is
      Length : constant Natural := Buffer_Length (Mode, Data);
      Cursor : Natural := 0;
   begin
      if Length > Max_Output_Length then return (Status => Length_Limit, Length => 0); end if;
      return R : Result (Built, Length) do
         for I in Data'Range loop
            declare
               B : constant AML_Decode.Byte := Data (I);
               Count : constant Positive :=
                 (if Mode = Hexadecimal_Format then Hex_Byte_Digits elsif B >= Three_Digit_Threshold then Max_Decimal_Byte_Digits elsif B >= Two_Digit_Threshold then Max_Decimal_Byte_Digits - 1 else 1);
            begin
               if Cursor /= 0 then Cursor := Cursor + 1; R.Data (Cursor) := Comma; end if;
               if Mode = Hexadecimal_Format then
                  R.Data (Cursor + 1) := ASCII_Zero; R.Data (Cursor + Hex_Prefix_Length) := Prefix_X; Cursor := Cursor + Hex_Prefix_Length;
               end if;
               Emit (AML_Decode.Integer_Value (B), Mode, R.Data (Cursor + 1 .. Cursor + Count));
               Cursor := Cursor + Count;
            end;
         end loop;
      end return;
   end From_Buffer;
   -- Recognition uses forward accumulation, unlike reverse quotient emission.
   -- A token must end at end-of-input or a comma. Canonical leading zeros and
   -- the lowercase prefix / uppercase digits are checked independently.
   procedure Read_Token (Mode : Format_Mode; Fixed_Hex : Boolean;
      Data : AML_Decode.Bytes; Cursor : in out Natural;
      Value : out AML_Decode.Integer_Value; Valid : out Boolean)
     with Ghost, Pre => Cursor <= Data'Length
   is
      Count : Natural := 0;
      Leading_Zero : Boolean := False;
      Digit : AML_Decode.Integer_Value;
      B : AML_Decode.Byte;
      Base : constant AML_Decode.Integer_Value := (if Mode = Decimal_Format then Decimal_Radix else Hexadecimal_Radix);
   begin
      Value := 0; Valid := False;
      if Mode = Hexadecimal_Format then
         if Data'Length - Cursor < Hex_Prefix_Length then return; end if;
         if Data (Data'First + Cursor) /= ASCII_Zero or else Data (Data'First + (Cursor + 1)) /= Prefix_X then return; end if;
         Cursor := Cursor + Hex_Prefix_Length;
      end if;
      while Cursor < Data'Length loop
         B := Data (Data'First + Cursor);
         exit when B = Comma;
         if B in ASCII_Zero .. ASCII_Nine then Digit := AML_Decode.Integer_Value (B - ASCII_Zero);
         elsif Mode = Hexadecimal_Format and then B in ASCII_A .. ASCII_F then Digit := AML_Decode.Integer_Value (B - ASCII_A) + Decimal_Radix;
         else return; end if;
         if Count = 0 then Leading_Zero := Digit = 0; end if;
         if Value > (AML_Decode.Integer_Value'Last - Digit) / Base then return; end if;
         Value := Value * Base + Digit; Count := Count + 1; Cursor := Cursor + 1;
      end loop;
      if Fixed_Hex and then Mode = Hexadecimal_Format then Valid := Count = Hex_Byte_Digits;
      else Valid := Count > 0 and then (Count = 1 or else not Leading_Zero); end if;
   end Read_Token;
   function Integer_Encoding (Mode : Format_Mode; Width : AML_Decode.Integer_Width;
      Value : AML_Decode.Integer_Value; Data : AML_Decode.Bytes) return Boolean
   is
      Cursor : Natural := 0;
      Number : AML_Decode.Integer_Value;
      Valid : Boolean;
   begin
      Read_Token (Mode, False, Data, Cursor, Number, Valid);
      return Valid and then Cursor = Data'Length and then Number = Width_Value (Value, Width);
   end Integer_Encoding;
   function Buffer_Encoding (Mode : Format_Mode; Source, Data : AML_Decode.Bytes) return Boolean is
      Cursor : Natural := 0;
      Number : AML_Decode.Integer_Value;
      Valid : Boolean;
   begin
      for I in Source'Range loop
         if I /= Source'First then
            if Cursor = Data'Length or else Data (Data'First + Cursor) /= Comma then return False; end if;
            Cursor := Cursor + 1;
         end if;
         Read_Token (Mode, True, Data, Cursor, Number, Valid);
         if not Valid or else Number /= AML_Decode.Integer_Value (Source (I)) then return False; end if;
      end loop;
      return Cursor = Data'Length;
   end Buffer_Encoding;
end AML_Explicit_Formatting;

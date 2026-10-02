pragma Ada_2022;
package body AML_Coercions with SPARK_Mode is
   use AML_Decode;
   function Maximum (Width : Integer_Width) return Integer_Value is
     (if Width = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last);
   function From_String (Data : Bytes; Width : Integer_Width) return Result is
      Offset : Natural := 0;
      Value : Integer_Value := 0;
      Digit : Integer_Value range 0 .. 15;
      B : Byte;
   begin
      while Offset < Data'Length and then Data (Data'First + Offset) in 9 .. 13 | 32 loop
         pragma Loop_Invariant (Offset <= Data'Length);
         pragma Loop_Variant (Decreases => Data'Length - Offset);
         Offset := Offset + 1;
      end loop;
      if Data'Length - Offset >= 2 and then Data (Data'First + Offset) = 48
        and then Data (Data'First + (Offset + 1)) in 88 | 120 then
         Offset := Offset + 2;
      end if;
      while Offset < Data'Length loop
         pragma Loop_Invariant (Offset <= Data'Length);
         pragma Loop_Invariant (Value <= Maximum (Width));
         pragma Loop_Variant (Decreases => Data'Length - Offset);
         B := Data (Data'First + Offset);
         case B is
            when 48 .. 57 => Digit := Integer_Value (B - 48);
            when 65 .. 70 => Digit := Integer_Value (B - 55);
            when 97 .. 102 => Digit := Integer_Value (B - 87);
            when others => exit;
         end case;
         exit when Value > Maximum (Width) / 16;
         Value := Value * 16 + Digit;
         Offset := Offset + 1;
      end loop;
      return (Status => Converted, Value => Value);
   end From_String;
   function From_Buffer (Data : Bytes; Width : Integer_Width) return Result is
      Count : constant Natural := Buffer_Count (Data'Length, Width);
      Value : Integer_Value := 0;
   begin
      if Data'Length = 0 then return (Status => Empty_Buffer, Value => 0); end if;
      for I in Byte_Index loop
         if I < Count then
            Value := Value or Interfaces.Shift_Left (Integer_Value (Data (Data'First + I)), I * 8);
         end if;
      end loop;
      return (Status => Converted, Value => Value);
   end From_Buffer;
end AML_Coercions;

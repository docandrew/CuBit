pragma Ada_2022;
package body ACPI_Resource_Data.Clock_Inputs with SPARK_Mode is
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   Clock_Name : constant Item_Name := 19;
   Revision_One : constant Firmware_Tables.Byte := 1;
   Fixed_Bytes : constant Byte_Count := 13;
   Revision_Field : constant Byte_Offset := 3;
   Flags_Field : constant Byte_Offset := 4;
   Divisor_Field : constant Byte_Offset := 6;
   Numerator_Field : constant Byte_Offset := 8;
   Source_Index_Field : constant Byte_Offset := 12;
   Variable_Bit : constant Interfaces.Unsigned_16 := 1;
   Reserved_Flags : constant Interfaces.Unsigned_16 := 16#FFF0#;
   Scale_Divisor : constant Interfaces.Unsigned_16 := 2;
   Scale_Mask : constant Interfaces.Unsigned_16 := 7;
   Word_Radix : constant Interfaces.Unsigned_16 := 256;
   Dword_Radix : constant Interfaces.Unsigned_32 := 256;
   Hertz_Multiplier : constant array (Frequency_Scale) of Hertz_Numerator := [1,1_000,1_000_000];
   function Octet (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Firmware_Tables.Byte with Pre => Position < Byte_Count (Data'Length);
   function Octet (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Firmware_Tables.Byte is (Data (Data'First + Natural (Position)));
   function Word (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_16 with
      Pre => Position <= Byte_Count (Data'Length) and then Byte_Count (Data'Length) - Position >= 2;
   function Word (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_16 is
     (Interfaces.Unsigned_16 (Octet (Data, Position)) + Word_Radix * Interfaces.Unsigned_16 (Octet (Data, Position + 1)));
   function Dword (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_32 with
      Pre => Position <= Byte_Count (Data'Length) and then Byte_Count (Data'Length) - Position >= 4;
   function Dword (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_32 is
     (Interfaces.Unsigned_32 (Octet (Data, Position))
      + Dword_Radix * Interfaces.Unsigned_32 (Octet (Data, Position + 1))
      + Dword_Radix ** 2 * Interfaces.Unsigned_32 (Octet (Data, Position + 2))
      + Dword_Radix ** 3 * Interfaces.Unsigned_32 (Octet (Data, Position + 3)));
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result is
      Item : constant Frame := Read_Frame (Data, Offset);
      Flags, Scale, Divisor : Interfaces.Unsigned_16;
      Source : Byte_Span;
      Index : Firmware_Tables.Byte;
   begin
      if Item.Status /= ACPI_Resource_Data.Decoded then return (Status => Malformed_Frame); end if;
      if Item.Encoding /= Large_Item or else Item.Name /= Clock_Name then return (Status => Unsupported_Descriptor); end if;
      if Byte_Count (Item.Length) < Fixed_Bytes - Byte_Count (Large_Header_Bytes) then return (Status => Bad_Length); end if;
      if Octet (Data, Offset + Revision_Field) /= Revision_One then return (Status => Unsupported_Revision); end if;
      Flags := Word (Data, Offset + Flags_Field);
      if (Flags and Reserved_Flags) /= 0 then return (Status => Reserved_Bits_Set); end if;
      Scale := (Flags / Scale_Divisor) and Scale_Mask;
      if Scale > Frequency_Scale'Pos (Frequency_Scale'Last) then return (Status => Reserved_Scale); end if;
      Divisor := Word (Data, Offset + Divisor_Field);
      if Divisor = 0 then return (Status => Zero_Divisor); end if;
      Source := (First => Offset + Fixed_Bytes, Length => Item.Next_Offset - Offset - Fixed_Bytes);
      Index := Octet (Data, Offset + Source_Index_Field);
      if Source.Length > 0 then
         for Position in Byte_Offset range Source.First .. Item.Next_Offset - 1 loop
            if (Octet (Data, Position) = 0) /= (Position = Item.Next_Offset - 1) then
               return (Status => Bad_Source_Termination);
            end if;
         end loop;
      end if;
      if Source.Length <= 1 then
         if (Flags and Variable_Bit) /= 0 then return (Status => Missing_Source); end if;
         if Index /= 0 then return (Status => Bad_Source_Index); end if;
      end if;
      return (Status => Decoded, Value =>
        (Mode => (if (Flags and Variable_Bit) = 0 then Fixed_Frequency else Variable_Frequency),
         Scale => Frequency_Scale'Val (Scale), Numerator => Frequency_Numerator (Dword (Data, Offset + Numerator_Field)),
         Divisor => Frequency_Divisor (Divisor), Source_Index => Index, Source => Source), Next_Offset => Item.Next_Offset);
   end Decode;
   function In_Hertz (Value : Clock_Description) return Rational_Hertz is
     ((Numerator => Hertz_Numerator (Value.Numerator) * Hertz_Multiplier (Value.Scale), Divisor => Value.Divisor));
end ACPI_Resource_Data.Clock_Inputs;

pragma Ada_2022;
with Interfaces;
package ACPI_Resource_Data.Clock_Inputs with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   type Frequency_Scale is (Hertz, Kilohertz, Megahertz);
   type Clock_Mode is (Fixed_Frequency, Variable_Frequency);
   type Frequency_Numerator is new Interfaces.Unsigned_32;
   type Frequency_Divisor is new Interfaces.Unsigned_16 range 1 .. Interfaces.Unsigned_16'Last;
   type Hertz_Numerator is new Interfaces.Unsigned_64 range 0 .. 4_294_967_295_000_000;
   type Rational_Hertz is record
      Numerator : Hertz_Numerator;
      Divisor : Frequency_Divisor;
   end record;
   type Byte_Span is record
      First : Byte_Offset := 0;
      Length : Byte_Count := 0;
   end record;
   type Clock_Description is record
      Mode : Clock_Mode;
      Scale : Frequency_Scale;
      Numerator : Frequency_Numerator;
      Divisor : Frequency_Divisor;
      Source_Index : Firmware_Tables.Byte;
      -- Includes final NUL when present; zero/one-byte names have no source.
      Source : Byte_Span;
   end record;
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor,
      Bad_Length, Unsupported_Revision, Reserved_Bits_Set, Reserved_Scale,
      Zero_Divisor, Bad_Source_Termination, Missing_Source, Bad_Source_Index);
   type Decode_Result (Status : Decode_Status := Unsupported_Descriptor) is record
      case Status is
         when Decoded => Value : Clock_Description; Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- Revision one; the fixed fields include the source-index octet even when
   -- the optional source string is absent. Variable clocks require a nonempty
   -- source; fixed empty/absent sources require index0. Name syntax, ownership,
   -- source clock-output count and physical feasibility are separate admission.
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result with Global => null,
      Post => (if Decode'Result.Status = Decoded then
         Decode'Result.Next_Offset > Offset
         and then Decode'Result.Next_Offset <= Byte_Count (Data'Length)
         and then Decode'Result.Value.Source.First > Offset
         and then Decode'Result.Value.Source.First + Decode'Result.Value.Source.Length = Decode'Result.Next_Offset);
   -- Exact fraction; never integer-divide away the fractional frequency.
   function In_Hertz (Value : Clock_Description) return Rational_Hertz with Global => null,
      Post => In_Hertz'Result.Divisor = Value.Divisor
        and then Interfaces.Unsigned_64 (In_Hertz'Result.Numerator) =
          Interfaces.Unsigned_64 (Value.Numerator) *
            (case Value.Scale is when Hertz => 1, when Kilohertz => 1_000, when Megahertz => 1_000_000);
end ACPI_Resource_Data.Clock_Inputs;

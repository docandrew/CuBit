pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with ACPI_Resource_Data.Clock_Inputs;
procedure Resource_Clock_Tests is
   use type Firmware_Tables.Bytes;
   package C renames ACPI_Resource_Data.Clock_Inputs;
   use type C.Decode_Status;
   use type C.Clock_Mode;
   use type C.Frequency_Scale;
   use type C.Frequency_Numerator;
   use type C.Frequency_Divisor;
   use type C.Hertz_Numerator;
   subtype Bytes is Firmware_Tables.Bytes;
   Named : constant Bytes := [16#93#,15,0,1,5,0,3,0,100,0,0,0,7,67,76,75,48,0];
   Bare : constant Bytes := [16#93#,10,0,1,4,0,3,0,100,0,0,0,0];
   Checks : Natural := 0;
   procedure Verify (Good : Boolean; Name : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Name; end if; end Verify;
   procedure Put_Word (Data : in out Bytes; Offset, Value : Natural) is
   begin Data (Data'First + Offset) := Firmware_Tables.Byte (Value mod 256);
      Data (Data'First + Offset + 1) := Firmware_Tables.Byte (Value / 256); end Put_Word;
   procedure Put_Dword (Data : in out Bytes; Offset : Natural; Value : Unsigned_32) is
   begin for I in Natural range 0 .. 3 loop
      Data (Data'First + Offset + I) := Firmware_Tables.Byte (Shift_Right (Value, I * 8) and 255); end loop; end Put_Dword;
begin
   Verify (C.Decode ([], 0).Status = C.Malformed_Frame, "empty");
   Verify (C.Decode ([16#79#,0], 0).Status = C.Unsupported_Descriptor, "other descriptor");
   Verify (C.Decode (Named, Byte_Offset'Last).Status = C.Malformed_Frame, "offset beyond input");
   for Size in Natural range 0 .. Named'Length - 1 loop
      Verify (C.Decode (Named (1 .. Size), 0).Status = C.Malformed_Frame, "every truncated prefix");
   end loop;
   for Size in Natural range 0 .. 9 loop
      declare Data : Bytes (1 .. Size + 3) := [others => 0]; begin
         Data (1) := 16#93#; Data (2) := Firmware_Tables.Byte (Size);
         Verify (C.Decode (Data, 0).Status = C.Bad_Length, "fixed fields include source-index byte");
      end;
   end loop;
   declare R : constant C.Decode_Result := C.Decode (Named, 0); begin
      Verify (R.Status = C.Decoded and then R.Value.Mode = C.Variable_Frequency and then R.Value.Scale = C.Megahertz
        and then R.Value.Divisor = 3 and then R.Value.Numerator = 100 and then R.Value.Source_Index = 7
        and then R.Value.Source.First = 13 and then R.Value.Source.Length = 5 and then R.Next_Offset = 18,
        "named clock fields and spans");
      declare Hz : constant C.Rational_Hertz := C.In_Hertz (R.Value); begin
         Verify (Hz.Numerator = 100_000_000 and then Hz.Divisor = 3, "fraction preserved without division");
      end;
   end;
   Verify (C.Decode (Bare, 0).Status = C.Decoded and then C.Decode (Bare, 0).Value.Source.Length = 0,
     "fixed clock with absent source");
   for Value in Natural range 0 .. 65_535 loop
      declare Data : Bytes := Named; begin
         Put_Word (Data, 4, Value);
         declare R : constant C.Decode_Result := C.Decode (Data, 0); begin
            Verify (R.Status = (if Value >= 16 then C.Reserved_Bits_Set
              elsif Value >= 6 then C.Reserved_Scale else C.Decoded), "every flag and scale word");
            if R.Status = C.Decoded then
               Verify (R.Value.Mode = (if Value mod 2 = 0 then C.Fixed_Frequency else C.Variable_Frequency)
                 and then R.Value.Scale = C.Frequency_Scale'Val (Value / 2), "mode and scale fields");
            end if;
         end;
         Data := Named; Put_Word (Data, 6, Value);
         declare R : constant C.Decode_Result := C.Decode (Data, 0); begin
            Verify (R.Status = (if Value = 0 then C.Zero_Divisor else C.Decoded), "every divisor");
            if R.Status = C.Decoded then
               Verify (R.Value.Divisor = C.Frequency_Divisor (Value)
                 and then C.In_Hertz (R.Value).Divisor = R.Value.Divisor
                 and then C.In_Hertz (R.Value).Numerator = 100_000_000, "exact rational denominator");
            end if;
         end;
      end;
   end loop;
   for Value in Firmware_Tables.Byte loop
      declare Data : Bytes := Named; Unnamed : Bytes := Bare; begin
         Data (4) := Value;
         Verify (C.Decode (Data, 0).Status = (if Value = 1 then C.Decoded else C.Unsupported_Revision), "all revisions");
         Data := Named; Data (13) := Value;
         Verify (C.Decode (Data, 0).Status = C.Decoded, "named source index is controller-relative");
         Unnamed (13) := Value;
         Verify (C.Decode (Unnamed, 0).Status = (if Value = 0 then C.Decoded else C.Bad_Source_Index), "absent source index must be zero");
         Unnamed (5) := 5;
         Verify (C.Decode (Unnamed, 0).Status = C.Missing_Source, "variable clock needs named source");
      end;
   end loop;
   for Scale in C.Frequency_Scale loop
      for Bit in Natural range 0 .. 31 loop
         for Invert in Boolean loop
            declare
               Data : Bytes := Named;
               Value : constant Unsigned_32 := (if Invert then not Shift_Left (1, Bit) else Shift_Left (1, Bit));
               Multiplier : constant Unsigned_64 := (case Scale is when C.Hertz => 1, when C.Kilohertz => 1_000, when C.Megahertz => 1_000_000);
            begin
               Data (5) := Firmware_Tables.Byte (C.Frequency_Scale'Pos (Scale) * 2 + 1); Put_Dword (Data, 8, Value);
               declare R : constant C.Decode_Result := C.Decode (Data, 0); begin
                  Verify (R.Status = C.Decoded and then R.Value.Numerator = C.Frequency_Numerator (Value)
                    and then Unsigned_64 (C.In_Hertz (R.Value).Numerator) = Unsigned_64 (Value) * Multiplier,
                    "every numerator bit and unit multiplier");
               end;
            end;
         end loop;
      end loop;
      for Maximum in Boolean loop
         declare
            Data : Bytes := Named;
            Value : constant Unsigned_32 := (if Maximum then Unsigned_32'Last else 0);
            Multiplier : constant Unsigned_64 := (case Scale is when C.Hertz => 1, when C.Kilohertz => 1_000, when C.Megahertz => 1_000_000);
         begin
            Data (5) := Firmware_Tables.Byte (C.Frequency_Scale'Pos (Scale) * 2); Put_Dword (Data, 8, Value);
            declare R : constant C.Decode_Result := C.Decode (Data, 0); begin
               Verify (R.Status = C.Decoded and then Unsigned_64 (C.In_Hertz (R.Value).Numerator) = Unsigned_64 (Value) * Multiplier,
                 "zero and full-width numerator");
            end;
         end;
      end loop;
   end loop;
   for Position in Positive range 14 .. 18 loop
      declare Data : Bytes := Named; begin
         Data (Position) := (if Position = Data'Last then 1 else 0);
         Verify (C.Decode (Data, 0).Status = C.Bad_Source_Termination, "missing or early source NUL");
      end;
   end loop;
   declare Empty_Name : Bytes (1 .. 14) := Bare & [0]; begin
      Empty_Name (2) := 11;
      Verify (C.Decode (Empty_Name, 0).Status = C.Decoded, "fixed explicit empty source");
      Empty_Name (5) := 5;
      Verify (C.Decode (Empty_Name, 0).Status = C.Missing_Source, "empty name cannot control variable clock");
   end;
   declare Data : Bytes (Positive'Last - Named'Length - 2 .. Positive'Last) := [others => 16#AA#]; begin
      Data (Data'First + 2 .. Data'Last - 1) := Named;
      declare R : constant C.Decode_Result := C.Decode (Data, 2); begin
         Verify (R.Status = C.Decoded and then R.Next_Offset = 20 and then R.Value.Source.First = 15,
           "high input bound and nonzero origin");
      end;
   end;
   declare Data : Bytes (1 .. 65_538) := [others => 65]; begin
      Data (1 .. 13) := Named (1 .. 13); Put_Word (Data, 1, 65_535); Data (Data'Last) := 0;
      declare R : constant C.Decode_Result := C.Decode (Data, 0); begin
         Verify (R.Status = C.Decoded and then R.Value.Source.Length = 65_525 and then R.Next_Offset = 65_538,
           "maximum clock source extent");
      end;
      Data (Data'Last - 1) := 0;
      Verify (C.Decode (Data, 0).Status = C.Bad_Source_Termination, "early NUL in maximum name");
   end;
   Ada.Text_IO.Put_Line ("Resource clock checks" & Checks'Image);
end Resource_Clock_Tests;

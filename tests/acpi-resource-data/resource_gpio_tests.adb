pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with ACPI_Resource_Data.GPIO_Connections;
procedure Resource_GPIO_Tests is
   package G renames ACPI_Resource_Data.GPIO_Connections;
   use type G.Decode_Status;
   use type G.Connection_Kind;
   use type G.Resource_Role;
   use type G.Interrupt_Mode;
   use type G.Interrupt_Polarity;
   use type G.IO_Restriction;
   use type G.Pin_Number;
   use type G.Pin_Configuration;
   use type G.Drive_Hundredths_Milliampere;
   use type G.Debounce_Hundredths_Millisecond;
   subtype Bytes is Firmware_Tables.Bytes;
   Base : constant Bytes := [16#8C#,31,0,1,1,1,0,2,0,2,16#34#,16#12#,16#78#,16#56#,
     23,0,0,25,0,30,0,4,0,16#EF#,16#BE#,71,80,79,48,0,16#DA#,16#DB#,16#DC#,16#DD#];
   Checks : Natural := 0;
   procedure Verify (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Verify;
   procedure Set_Word (Data : in out Bytes; Field : Positive; Value : Natural) is
   begin Data (Field) := Firmware_Tables.Byte (Value mod 256); Data (Field + 1) := Firmware_Tables.Byte (Value / 256); end Set_Word;
begin
   Verify (G.Decode ([], 0).Status = G.Malformed_Frame, "empty");
   Verify (G.Decode ([16#79#,0], 0).Status = G.Unsupported_Descriptor, "wrong descriptor");
   Verify (G.Decode (Base, Byte_Offset'Last).Status = G.Malformed_Frame, "large offset");
   declare R : constant G.Decode_Result := G.Decode (Base, 0); begin
      Verify (R.Status = G.Decoded and then R.Information.Kind = G.IO_Connection
        and then R.Information.Restriction = G.Output_Only and then R.Role = G.Consumes_Resource
        and then not R.Shared and then R.Configuration = 2
        and then R.Drive_Strength = 16#1234# and then R.Debounce_Timeout = 16#5678#
        and then R.Count = 1 and then R.Pins.First = 23 and then R.Pins.Length = 2
        and then R.Source.First = 25 and then R.Source.Length = 5
        and then R.Vendor.First = 30 and then R.Vendor.Length = 4 and then R.Next_Offset = 34,
        "fixed fields and absolute spans");
      Verify (G.Read_Pin (Base, 0, 1).Available and then G.Read_Pin (Base, 0, 1).Value = 16#BEEF#,
        "little-endian pin");
      Verify (not G.Read_Pin (Base, 0, 2).Available and then not G.Read_Pin (Base, 0, Positive'Last).Available,
        "pin index limits");
   end;
   for Size in 0 .. Base'Length - 1 loop
      Verify (G.Decode (Base (1 .. Size), 0).Status = G.Malformed_Frame, "every truncated prefix");
   end loop;
   for Value in Natural range 0 .. 65_535 loop
      declare Data : Bytes := Base; begin
         Set_Word (Data, 6, Value);
         declare R : constant G.Decode_Result := G.Decode (Data, 0); begin
            Verify (R.Status = (if Value <= 1 then G.Decoded else G.Reserved_Bits_Set), "all general flags");
            if R.Status = G.Decoded then
               Verify (R.Role = (if Value = 0 then G.Produces_And_Consumes else G.Consumes_Resource), "role");
            end if;
         end;
         for Kind in Natural range 0 .. 1 loop
            Data := Base; Data (5) := Firmware_Tables.Byte (Kind); Set_Word (Data, 8, Value);
            declare
               R : constant G.Decode_Result := G.Decode (Data, 0);
               Expected : constant G.Decode_Status :=
                 (if Kind = 1 then (if Value in 0 .. 3 | 8 .. 11 then G.Decoded else G.Reserved_Bits_Set)
                  elsif Value >= 32 then G.Reserved_Bits_Set
                  elsif (Value / 2) mod 4 = 3 or else ((Value / 2) mod 4 = 2 and then Value mod 2 = 0)
                    then G.Bad_Interrupt_Polarity else G.Decoded);
            begin
               Verify (R.Status = Expected, "all connection flags and polarity combinations");
               if R.Status = G.Decoded then
                  Verify (R.Shared = ((Value / 8) mod 2 = 1), "sharing");
                  if Kind = 0 then
                     Verify (R.Information.Mode = (if Value mod 2 = 0 then G.Level_Triggered else G.Edge_Triggered)
                       and then R.Information.Polarity = G.Interrupt_Polarity'Val ((Value / 2) mod 4)
                       and then R.Information.Wake_Capable = ((Value / 16) mod 2 = 1), "interrupt fields");
                  else Verify (R.Information.Restriction = G.IO_Restriction'Val (Value mod 4), "IO restriction"); end if;
               end if;
            end;
         end loop;
         Data := Base; Set_Word (Data, 24, Value);
         declare R : constant G.Pin_Result := G.Read_Pin (Data, 0, 1); begin
            Verify (R.Available and then R.Value = G.Pin_Number (Value), "every pin including No_Pin");
         end;
         Data := Base; Set_Word (Data, 11, Value); Set_Word (Data, 13, 65_535 - Value);
         declare R : constant G.Decode_Result := G.Decode (Data, 0); begin
            Verify (R.Status = G.Decoded and then R.Drive_Strength = G.Drive_Hundredths_Milliampere (Value)
              and then R.Debounce_Timeout = G.Debounce_Hundredths_Millisecond (65_535 - Value), "all raw unit values");
         end;
         for Field in Positive range 1 .. 4 loop
            Data := Base;
            Set_Word (Data, (case Field is when 1 => 15, when 2 => 18, when 3 => 20, when 4 => 22), Value);
            declare
               R : constant G.Decode_Result := G.Decode (Data, 0);
               Expected : constant G.Decode_Status :=
                 (case Field is
                    when 1 => (if Value < 23 or else Value > 25 then G.Bad_Offsets
                      elsif Value = 23 then G.Decoded else G.Bad_Pin_Count),
                    when 2 => (if Value < 23 or else Value > 30 then G.Bad_Offsets
                      elsif Value < 25 or else Value mod 2 = 0 then G.Bad_Pin_Count else G.Decoded),
                    when 3 => (if Value = 30 then G.Decoded else G.Bad_Offsets),
                    when 4 => (if Value = 4 then G.Decoded else G.Bad_Offsets));
            begin Verify (R.Status = Expected, "independent offset partition matrix"); end;
         end loop;
      end;
   end loop;
   for Value in Firmware_Tables.Byte loop
      declare Data : Bytes := Base; begin
         Data (4) := Value;
         Verify (G.Decode (Data, 0).Status = (if Value = 1 then G.Decoded else G.Unsupported_Revision), "all revisions");
         Data := Base; Data (5) := Value; Data (8) := 1;
         Verify (G.Decode (Data, 0).Status = (if Value <= 1 then G.Decoded else G.Reserved_Connection_Type), "all kinds");
         Data := Base; Data (10) := Value;
         Verify (G.Decode (Data, 0).Status = (if Value <= 3 or else Value >= 128 then G.Decoded else G.Reserved_Configuration), "configuration codes");
         Data := Base; Data (17) := Value;
         Verify (G.Decode (Data, 0).Status = (if Value = 0 then G.Decoded else G.Bad_Source_Index), "source index reserved");
      end;
   end loop;
   -- Alternate base index, descriptor prefix and descriptor-local extent.
   declare Data : Bytes (Positive'Last - Base'Length - 2 .. Positive'Last) := [others => 16#AA#]; begin
      Data (Data'First + 2 .. Data'Last - 1) := Base;
      declare R : constant G.Decode_Result := G.Decode (Data, 2); begin
         Verify (R.Status = G.Decoded and then R.Pins.First = 25 and then R.Source.First = 27
           and then R.Vendor.First = 32 and then R.Next_Offset = 36, "absolute spans at high input bound");
         Verify (G.Read_Pin (Data, 2, 1).Available and then G.Read_Pin (Data, 2, 1).Value = 16#BEEF#, "high-bound pin");
      end;
   end;
   -- Source bytes are opaque, including absent termination; vendor extent is exact.
   declare Data : Bytes := Base; begin
      Data (30) := 16#FF#;
      Verify (G.Decode (Data, 0).Status = G.Decoded, "opaque source bytes retained");
      Set_Word (Data, 20, 25); Set_Word (Data, 22, 9);
      Verify (G.Decode (Data, 0).Status = G.Decoded, "zero-length source structurally retained");
      Data := Base; Set_Word (Data, 18, 27);
      Verify (G.Decode (Data, 0).Status = G.Decoded and then G.Read_Pin (Data, 0, 2).Available
        and then G.Read_Pin (Data, 0, 2).Value = 16#5047#, "caller-sized IO list");
      Data (5) := 0; Data (8) := 1;
      Verify (G.Decode (Data, 0).Status = G.Bad_Pin_Count, "interrupt requires single pin");
   end;
   Verify (not G.Read_Pin ([], 0, 1).Available, "invalid descriptor pin read");
   declare Data : Bytes (1 .. 65_538) := [others => 0]; begin
      Data (1 .. 23) := Base (1 .. 23);
      Set_Word (Data, 2, 65_535); Set_Word (Data, 18, 65_535);
      Set_Word (Data, 20, 65_535); Set_Word (Data, 22, 3);
      Set_Word (Data, 24, 16#1234#); Set_Word (Data, 65_534, 16#FFFF#);
      declare R : constant G.Decode_Result := G.Decode (Data, 0); begin
         Verify (R.Status = G.Decoded and then R.Count = 32_756 and then R.Next_Offset = 65_538,
           "maximum descriptor has no hidden pin cap");
         Verify (G.Read_Pin (Data, 0, 32_756).Available
           and then G.Read_Pin (Data, 0, 32_756).Value = G.No_Pin, "last maximum-list pin");
         Verify (not G.Read_Pin (Data, 0, 32_757).Available, "maximum-list index limit");
      end;
   end;
   Ada.Text_IO.Put_Line ("Resource GPIO checks" & Checks'Image);
end Resource_GPIO_Tests;

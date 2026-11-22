pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with ACPI_Resource_Data.Serial_Connections;
procedure Resource_Serial_Tests is
   package S renames ACPI_Resource_Data.Serial_Connections;
   use type S.Decode_Status;
   use type S.Bus_Kind;
   use type S.Resource_Role;
   use type S.Initiator;
   use type S.Address_Mode;
   use type S.Slave_Address;
   use type S.Rate;
   use type S.Wire_Mode;
   use type S.Signal_Polarity;
   use type S.Clock_Phase;
   use type S.Clock_Polarity;
   use type S.Device_Selection;
   use type S.Transfer_Bits;
   use type S.Flow_Control;
   use type S.Data_Bits;
   use type S.Stop_Bits;
   use type S.Byte_Order;
   use type S.Parity;
   use type S.Serial_Lines;
   use type S.FIFO_Bytes;
   use type S.PHY_Kind;
   use type S.Local_Port;
   subtype Bytes is Firmware_Tables.Bytes;
   Checks : Natural := 0;
   procedure Verify (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Verify;
   procedure Put_Word (Data : in out Bytes; Offset, Value : Natural) is
   begin
      Data (Data'First + Offset) := Firmware_Tables.Byte (Value mod 256);
      Data (Data'First + Offset + 1) := Firmware_Tables.Byte (Value / 256);
   end Put_Word;
   procedure Put_Dword (Data : in out Bytes; Offset : Natural; Value : Unsigned_32) is
   begin
      for Index in Natural range 0 .. 3 loop
         Data (Data'First + Offset + Index) := Firmware_Tables.Byte (Shift_Right (Value, 8 * Index) and 255);
      end loop;
   end Put_Dword;
   function Minimum (Kind : Positive) return Natural is
     (case Kind is when 1 => 6, when 2 => 9, when 3 => 10, when others => 0);
   function Example (Kind : Positive) return Bytes is
      Size : constant Positive := 12 + Minimum (Kind) + 2 + 5;
      Data : Bytes (1 .. Size) := [others => 0];
   begin
      Data (1) := 16#8E#; Put_Word (Data, 1, Size - 3);
      Data (4) := (if Kind = 4 then 1 else 2);
      Data (6) := Firmware_Tables.Byte (Kind); Data (7) := 2; Data (10) := 1;
      Put_Word (Data, 10, Minimum (Kind) + 2);
      if Kind <= 3 then Put_Dword (Data, 12, 16#1234_5678#); end if;
      case Kind is
         when 1 => Put_Word (Data, 16, 16#5A#);
         when 2 => Data (17) := 8; Data (18) := 1; Put_Word (Data, 19, 16#BEEF#);
         when 3 => Data (8) := 16#B9#; Put_Word (Data, 16, 16#1234#);
            Put_Word (Data, 18, 16#FEDC#); Data (21) := 4; Data (22) := 16#FC#;
         when others => Data (8) := 16#45#;
      end case;
      Data (13 + Minimum (Kind)) := 16#DA#; Data (14 + Minimum (Kind)) := 16#DB#;
      Data (Size - 4 .. Size) := [66,85,83,48,0];
      return Data;
   end Example;
begin
   Verify (S.Decode ([], 0).Status = S.Malformed_Frame, "empty");
   Verify (S.Decode ([16#79#,0], 0).Status = S.Unsupported_Descriptor, "other tag");
   for Kind in Positive range 1 .. 4 loop
      declare Data : constant Bytes := Example (Kind); R : constant S.Decode_Result := S.Decode (Data, 0); begin
         Verify (R.Status = S.Decoded and then R.Information.Kind = S.Bus_Kind'Val (Kind - 1)
           and then R.Vendor.First = Byte_Count (12 + Minimum (Kind)) and then R.Vendor.Length = 2
           and then R.Source.First = Byte_Count (14 + Minimum (Kind)) and then R.Source.Length = 5
           and then R.Next_Offset = Byte_Count (Data'Length) and then R.Role = S.Consumes_Resource
           and then R.Initiated_By = S.Controller_Initiated and then not R.Shared, "typed descriptor and spans");
         for Size in Natural range 0 .. Data'Length - 1 loop
            Verify (S.Decode (Data (1 .. Size), 0).Status = S.Malformed_Frame, "all truncated prefixes");
         end loop;
         Verify (S.Decode (Data, Byte_Offset'Last).Status = S.Malformed_Frame, "large offset");
      end;
   end loop;
   declare R : constant S.Decode_Result := S.Decode (Example (1), 0); begin
      Verify (R.Information.Address = 16#5A# and then R.Information.Addressing = S.Seven_Bit
        and then R.Information.Connection_Speed = 16#1234_5678# and then R.Information.Legacy_Virtual_Register = 0,
        "I2C fields");
   end;
   declare R : constant S.Decode_Result := S.Decode (Example (2), 0); begin
      Verify (R.Information.Wires = S.Four_Wire and then R.Information.Selection_Polarity = S.Active_Low
        and then R.Information.Selection = 16#BEEF# and then R.Information.Transfer_Unit = 8
        and then R.Information.Phase = S.Second_Phase and then R.Information.Clock = S.Starts_Low
        and then R.Information.Maximum_Speed = 16#1234_5678#, "SPI fields");
   end;
   declare R : constant S.Decode_Result := S.Decode (Example (3), 0); begin
      Verify (R.Information.Flow = S.Hardware_Flow_Control and then R.Information.Stops = S.One_And_Half_Stop_Bits
        and then R.Information.Bits = S.Eight_Bits and then R.Information.Order = S.Big_Endian
        and then R.Information.Parity_Mode = S.Space_Parity and then R.Information.Lines = 16#FC#
        and then R.Information.Baud_Rate = 16#1234_5678# and then R.Information.Receive_FIFO = 16#1234#
        and then R.Information.Transmit_FIFO = 16#FEDC#, "UART fields");
   end;
   declare R : constant S.Decode_Result := S.Decode (Example (4), 0); begin
      Verify (R.Information.PHY = S.D_PHY and then R.Information.Port = 17, "CSI2 fields");
   end;
   for Value in Natural range 0 .. 65_535 loop
      for Kind in Positive range 1 .. 4 loop
         declare
            Data : Bytes := Example (Kind);
            Expected : constant S.Decode_Status :=
              (case Kind is
                 when 1 => (if Value mod 256 <= 1 then S.Decoded else S.Reserved_Bits_Set),
                 when 2 => (if Value <= 3 then S.Decoded else S.Reserved_Bits_Set),
                 when 3 => (if Value > 255 then S.Reserved_Bits_Set
                   elsif Value mod 4 = 3 or else (Value / 16) mod 8 > 4 then S.Bad_UART_Format else S.Decoded),
                 when others => (if Value > 255 then S.Reserved_Bits_Set
                   elsif Value mod 4 > 1 then S.Bad_CSI2_PHY else S.Decoded));
         begin
            Put_Word (Data, 7, Value);
            declare R : constant S.Decode_Result := S.Decode (Data, 0); begin
               Verify (R.Status = Expected, "all type flags");
               if R.Status = S.Decoded then
                  Verify (R.Specific_Flags = Unsigned_16 (Value), "raw type flags preserved");
                  case Kind is
                     when 1 => Verify (R.Information.Addressing = (if Value mod 2 = 0 then S.Seven_Bit else S.Ten_Bit)
                       and then R.Information.Legacy_Virtual_Register = Firmware_Tables.Byte (Value / 256), "I2C mode/LVR");
                     when 2 => Verify (R.Information.Wires = (if Value mod 2 = 0 then S.Four_Wire else S.Three_Wire)
                       and then R.Information.Selection_Polarity = (if (Value / 2) mod 2 = 0 then S.Active_Low else S.Active_High), "SPI flags");
                     when 3 => Verify (R.Information.Flow = S.Flow_Control'Val (Value mod 4)
                       and then R.Information.Stops = S.Stop_Bits'Val ((Value / 4) mod 4)
                       and then R.Information.Bits = S.Data_Bits'Val ((Value / 16) mod 8)
                       and then R.Information.Order = (if Value < 128 then S.Little_Endian else S.Big_Endian), "UART flags");
                     when others => Verify (R.Information.PHY = S.PHY_Kind'Val (Value mod 4)
                       and then R.Information.Port = S.Local_Port (Value / 4), "CSI flags");
                  end case;
               end if;
            end;
            Data := Example (Kind); Put_Word (Data, 10, Value);
            declare
               Limit : constant Natural := Data'Length - 12;
               R : constant S.Decode_Result := S.Decode (Data, 0);
            begin
               Verify (R.Status = (if Value < Minimum (Kind) or else Value > Limit then S.Bad_Type_Length
                 elsif Value = Limit then S.Missing_Source else S.Decoded), "all type lengths");
               if R.Status = S.Decoded then
                  Verify (R.Vendor.Length = Byte_Count (Value - Minimum (Kind))
                    and then R.Source.First = Byte_Count (12 + Value)
                    and then R.Source.Length = Byte_Count (Limit - Value), "length-derived partitions");
               end if;
            end;
         end;
      end loop;
      for Mode in Natural range 0 .. 1 loop
         declare Data : Bytes := Example (1); begin
            Data (8) := Firmware_Tables.Byte (Mode); Put_Word (Data, 16, Value);
            declare R : constant S.Decode_Result := S.Decode (Data, 0); begin
               Verify (R.Status = (if Value <= (if Mode = 0 then 127 else 1_023) then S.Decoded else S.Bad_I2C_Address),
                 "every I2C address in both modes");
               if R.Status = S.Decoded then Verify (R.Information.Address = S.Slave_Address (Value), "address value"); end if;
            end;
         end;
      end loop;
      declare Data : Bytes := Example (2); begin
         Put_Word (Data, 19, Value);
         declare R : constant S.Decode_Result := S.Decode (Data, 0); begin
            Verify (R.Status = S.Decoded and then R.Information.Selection = S.Device_Selection (Value), "all SPI selections");
         end;
      end;
      declare Data : Bytes := Example (3); begin
         Put_Word (Data, 16, Value); Put_Word (Data, 18, 65_535 - Value);
         declare R : constant S.Decode_Result := S.Decode (Data, 0); begin
            Verify (R.Status = S.Decoded and then R.Information.Receive_FIFO = S.FIFO_Bytes (Value)
              and then R.Information.Transmit_FIFO = S.FIFO_Bytes (65_535 - Value), "all FIFO extents");
         end;
      end;
   end loop;
   for Value in Firmware_Tables.Byte loop
      for Kind in Positive range 1 .. 4 loop
         declare Data : Bytes := Example (Kind); begin
            Data (4) := Value;
            Verify (S.Decode (Data, 0).Status = (if Value = 1 or else (Value = 2 and then Kind /= 4)
              then S.Decoded else S.Unsupported_Revision), "all revisions");
            Data := Example (Kind); Data (10) := Value;
            Verify (S.Decode (Data, 0).Status = (if Value = 1 then S.Decoded else S.Unsupported_Type_Revision), "all type revisions");
            Data := Example (Kind); Data (5) := Value;
            Verify (S.Decode (Data, 0).Status = (if Value = 0 or else Kind in 1 | 4
              then S.Decoded else S.Bad_Source_Index), "source index policy");
            for Revision in Positive range 1 .. (if Kind = 4 then 1 else 2) loop
               Data := Example (Kind); Data (4) := Firmware_Tables.Byte (Revision); Data (7) := Value;
               declare R : constant S.Decode_Result := S.Decode (Data, 0); begin
                  Verify (R.Status = (if Natural (Value) < (if Revision = 1 then 4 else 8)
                    then S.Decoded else S.Reserved_Bits_Set), "V1/V2 all general flags");
                  if R.Status = S.Decoded then
                     Verify (R.Role = (if (Natural (Value) / 2) mod 2 = 0 then S.Produces_And_Consumes else S.Consumes_Resource)
                       and then R.Initiated_By = (if Value mod 2 = 0 then S.Controller_Initiated else S.Device_Initiated)
                       and then R.Shared = ((Natural (Value) / 4) mod 2 = 1), "general flag fields");
                  end if;
               end;
            end loop;
         end;
      end loop;
      declare Data : Bytes := Example (1); begin
         Data (6) := Value;
         Verify (S.Decode (Data, 0).Status = (if Value = 1 or else Value >= 192 then S.Decoded
           elsif Value in 2 .. 3 then S.Bad_Type_Length elsif Value = 4 then S.Unsupported_Revision
           else S.Unsupported_Bus_Type), "all bus codes including opaque vendor");
         if Value >= 192 then
            Data (10) := Value; Put_Word (Data, 7, 65_535);
            declare R : constant S.Decode_Result := S.Decode (Data, 0); begin
               Verify (R.Status = S.Decoded and then R.Information.Kind = S.Vendor_Bus
                 and then R.Bus_Type = Value and then R.Type_Revision = Value and then R.Specific_Flags = 65_535
                 and then R.Vendor.Length = 8, "vendor envelope is opaque");
            end;
         end if;
      end;
      declare Data : Bytes := Example (2); begin
         for Field in Natural range 17 .. 18 loop
            Data := Example (2); Data (Data'First + Field) := Value;
            Verify (S.Decode (Data, 0).Status = (if Value <= 1 then S.Decoded else S.Bad_SPI_Clock), "clock codes");
         end loop;
         Data := Example (2); Data (17) := Value;
         declare R : constant S.Decode_Result := S.Decode (Data, 0); begin
            Verify (R.Status = S.Decoded and then R.Information.Transfer_Unit = S.Transfer_Bits (Value), "raw transfer unit");
         end;
      end;
      declare Data : Bytes := Example (3); begin
         Data (21) := Value;
         Verify (S.Decode (Data, 0).Status = (if Value <= 4 then S.Decoded else S.Bad_UART_Format), "all parity codes");
         Data := Example (3); Data (22) := Value;
         Verify (S.Decode (Data, 0).Status = (if Value mod 4 = 0 then S.Decoded else S.Reserved_Bits_Set), "all UART lines");
      end;
   end loop;
   for Kind in Positive range 1 .. 3 loop
      for Bit in Natural range 0 .. 31 loop
         for Invert in Boolean loop
            declare
               Data : Bytes := Example (Kind);
               Value : constant Unsigned_32 := (if Invert then not Shift_Left (1, Bit) else Shift_Left (1, Bit));
            begin
               Put_Dword (Data, 12, Value);
               declare R : constant S.Decode_Result := S.Decode (Data, 0); begin
                  Verify (R.Status = S.Decoded and then
                    (case Kind is when 1 => R.Information.Connection_Speed = S.Rate (Value),
                     when 2 => R.Information.Maximum_Speed = S.Rate (Value), when others => R.Information.Baud_Rate = S.Rate (Value)),
                    "every speed bit including full-width complements");
               end;
            end;
         end loop;
      end loop;
   end loop;
   for Kind in Positive range 1 .. 4 loop
      declare Base : constant Bytes := Example (Kind); begin
         for Position in Base'Last - 4 .. Base'Last loop
            declare Data : Bytes := Base; begin
               Data (Position) := (if Position = Data'Last then 1 else 0);
               Verify (S.Decode (Data, 0).Status = S.Bad_Source_Termination, "embedded or missing terminator");
            end;
         end loop;
         declare Data : Bytes (Positive'Last - Base'Length - 2 .. Positive'Last) := [others => 16#AA#]; begin
            Data (Data'First + 2 .. Data'Last - 1) := Base;
            declare R : constant S.Decode_Result := S.Decode (Data, 2); begin
               Verify (R.Status = S.Decoded and then R.Source.First = Byte_Count (16 + Minimum (Kind))
                 and then R.Next_Offset = Byte_Count (Base'Length + 2), "high array bound and nonzero offset");
            end;
         end;
      end;
   end loop;
   declare Data : Bytes (1 .. 65_538) := [others => 16#A5#]; begin
      Data (1) := 16#8E#; Put_Word (Data, 1, 65_535); Data (4) := 2;
      Data (5) := 255; Data (6) := 192; Data (7) := 7; Data (10) := 255;
      Put_Word (Data, 10, 65_525); Data (Data'Last) := 0;
      declare R : constant S.Decode_Result := S.Decode (Data, 0); begin
         Verify (R.Status = S.Decoded and then R.Vendor.Length = 65_525 and then R.Source.Length = 1
           and then R.Next_Offset = 65_538, "maximum descriptor and empty terminated name");
      end;
   end;
   Ada.Text_IO.Put_Line ("Resource serial checks" & Checks'Image);
end Resource_Serial_Tests;

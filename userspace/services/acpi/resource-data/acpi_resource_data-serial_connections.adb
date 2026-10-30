pragma Ada_2022;
package body ACPI_Resource_Data.Serial_Connections with SPARK_Mode is
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   Serial_Name : constant Item_Name := 14;
   Common_Length : constant Payload_Length := 9;
   Common_Bytes : constant Byte_Count := 12;
   Revision_Field : constant Byte_Offset := 3;
   Index_Field : constant Byte_Offset := 4;
   Bus_Field : constant Byte_Offset := 5;
   Flags_Field : constant Byte_Offset := 6;
   Specific_Field : constant Byte_Offset := 7;
   Type_Revision_Field : constant Byte_Offset := 9;
   Type_Length_Field : constant Byte_Offset := 10;
   Data_Field : constant Byte_Offset := Common_Bytes;
   I2C_Code : constant Firmware_Tables.Byte := 1;
   SPI_Code : constant Firmware_Tables.Byte := 2;
   UART_Code : constant Firmware_Tables.Byte := 3;
   CSI2_Code : constant Firmware_Tables.Byte := 4;
   First_Vendor_Code : constant Firmware_Tables.Byte := 192;
   Revision_One : constant Firmware_Tables.Byte := 1;
   Revision_Two : constant Firmware_Tables.Byte := 2;
   I2C_Bytes : constant Byte_Count := 6;
   SPI_Bytes : constant Byte_Count := 9;
   UART_Bytes : constant Byte_Count := 10;
   Flag_Initiator : constant Firmware_Tables.Byte := 1;
   Flag_Consumer : constant Firmware_Tables.Byte := 2;
   Flag_Sharing : constant Firmware_Tables.Byte := 4;
   V1_Reserved : constant Firmware_Tables.Byte := 16#FC#;
   V2_Reserved : constant Firmware_Tables.Byte := 16#F8#;
   I2C_Reserved : constant Interfaces.Unsigned_16 := 16#00FE#;
   SPI_Reserved : constant Interfaces.Unsigned_16 := 16#FFFC#;
   Byte_Radix : constant Interfaces.Unsigned_32 := 256;
   Two_Bits : constant Interfaces.Unsigned_16 := 3;
   UART_Bits_Divisor : constant Interfaces.Unsigned_16 := 16;
   UART_Order_Bit : constant Interfaces.Unsigned_16 := 128;
   UART_Lines_Reserved : constant Firmware_Tables.Byte := 3;
   Seven_Bit_Maximum : constant Interfaces.Unsigned_16 := 127;
   Ten_Bit_Maximum : constant Interfaces.Unsigned_16 := 1_023;
   function Octet (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Firmware_Tables.Byte with Pre => Position < Byte_Count (Data'Length);
   function Octet (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Firmware_Tables.Byte is (Data (Data'First + Natural (Position)));
   function Word (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_16 with
      Pre => Position <= Byte_Count (Data'Length) and then Byte_Count (Data'Length) - Position >= 2;
   function Word (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_16 is
     (Interfaces.Unsigned_16 (Octet (Data, Position))
      + 256 * Interfaces.Unsigned_16 (Octet (Data, Position + 1)));
   function Dword (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_32 with
      Pre => Position <= Byte_Count (Data'Length) and then Byte_Count (Data'Length) - Position >= 4;
   function Dword (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_32 is
     (Interfaces.Unsigned_32 (Octet (Data, Position))
      + Byte_Radix * Interfaces.Unsigned_32 (Octet (Data, Position + 1))
      + Byte_Radix ** 2 * Interfaces.Unsigned_32 (Octet (Data, Position + 2))
      + Byte_Radix ** 3 * Interfaces.Unsigned_32 (Octet (Data, Position + 3)));
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result is
      Item : constant Frame := Read_Frame (Data, Offset);
      Revision, Bus_Code, Flags, Type_Revision : Firmware_Tables.Byte;
      Specific : Interfaces.Unsigned_16;
      Minimum, Type_Length, Source_Start, Vendor_Start : Byte_Count;
      Info : Connection_Information;
   begin
      if Item.Status /= ACPI_Resource_Data.Decoded then return (Status => Malformed_Frame); end if;
      if Item.Encoding /= Large_Item or else Item.Name /= Serial_Name then
         return (Status => Unsupported_Descriptor);
      end if;
      if Item.Length < Common_Length then return (Status => Bad_Length); end if;
      Revision := Octet (Data, Offset + Revision_Field);
      Bus_Code := Octet (Data, Offset + Bus_Field);
      if Revision not in Revision_One | Revision_Two
        or else (Bus_Code = CSI2_Code and then Revision /= Revision_One) then
         return (Status => Unsupported_Revision);
      end if;
      if Bus_Code not in I2C_Code | SPI_Code | UART_Code | CSI2_Code
        and then Bus_Code < First_Vendor_Code then return (Status => Unsupported_Bus_Type); end if;
      Type_Revision := Octet (Data, Offset + Type_Revision_Field);
      if Bus_Code < First_Vendor_Code and then Type_Revision /= Revision_One then
         return (Status => Unsupported_Type_Revision);
      end if;
      Flags := Octet (Data, Offset + Flags_Field);
      if (Flags and (if Revision = Revision_One then V1_Reserved else V2_Reserved)) /= 0 then
         return (Status => Reserved_Bits_Set);
      end if;
      if Bus_Code in SPI_Code | UART_Code and then Octet (Data, Offset + Index_Field) /= 0 then
         return (Status => Bad_Source_Index);
      end if;
      Minimum := (case Bus_Code is when I2C_Code => I2C_Bytes, when SPI_Code => SPI_Bytes,
        when UART_Code => UART_Bytes, when others => 0);
      Type_Length := Byte_Count (Word (Data, Offset + Type_Length_Field));
      if Type_Length < Minimum or else Type_Length > Byte_Count (Item.Length - Common_Length) then
         return (Status => Bad_Type_Length);
      end if;
      Source_Start := Offset + Common_Bytes + Type_Length;
      Vendor_Start := Offset + Common_Bytes + Minimum;
      if Source_Start = Item.Next_Offset then return (Status => Missing_Source); end if;
      for Position in Byte_Offset range Source_Start .. Item.Next_Offset - 1 loop
         if (Octet (Data, Position) = 0) /= (Position = Item.Next_Offset - 1) then
            return (Status => Bad_Source_Termination);
         end if;
      end loop;
      Specific := Word (Data, Offset + Specific_Field);
      case Bus_Code is
         when I2C_Code =>
            if (Specific and I2C_Reserved) /= 0 then return (Status => Reserved_Bits_Set); end if;
            declare Address : constant Interfaces.Unsigned_16 := Word (Data, Offset + Data_Field + 4); begin
               if Address > (if (Specific and 1) = 0 then Seven_Bit_Maximum else Ten_Bit_Maximum) then
                  return (Status => Bad_I2C_Address);
               end if;
               Info := (Kind => I2C_Bus, Addressing => (if (Specific and 1) = 0 then Seven_Bit else Ten_Bit),
                 Address => Slave_Address (Address), Connection_Speed => Rate (Dword (Data, Offset + Data_Field)),
                 Legacy_Virtual_Register => Firmware_Tables.Byte (Specific / 256));
            end;
         when SPI_Code =>
            if (Specific and SPI_Reserved) /= 0 then return (Status => Reserved_Bits_Set); end if;
            if Octet (Data, Offset + Data_Field + 5) > 1 or else Octet (Data, Offset + Data_Field + 6) > 1 then
               return (Status => Bad_SPI_Clock);
            end if;
            Info := (Kind => SPI_Bus, Wires => (if (Specific and 1) = 0 then Four_Wire else Three_Wire),
              Selection_Polarity => (if (Specific and 2) = 0 then Active_Low else Active_High),
              Selection => Device_Selection (Word (Data, Offset + Data_Field + 7)),
              Transfer_Unit => Transfer_Bits (Octet (Data, Offset + Data_Field + 4)),
              Phase => Clock_Phase'Val (Octet (Data, Offset + Data_Field + 5)),
              Clock => Clock_Polarity'Val (Octet (Data, Offset + Data_Field + 6)),
              Maximum_Speed => Rate (Dword (Data, Offset + Data_Field)));
         when UART_Code =>
            if Specific > Interfaces.Unsigned_16 (Firmware_Tables.Byte'Last) then return (Status => Reserved_Bits_Set); end if;
            if (Specific and Two_Bits) > Flow_Control'Pos (Flow_Control'Last)
              or else (Specific / UART_Bits_Divisor) mod 8 > Data_Bits'Pos (Data_Bits'Last)
              or else Octet (Data, Offset + Data_Field + 8) > Parity'Pos (Parity'Last) then
               return (Status => Bad_UART_Format);
            end if;
            if (Octet (Data, Offset + Data_Field + 9) and UART_Lines_Reserved) /= 0 then
               return (Status => Reserved_Bits_Set);
            end if;
            Info := (Kind => UART_Bus, Flow => Flow_Control'Val (Specific and Two_Bits),
              Stops => Stop_Bits'Val ((Specific / 4) and Two_Bits),
              Bits => Data_Bits'Val ((Specific / UART_Bits_Divisor) mod 8),
              Order => (if (Specific and UART_Order_Bit) = 0 then Little_Endian else Big_Endian),
              Parity_Mode => Parity'Val (Octet (Data, Offset + Data_Field + 8)),
              Lines => Serial_Lines (Octet (Data, Offset + Data_Field + 9)),
              Baud_Rate => Rate (Dword (Data, Offset + Data_Field)),
              Receive_FIFO => FIFO_Bytes (Word (Data, Offset + Data_Field + 4)),
              Transmit_FIFO => FIFO_Bytes (Word (Data, Offset + Data_Field + 6)));
         when CSI2_Code =>
            if Specific > Interfaces.Unsigned_16 (Firmware_Tables.Byte'Last) then return (Status => Reserved_Bits_Set); end if;
            if (Specific and Two_Bits) > PHY_Kind'Pos (PHY_Kind'Last) then return (Status => Bad_CSI2_PHY); end if;
            Info := (Kind => CSI2_Bus, PHY => PHY_Kind'Val (Specific and Two_Bits), Port => Local_Port (Specific / 4));
         when others => Info := (Kind => Vendor_Bus);
      end case;
      return (Status => Decoded, Revision => Descriptor_Revision (Revision), Bus_Type => Bus_Code,
        Type_Revision => Type_Revision, Source_Index => Octet (Data, Offset + Index_Field),
        Specific_Flags => Specific, Role => (if (Flags and Flag_Consumer) = 0 then Produces_And_Consumes else Consumes_Resource),
        Initiated_By => (if (Flags and Flag_Initiator) = 0 then Controller_Initiated else Device_Initiated),
        Shared => (Flags and Flag_Sharing) /= 0, Information => Info,
        Vendor => (First => Vendor_Start, Length => Type_Length - Minimum),
        Source => (First => Source_Start, Length => Item.Next_Offset - Source_Start), Next_Offset => Item.Next_Offset);
   end Decode;
end ACPI_Resource_Data.Serial_Connections;

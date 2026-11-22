pragma Ada_2022;
package body ACPI_Resource_Data.GPIO_Connections with SPARK_Mode is
   function Word (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Interfaces.Unsigned_16 with
      Pre => Offset <= Byte_Count (Data'Length)
        and then Byte_Count (Data'Length) - Offset >= Word_Bytes,
      Post => Word'Result = Wire_Word (Data, Offset);
   function Word (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Interfaces.Unsigned_16 is
     (Interfaces.Unsigned_16 (Data (Data'First + Natural (Offset))) +
      Byte_Radix * Interfaces.Unsigned_16 (Data (Data'First + Natural (Offset) + 1)));
   function To_Absolute (Origin : Byte_Offset; Relative, Limit : Byte_Count)
      return Byte_Offset with
      Pre => Origin <= Limit and then Relative <= Limit - Origin,
      Post => To_Absolute'Result >= Origin and then To_Absolute'Result <= Limit
        and then To_Absolute'Result - Origin = Relative;
   function To_Absolute (Origin : Byte_Offset; Relative, Limit : Byte_Count)
      return Byte_Offset is (Origin + Relative);
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result is
      Item : constant Frame := Read_Frame (Data, Offset);
      General, Specific : Interfaces.Unsigned_16;
      Kind_Code, Config : Firmware_Tables.Byte;
      Pins, Source, Vendor, Vendor_Length, Extent : Byte_Count;
      Info : Connection_Information;
   begin
      if Item.Status /= ACPI_Resource_Data.Decoded then return (Status => Malformed_Frame); end if;
      if Item.Encoding /= Large_Item or else Item.Name /= GPIO_Name then
         return (Status => Unsupported_Descriptor);
      end if;
      if Item.Length < Minimum_Length then return (Status => Bad_Length); end if;
      if Data (Data'First + Natural (Offset + Revision_Field)) /= Revision_One then
         return (Status => Unsupported_Revision);
      end if;
      Kind_Code := Data (Data'First + Natural (Offset + Kind_Field));
      if Kind_Code > Connection_Kind'Pos (Connection_Kind'Last) then
         return (Status => Reserved_Connection_Type);
      end if;
      General := Word (Data, Offset + General_Field);
      Specific := Word (Data, Offset + Specific_Field);
      if (General and General_Reserved) /= 0 then return (Status => Reserved_Bits_Set); end if;
      if Kind_Code = Connection_Kind'Pos (Interrupt_Connection) then
         if (Specific and Interrupt_Reserved) /= 0 then return (Status => Reserved_Bits_Set); end if;
         declare Polarity : constant Interfaces.Unsigned_16 := (Specific / Polarity_Divisor) and Two_Bits; begin
            if Polarity = Two_Bits or else (Polarity = Interrupt_Polarity'Pos (Active_Both)
              and then (Specific and Mode_Bit) = 0) then
               return (Status => Bad_Interrupt_Polarity);
            end if;
            Info := (Kind => Interrupt_Connection,
              Mode => (if (Specific and Mode_Bit) = 0 then Level_Triggered else Edge_Triggered),
              Polarity => Interrupt_Polarity'Val (Natural (Polarity)), Wake_Capable => (Specific and Wake_Bit) /= 0);
         end;
      else
         if (Specific and IO_Reserved) /= 0 then return (Status => Reserved_Bits_Set); end if;
         Info := (Kind => IO_Connection, Restriction => IO_Restriction'Val (Natural (Specific and Two_Bits)));
      end if;
      Config := Data (Data'First + Natural (Offset + Configuration_Field));
      if Config > Last_Standard_Configuration and then Config < First_Vendor_Configuration then
         return (Status => Reserved_Configuration);
      end if;
      if Data (Data'First + Natural (Offset + Source_Index_Field)) /= 0 then
         return (Status => Bad_Source_Index);
      end if;
      Pins := Byte_Count (Word (Data, Offset + Pins_Field));
      Source := Byte_Count (Word (Data, Offset + Source_Field));
      Vendor := Byte_Count (Word (Data, Offset + Vendor_Field));
      Vendor_Length := Byte_Count (Word (Data, Offset + Vendor_Length_Field));
      Extent := Item.Next_Offset - Offset;
      if Pins < Fixed_Bytes or else Source < Pins or else Vendor < Source or else Vendor > Extent
        or else Vendor_Length /= Extent - Vendor then
         return (Status => Bad_Offsets);
      end if;
      if Source - Pins < Word_Bytes or else (Source - Pins) mod Word_Bytes /= 0
        or else (Kind_Code = Connection_Kind'Pos (Interrupt_Connection) and then Source - Pins /= Word_Bytes) then
         return (Status => Bad_Pin_Count);
      end if;
      return (Status => Decoded, Information => Info,
        Role => (if General = 0 then Produces_And_Consumes else Consumes_Resource),
        Shared => (Specific and Shared_Bit) /= 0, Configuration => Pin_Configuration (Config),
        Drive_Strength => Drive_Hundredths_Milliampere (Word (Data, Offset + Drive_Field)),
        Debounce_Timeout => Debounce_Hundredths_Millisecond (Word (Data, Offset + Debounce_Field)),
        Count => Pin_Count ((Source - Pins) / Word_Bytes),
        Pins => (First => To_Absolute (Offset, Pins, Item.Next_Offset), Length => Source - Pins),
        Source => (First => To_Absolute (Offset, Source, Item.Next_Offset), Length => Vendor - Source),
        Vendor => (First => To_Absolute (Offset, Vendor, Item.Next_Offset), Length => Vendor_Length), Next_Offset => Item.Next_Offset);
   end Decode;
   function Read_Pin (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Index : Positive) return Pin_Result is
      Description : constant Decode_Result := Decode (Data, Offset);
   begin
      if Description.Status /= Decoded or else Index > Description.Count then return (Available => False); end if;
      return (Available => True, Value => Pin_Number
        (Word (Data, Description.Pins.First + Byte_Count ((Index - 1) * Pin_Bytes))));
   end Read_Pin;
end ACPI_Resource_Data.GPIO_Connections;

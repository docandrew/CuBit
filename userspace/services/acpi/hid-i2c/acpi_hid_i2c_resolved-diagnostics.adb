with Interfaces;
package body ACPI_HID_I2C_Resolved.Diagnostics with SPARK_Mode is
   package S renames Candidates.S;
   package G renames Candidates.G;
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_64;
   Stage : constant String := "acpi: stage hid-i2c-facts ";
   Hex_Radix : constant Interfaces.Unsigned_64 := 16;
   Decimal_Radix : constant Interfaces.Unsigned_64 := 10;
   function Hex (Value : Interfaces.Unsigned_64) return String
     with Pre => Value <= Interfaces.Unsigned_64 (Interfaces.Unsigned_16'Last)
   is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Result : String (1 .. 4);
      Left : Interfaces.Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digits (Natural (Left mod Hex_Radix) + 1);
         Left := Left / Hex_Radix;
      end loop;
      return Result;
   end Hex;
   function Decimal (Value : Interfaces.Unsigned_64) return String is
      Maximum_Digits : constant Positive := 20;
      Data : String (1 .. Maximum_Digits) := [others => '0'];
      First : Positive := Data'Last;
      Left : Interfaces.Unsigned_64 := Value;
   begin
      loop
         Data (First) := Character'Val (Character'Pos ('0') + Natural (Left mod Decimal_Radix));
         Left := Left / Decimal_Radix;
         exit when Left = 0;
         First := First - 1;
      end loop;
      return Data (First .. Data'Last);
   end Decimal;
   function Safe_Source (Value : Candidates.Owned_Span) return Boolean is
   begin
      if Value.Length < 2 or else Value.Data (Value.Length) /= 0 then return False; end if;
      for I in 1 .. Value.Length - 1 loop
         if not (Value.Data (I) in Character'Pos ('A') .. Character'Pos ('Z')
           or else Value.Data (I) in Character'Pos ('a') .. Character'Pos ('z')
           or else Value.Data (I) in Character'Pos ('0') .. Character'Pos ('9')
           or else Value.Data (I) = Character'Pos ('_') or else Value.Data (I) = Character'Pos ('\')
           or else Value.Data (I) = Character'Pos ('^') or else Value.Data (I) = Character'Pos ('.'))
         then return False; end if;
      end loop;
      return True;
   end Safe_Source;
   function Format (Service : Core.State; Item : Report) return Line is
      Result : Line;
      Overflow : Boolean := False;
      procedure Append (Text : String) is
      begin
         if Overflow then return; end if;
         if Text'Length > Maximum_Line_Bytes - Result.Length then Overflow := True; return; end if;
         for C of Text loop
            Result.Length := Result.Length + 1; Result.Data (Result.Length) := C;
         end loop;
      end Append;
      procedure Source (Value : Candidates.Owned_Span) is
      begin
         for I in 1 .. Value.Length - 1 loop Append (String'[Character'Val (Value.Data (I))]); end loop;
      end Source;
      function Failure (Why : Outcome; Detail : String) return Line is
      begin
         Result := (Status => Why, others => <>); Overflow := False;
         Append (Stage); Append ("fail "); Append (Detail);
         if Overflow then return (Status => Log_Capacity_Refused, others => <>); end if;
         return Result;
      end Failure;
   begin
      if Item.Status /= Resolved then
         return Failure (Discovery_Failed, Item.Status'Image & " copy=" & Item.Copy.Status'Image &
           " i2c=" & Item.I2C.Status'Image & " gpio=" & Item.GPIO.Status'Image);
      end if;
      if Item.Value = Empty_Observation then return Failure (No_Observation, "NO_OBSERVATION"); end if;
      if not Current (Service, Item.Value) then return Failure (Stale_Binding, "STALE_BINDING"); end if;
      if not Safe_Source (Item.Value.Data.I2C_Source) or else not Safe_Source (Item.Value.Data.GPIO_Source) then
         return Failure (Invalid_Source_Text, "INVALID_SOURCE_TEXT");
      end if;
      Result.Status := Facts_Formatted;
      Append (Stage); Append ("ok dev="); Append (Core.Values.Node_Name (Item.Value.Device_Node));
      Append (" bus="); Source (Item.Value.Data.I2C_Source);
      Append (" addr=0x"); Append (Hex (Interfaces.Unsigned_64 (Item.Value.Data.I2C.Address)));
      Append (" mode="); Append ((case Item.Value.Data.I2C.Addressing is when S.Seven_Bit => "7", when S.Ten_Bit => "10"));
      Append (" speed="); Append (Decimal (Interfaces.Unsigned_64 (Item.Value.Data.I2C.Connection_Speed)));
      Append (" gpio="); Source (Item.Value.Data.GPIO_Source);
      Append (" pin=0x"); Append (Hex (Interfaces.Unsigned_64 (Item.Value.Data.Pin)));
      Append (" polarity="); Append ((case Item.Value.Data.Interrupt.Polarity is
         when G.Active_High => "high", when G.Active_Low => "low", when G.Active_Both => "both"));
      Append (" trigger="); Append ((case Item.Value.Data.Interrupt.Mode is
         when G.Level_Triggered => "level", when G.Edge_Triggered => "edge"));
      Append (" dreg=0x"); Append (Hex (Interfaces.Unsigned_64 (Item.Value.Data.Descriptor_Register)));
      if Overflow then return Failure (Log_Capacity_Refused, "LOG_CAPACITY_REFUSED"); end if;
      return Result;
   end Format;
end ACPI_HID_I2C_Resolved.Diagnostics;

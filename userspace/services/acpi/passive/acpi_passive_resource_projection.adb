pragma Ada_2022;
with ACPI_Resource_Data.Basic_Descriptors;
with ACPI_Resource_Data.Memory_Ranges;
with ACPI_Resource_Data.Address_Spaces;
with ACPI_Resource_Data.Address_Spaces.Extended_Descriptors;
with ACPI_Resource_Data.Extended_Interrupts;
with ACPI_Resource_Data.GPIO_Connections;
with ACPI_Resource_Data.Serial_Connections;
package body ACPI_Passive_Resource_Projection with SPARK_Mode is
   package R renames Records;
   package D renames Data_IO;
   package Basic renames D.Basic_Descriptors;
   package Memory renames D.Memory_Ranges;
   package Windows renames D.Address_Spaces;
   package Extended renames Windows.Extended_Descriptors;
   package IRQ renames D.Extended_Interrupts;
   package GPIO renames D.GPIO_Connections;
   package Serial renames D.Serial_Connections;
   use type D.Frame_Status;
   use type D.Item_Encoding;
   use type D.Byte_Count;
   use type R.U16;
   use type R.U32;
   use type Wire.Word;
   use type Basic.Decode_Status;
   use type Basic.Interrupt_Mode;
   use type Basic.Interrupt_Polarity;
   use type Basic.IO_Decoding;
   use type Memory.Decode_Status;
   use type Memory.Memory_Encoding;
   use type Windows.Decode_Status;
   use type Windows.Decode_Mode;
   use type Extended.Decode_Status;
   use type Extended.Resource_Role;
   use type IRQ.Decode_Status;
   use type IRQ.Interrupt_Mode;
   use type IRQ.Interrupt_Polarity;
   use type IRQ.Resource_Role;
   use type GPIO.Decode_Status;
   use type GPIO.Connection_Kind;
   use type GPIO.Interrupt_Mode;
   use type GPIO.Resource_Role;
   use type Serial.Decode_Status;
   use type Serial.Bus_Kind;
   use type Serial.Resource_Role;
   use type Serial.Address_Mode;
   use type Serial.Initiator;
   GPIO_General_Offset : constant D.Byte_Offset := 5;
   GPIO_Specific_Offset : constant D.Byte_Offset := 7;
   Serial_General_Offset : constant D.Byte_Offset := 6;
   procedure Read
     (Data : Firmware_Tables.Bytes; Offset : D.Byte_Offset;
      Subentry, Pool_Offset : R.U32; Source : out Firmware_Tables.Bytes;
      Item : out R.Resource_Result; Result : out Report) is
      Frame : constant D.Frame := D.Read_Frame (Data, Offset);
      Value : R.Resource_Record;
      Source_Start, Source_Length : D.Byte_Count := 0;
      Total : R.U32 := 1;
      function Octet (Position : D.Byte_Offset) return R.U8
        with Pre => Position < D.Byte_Count (Data'Length);
      function Octet (Position : D.Byte_Offset) return R.U8 is
        (Data (Data'First + Natural (Position)));
      function Word_At (Position : D.Byte_Offset) return R.U16
        with Pre => Position <= D.Byte_Count (Data'Length)
          and then D.Byte_Count (Data'Length) - Position >= 2;
      function Word_At (Position : D.Byte_Offset) return R.U16 is
        (R.U16 (Octet (Position)) + 256 * R.U16 (Octet (Position + 1)));
      procedure Opaque with Pre => Frame.Status = D.Decoded is
         Unknown : R.Resource_Record := (Kind => R.Unsupported_Descriptor, others => <>);
      begin
         if Subentry /= 0 then Result.Status := Bad_Subentry; return; end if;
         Unknown.Encoding := (if Frame.Encoding = D.Small_Item then R.Small_Item else R.Large_Item);
         Unknown.Name := R.U8 (Frame.Name); Unknown.Source_Offset := R.U32 (Offset);
         if R.Valid (Unknown, Wire.Node_Limit'Last, 0) then Item := (True, Unknown); Result.Status := Record_Ready; end if;
      end Opaque;
      procedure Source_At (First, Length : D.Byte_Count; Index : R.U8) is
      begin
         Source_Start := First; Source_Length := Length;
         Value.ResourceSource_Index := Index; Value.ResourceSource_Offset := R.U32 (First);
      end Source_At;
   begin
      Item := (Accepted => False); Result := (others => <>); Source := [others => 0];
      if Frame.Status /= D.Decoded then return; end if;
      Result.Next_Offset := Frame.Next_Offset;
      if Frame.Encoding = D.Small_Item and then Frame.Name = 15 then
         if Subentry /= 0 then Result.Status := Bad_Subentry;
         elsif Frame.Length = 1 then Result.Status := End_Tag; end if;
         return;
      end if;
      if (Frame.Encoding = D.Small_Item and then Frame.Name in 4 | 8 | 9)
        or else (Frame.Encoding = D.Large_Item and then Frame.Name = 6) then
         declare Decoded : constant Basic.Decode_Result := Basic.Decode (Data, Offset); begin
            if Decoded.Status /= Basic.Decoded then return; end if;
            case Decoded.Value.Kind is
               when Basic.Fixed_Memory_32_Resource =>
                  Value := (Kind => R.Fixed_Memory, others => <>);
                  Value.Fixed := (Information => Decoded.Value.Memory.Information,
                    Writeable => Decoded.Value.Memory.Declared_Writeable,
                    Base => R.U32 (Decoded.Value.Memory.Base), Length => R.U32 (Decoded.Value.Memory.Length));
               when Basic.IO_Resource =>
                  Value := (Kind => R.IO_Range, others => <>);
                  Value.IO := (False, (if Decoded.Value.IO.Decoding = Basic.Decode_16 then R.Decode_16 else R.Decode_10),
                    R.U16 (Decoded.Value.IO.Minimum), R.U16 (Decoded.Value.IO.Maximum),
                    Decoded.Value.IO.Alignment, Decoded.Value.IO.Length);
                  Value.Raw_General_Flags := R.U16 (Octet (Frame.Payload));
               when Basic.Fixed_IO_Resource =>
                  Value := (Kind => R.IO_Range, others => <>);
                  Value.IO := (True, R.Decode_10, R.U16 (Decoded.Value.Fixed_IO.Base),
                    R.U16 (Decoded.Value.Fixed_IO.Base), 0, Decoded.Value.Fixed_IO.Length);
               when Basic.IRQ_Resource =>
                  declare Mask : constant R.U16 := R.U16 (Decoded.Value.IRQ.Mask); Seen : R.U32 := 0; Number : R.U32 := 0; begin
                     Total := 0;
                     for Bit in 0 .. 15 loop if (Mask and 2 ** Bit) /= 0 then Total := Total + 1; end if; end loop;
                     if Total = 0 and then Subentry = 0 then Result.Status := Empty_Descriptor; return; end if;
                     if Subentry >= Total then Result.Status := Bad_Subentry; return; end if;
                     for Bit in 0 .. 15 loop
                        if (Mask and 2 ** Bit) /= 0 then if Seen = Subentry then Number := R.U32 (Bit); exit; end if; Seen := Seen + 1; end if;
                     end loop;
                     Value := (Kind => R.Interrupt_Entry, others => <>);
                     Value.IRQ := ((if Decoded.Value.IRQ.Mode = Basic.Level_Triggered then R.Level else R.Edge),
                       (if Decoded.Value.IRQ.Polarity = Basic.Active_Low then R.Active_Low else R.Active_High),
                       Decoded.Value.IRQ.Wake_Capable, Number);
                     Value.Shared := Decoded.Value.IRQ.Shared;
                     if Frame.Length = 3 then Value.Raw_General_Flags := R.U16 (Octet (Frame.Payload + 2)); end if;
                  end;
            end case;
         end;
      elsif Frame.Encoding = D.Large_Item and then Frame.Name in 1 | 5 then
         declare Decoded : constant Memory.Decode_Result := Memory.Decode (Data, Offset); begin
            if Decoded.Status /= Memory.Decoded then return; end if;
            Value := (Kind => R.Memory_Range, others => <>);
            Value.Memory := ((if Decoded.Encoding = Memory.Memory_24 then R.Memory_24 else R.Memory_32),
              Decoded.Information, Decoded.Declared_Writeable, R.U32 (Decoded.Minimum_Base), R.U32 (Decoded.Maximum_Base),
              R.U32 (Decoded.Encoded_Alignment), R.U32 (Decoded.Alignment), R.U32 (Decoded.Length));
         end;
      elsif Frame.Encoding = D.Large_Item and then Frame.Name in 7 | 8 | 10 then
         declare Decoded : constant Windows.Decode_Result := Windows.Decode (Data, Offset); begin
            if Decoded.Status /= Windows.Decoded then return; end if;
            Value := (Kind => R.Address_Window, others => <>);
            Value.Window := (Encoding => R.Address_Encoding'Val (Windows.Address_Width'Pos (Decoded.Width)),
              Resource_Type => Octet (Frame.Payload), Decode => (if Decoded.Mode = Windows.Positive_Decode then R.Positive_Decode else R.Subtractive_Decode),
              Minimum_Fixed => Decoded.Minimum_Fixed, Maximum_Fixed => Decoded.Maximum_Fixed, Type_Flags => Decoded.Information.Raw,
              Granularity => Wire.Word (Decoded.Granularity), Minimum => Wire.Word (Decoded.Minimum), Maximum => Wire.Word (Decoded.Maximum),
              Translation_Offset => Wire.Word (Decoded.Translation), Length => Wire.Word (Decoded.Length), others => <>);
            Value.Raw_General_Flags := R.U16 (Decoded.General_Flags); Value.Raw_Specific_Flags := R.U16 (Decoded.Information.Raw);
            Value.Resource_Role := (if (Decoded.General_Flags and 1) = 0 then R.Producer else R.Consumer);
            if Decoded.Source.Present then Source_At (Decoded.Source.Name_Offset, Decoded.Source.Name_Length + 1, Decoded.Source.Index); end if;
         end;
      elsif Frame.Encoding = D.Large_Item and then Frame.Name = 11 then
         declare Decoded : constant Extended.Decode_Result := Extended.Decode (Data, Offset); begin
            if Decoded.Status /= Extended.Decoded then
               if Decoded.Status = Extended.Unsupported_Revision then Opaque; end if;
               return;
            end if;
            Value := (Kind => R.Address_Window, others => <>);
            Value.Window := (Encoding => R.Extended_Address, Resource_Type => Octet (Frame.Payload),
              Decode => (if Decoded.Mode = Windows.Positive_Decode then R.Positive_Decode else R.Subtractive_Decode),
              Minimum_Fixed => Decoded.Minimum_Fixed, Maximum_Fixed => Decoded.Maximum_Fixed, Type_Flags => Decoded.Information.Raw,
              Granularity => Wire.Word (Decoded.Granularity), Minimum => Wire.Word (Decoded.Minimum), Maximum => Wire.Word (Decoded.Maximum),
              Translation_Offset => Wire.Word (Decoded.Translation), Length => Wire.Word (Decoded.Length),
              Revision => 1, Attributes => Wire.Word (Decoded.Attributes));
            Value.Resource_Role := (if Decoded.Role = Extended.Consumes_Resource then R.Consumer else R.Producer);
            Value.Raw_General_Flags := R.U16 (Decoded.General_Flags); Value.Raw_Specific_Flags := R.U16 (Decoded.Information.Raw);
         end;
      elsif Frame.Encoding = D.Large_Item and then Frame.Name = 9 then
         declare Decoded : constant IRQ.Decode_Result := IRQ.Decode (Data, Offset); begin
            if Decoded.Status /= IRQ.Decoded then return; end if;
            Total := R.U32 (Decoded.Count);
            if Subentry >= Total then Result.Status := Bad_Subentry; return; end if;
            Value := (Kind => R.Interrupt_Entry, others => <>);
            Value.IRQ := ((if Decoded.Mode = IRQ.Level_Triggered then R.Level else R.Edge),
              (if Decoded.Polarity = IRQ.Active_Low then R.Active_Low else R.Active_High), Decoded.Wake_Capable,
              R.U32 (Decoded.Interrupts (1 + Natural (Subentry))));
            Value.Resource_Role := (if Decoded.Role = IRQ.Consumer then R.Consumer else R.Producer);
            Value.Shared := Decoded.Shared; Value.Raw_General_Flags := R.U16 (Octet (Frame.Payload));
            if Decoded.Source.Present then Source_At (Decoded.Source.Name_Offset, Decoded.Source.Name_Length + 1, Decoded.Source.Index); end if;
         end;
      elsif Frame.Encoding = D.Large_Item and then Frame.Name = 12 then
         declare Decoded : constant GPIO.Decode_Result := GPIO.Decode (Data, Offset); Pin : GPIO.Pin_Result; begin
            if Decoded.Status /= GPIO.Decoded then
               if Decoded.Status = GPIO.Unsupported_Revision then Opaque; end if;
               return;
            end if;
            Total := R.U32 (Decoded.Count);
            if Subentry >= Total then Result.Status := Bad_Subentry; return; end if;
            Pin := GPIO.Read_Pin (Data, Offset, 1 + Natural (Subentry)); if not Pin.Available then return; end if;
            Value := (Kind => R.GPIO_Pin, others => <>);
            Value.GPIO.Pin := R.U16 (Pin.Value); Value.GPIO.Configuration := R.U8 (Decoded.Configuration);
            Value.GPIO.Drive_Hundredths_Milliampere := R.U16 (Decoded.Drive_Strength);
            Value.GPIO.Debounce_Ten_Microseconds := R.U16 (Decoded.Debounce_Timeout);
            if Decoded.Information.Kind = GPIO.Interrupt_Connection then
               Value.GPIO.Kind := R.Interrupt_Connection;
               Value.GPIO.Mode := (if Decoded.Information.Mode = GPIO.Level_Triggered then R.Level else R.Edge);
               Value.GPIO.Active := R.Polarity'Val (GPIO.Interrupt_Polarity'Pos (Decoded.Information.Polarity));
               Value.GPIO.Wake := Decoded.Information.Wake_Capable;
            else Value.GPIO.Kind := R.IO_Connection; Value.GPIO.Restriction := R.IO_Restriction'Val
                (GPIO.IO_Restriction'Pos (Decoded.Information.Restriction)); end if;
            Value.Shared := Decoded.Shared; Value.Resource_Role := (if Decoded.Role = GPIO.Consumes_Resource then R.Consumer else R.Producer);
            Value.Raw_General_Flags := Word_At (Offset + GPIO_General_Offset);
            Value.Raw_Specific_Flags := Word_At (Offset + GPIO_Specific_Offset);
            Source_At (Decoded.Source.First, Decoded.Source.Length, 0);
         end;
      elsif Frame.Encoding = D.Large_Item and then Frame.Name = 14 then
         declare Decoded : constant Serial.Decode_Result := Serial.Decode (Data, Offset); begin
            if Decoded.Status /= Serial.Decoded then
               if Decoded.Status in Serial.Unsupported_Revision | Serial.Unsupported_Bus_Type | Serial.Unsupported_Type_Revision then Opaque; end if;
               return;
            end if;
            if Decoded.Information.Kind = Serial.I2C_Bus then
               Value := (Kind => R.I2C_Connection, others => <>);
               Value.I2C := (Revision => R.U8 (Decoded.Revision), Type_Revision => Decoded.Type_Revision,
                 Initiator => (if Decoded.Initiated_By = Serial.Controller_Initiated then R.Controller_Initiated else R.Device_Initiated),
                 Addressing => (if Decoded.Information.Addressing = Serial.Seven_Bit then R.Seven_Bit else R.Ten_Bit),
                 Slave_Address => R.U16 (Decoded.Information.Address), Speed_Hertz => R.U32 (Decoded.Information.Connection_Speed),
                 Legacy_Virtual_Register => Decoded.Information.Legacy_Virtual_Register);
               Value.Shared := Decoded.Shared; Value.Resource_Role := (if Decoded.Role = Serial.Consumes_Resource then R.Consumer else R.Producer);
               Value.Raw_General_Flags := R.U16 (Octet (Offset + Serial_General_Offset)); Value.Raw_Specific_Flags := Decoded.Specific_Flags;
               Source_At (Decoded.Source.First, Decoded.Source.Length, Decoded.Source_Index);
            end if;
         end;
      end if;
      if Subentry >= Total then Result.Status := Bad_Subentry; return; end if;
      Value.Encoding := (if Frame.Encoding = D.Small_Item then R.Small_Item else R.Large_Item);
      Value.Name := R.U8 (Frame.Name); Value.Source_Offset := R.U32 (Offset);
      Value.Subentry := Subentry; Value.Total_Subentries := Total;
      if Source_Length > 0 then
         if Source_Start > D.Byte_Count (Data'Length) or else Source_Length > D.Byte_Count (Data'Length) - Source_Start then return; end if;
         Result.Required_Source_Bytes := Wire.Word (Source_Length);
         if Wire.Word (Source_Length) > Wire.Word (Source'Length) then Result.Status := Output_Required; return; end if;
         if Wire.Word (Pool_Offset) + Wire.Word (Source_Length) > Wire.Word (R.U32'Last) then
            Result.Status := Invalid_Pool_Offset; return;
         end if;
         Value.Controller := (Kind => R.Unresolved_Path, Path => (Pool_Offset, R.U32 (Source_Length)), others => <>);
      end if;
      if not R.Valid (Value, Wire.Node_Limit'Last, R.U32 (Wire.Word (Pool_Offset) + Wire.Word (Source_Length))) then return; end if;
      for I in 0 .. Natural (Source_Length) - 1 loop
         Source (Source'First + I) := Data (Data'First + (Natural (Source_Start) + I));
      end loop;
      Item := (True, Value); Result.Status := Record_Ready; Result.Source_Bytes := Wire.Word (Source_Length);
      if Subentry + 1 < Total then Result.Next_Offset := Offset; Result.Next_Subentry := Subentry + 1; end if;
   end Read;
end ACPI_Passive_Resource_Projection;

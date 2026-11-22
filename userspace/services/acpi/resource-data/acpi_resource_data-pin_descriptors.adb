pragma Ada_2022;
package body ACPI_Resource_Data.Pin_Descriptors with SPARK_Mode is
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   Revision_Field : constant Byte_Offset := 3;
   Flags_Field : constant Byte_Offset := 4;
   Revision_One : constant Firmware_Tables.Byte := 1;
   Last_Standard_Pull : constant Firmware_Tables.Byte := 3;
   Last_Standard_Configuration : constant Firmware_Tables.Byte := 13;
   First_Vendor_Configuration : constant Firmware_Tables.Byte := 128;
   Shared_Flag : constant Interfaces.Unsigned_16 := 1;
   Consumer_Flag : constant Interfaces.Unsigned_16 := 2;
   One_Flag_Reserved : constant Interfaces.Unsigned_16 := 16#FFFE#;
   Two_Flags_Reserved : constant Interfaces.Unsigned_16 := 16#FFFC#;
   Byte_Radix : constant Interfaces.Unsigned_32 := 256;
   Word_Byte_Radix : constant Interfaces.Unsigned_16 := 256;
   Word_Bytes : constant Byte_Count := Byte_Count (Pin_Bytes);
   Minimum_Label_Bytes : constant Byte_Count := 2;
   subtype Field_Offset is Byte_Offset range 0 .. 20;
   type Layout is record
      Fixed_Bytes : Byte_Count;
      Pins_Field, Source_Field, Label_Field, Vendor_Field, Length_Field,
        Index_Field, Function_Field, Pull_Field, Configuration_Field, Value_Field : Field_Offset := 0;
   end record;
   Layouts : constant array (Descriptor_Kind) of Layout :=
     [Pin_Function => (Fixed_Bytes => 18, Pins_Field => 9, Source_Field => 12,
        Vendor_Field => 14, Length_Field => 16, Index_Field => 11, Function_Field => 7,
        Pull_Field => 6, others => 0),
      Pin_Configuration => (Fixed_Bytes => 20, Pins_Field => 11, Source_Field => 14,
        Vendor_Field => 16, Length_Field => 18, Index_Field => 13,
        Configuration_Field => 6, Value_Field => 7, others => 0),
      Pin_Group => (Fixed_Bytes => 14, Pins_Field => 6, Label_Field => 8,
        Vendor_Field => 10, Length_Field => 12, others => 0),
      Group_Function => (Fixed_Bytes => 17, Source_Field => 9, Label_Field => 11,
        Vendor_Field => 13, Length_Field => 15, Index_Field => 8,
        Function_Field => 6, others => 0),
      Group_Configuration => (Fixed_Bytes => 20, Source_Field => 12, Label_Field => 14,
        Vendor_Field => 16, Length_Field => 18, Index_Field => 11,
        Configuration_Field => 6, Value_Field => 7, others => 0)];
   function Octet (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Firmware_Tables.Byte with Pre => Position < Byte_Count (Data'Length);
   function Octet (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Firmware_Tables.Byte is (Data (Data'First + Natural (Position)));
   function Word (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_16 with
      Pre => Position <= Byte_Count (Data'Length) and then Byte_Count (Data'Length) - Position >= Word_Bytes;
   function Word (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_16 is
     (Interfaces.Unsigned_16 (Octet (Data, Position))
      + Word_Byte_Radix * Interfaces.Unsigned_16 (Octet (Data, Position + 1)));
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
      Kind : Descriptor_Kind;
      Flags : Interfaces.Unsigned_16;
      Source, Label_Offset : Byte_Count := 0;
      Pins, Vendor, Vendor_Length, Extent, Name_First : Byte_Count;
      Pin_Span, Source_Span, Label_Span : Byte_Span;
      Count : Pin_Count := 0;
      Info : Pin_Information;
      Role : Resource_Role;
   begin
      if Item.Status /= ACPI_Resource_Data.Decoded then return (Status => Malformed_Frame); end if;
      if Item.Encoding /= Large_Item then return (Status => Unsupported_Descriptor); end if;
      case Item.Name is
         when 13 => Kind := Pin_Function;
         when 15 => Kind := Pin_Configuration;
         when 16 => Kind := Pin_Group;
         when 17 => Kind := Group_Function;
         when 18 => Kind := Group_Configuration;
         when others => return (Status => Unsupported_Descriptor);
      end case;
      declare L : constant Layout := Layouts (Kind); begin
         if Byte_Count (Item.Length) < L.Fixed_Bytes - Byte_Count (Large_Header_Bytes) then return (Status => Bad_Length); end if;
         if Octet (Data, Offset + Revision_Field) /= Revision_One then return (Status => Unsupported_Revision); end if;
         Flags := Word (Data, Offset + Flags_Field);
         if (Flags and (if Kind in Pin_Function | Pin_Group then One_Flag_Reserved else Two_Flags_Reserved)) /= 0 then
            return (Status => Reserved_Bits_Set);
         end if;
         if Kind /= Pin_Group and then Octet (Data, Offset + L.Index_Field) /= 0 then return (Status => Bad_Source_Index); end if;
         Vendor := Byte_Count (Word (Data, Offset + L.Vendor_Field));
         Vendor_Length := Byte_Count (Word (Data, Offset + L.Length_Field));
         Extent := Item.Next_Offset - Offset;
         if Vendor < L.Fixed_Bytes or else Vendor > Extent or else Vendor_Length /= Extent - Vendor then
            return (Status => Bad_Offsets);
         end if;
         if Kind /= Pin_Group then Source := Byte_Count (Word (Data, Offset + L.Source_Field)); end if;
         if Kind in Pin_Group | Group_Function | Group_Configuration then
            Label_Offset := Byte_Count (Word (Data, Offset + L.Label_Field));
            if Label_Offset < L.Fixed_Bytes or else Label_Offset > Vendor then return (Status => Bad_Offsets); end if;
            Label_Span := (First => Offset + Label_Offset, Length => Vendor - Label_Offset);
            if Label_Span.Length < Minimum_Label_Bytes then return (Status => Bad_Label); end if;
            for Position in Byte_Offset range Label_Span.First .. Offset + Vendor - 1 loop
               if (Octet (Data, Position) = 0) /= (Position = Offset + Vendor - 1) then return (Status => Bad_Label); end if;
            end loop;
         end if;
         if Kind /= Pin_Group then
            Name_First := (if Kind in Group_Function | Group_Configuration then Label_Offset else Vendor);
            if Source < L.Fixed_Bytes or else Source > Name_First then return (Status => Bad_Offsets); end if;
            Source_Span := (First => Offset + Source, Length => Name_First - Source);
         end if;
         if Kind in Pin_Function | Pin_Configuration | Pin_Group then
            Pins := Byte_Count (Word (Data, Offset + L.Pins_Field));
            Name_First := (if Kind = Pin_Group then Label_Offset else Source);
            if Pins < L.Fixed_Bytes or else Pins > Name_First then return (Status => Bad_Offsets); end if;
            if Name_First - Pins < Word_Bytes or else (Name_First - Pins) mod Word_Bytes /= 0 then return (Status => Bad_Pin_Count); end if;
            Pin_Span := (First => Offset + Pins, Length => Name_First - Pins);
            Count := Pin_Count ((Name_First - Pins) / Word_Bytes);
         end if;
         case Kind is
            when Pin_Function =>
               declare Pull : constant Firmware_Tables.Byte := Octet (Data, Offset + L.Pull_Field); begin
                  if Pull > Last_Standard_Pull and then Pull < First_Vendor_Configuration then return (Status => Reserved_Pull); end if;
                  Info := (Kind => Pin_Function, Pull => Pull_Configuration (Pull),
                    Function_ID => Function_Number (Word (Data, Offset + L.Function_Field)));
               end;
            when Group_Function => Info := (Kind => Group_Function,
               Group_Function_ID => Function_Number (Word (Data, Offset + L.Function_Field)));
            when Pin_Configuration | Group_Configuration =>
               declare Config : constant Firmware_Tables.Byte := Octet (Data, Offset + L.Configuration_Field); begin
                  if Config > Last_Standard_Configuration and then Config < First_Vendor_Configuration then
                     return (Status => Reserved_Configuration_Type);
                  end if;
                  if Kind = Pin_Configuration then
                     Info := (Kind => Pin_Configuration, Configuration => Configuration_Type (Config),
                       Value => Configuration_Value (Dword (Data, Offset + L.Value_Field)));
                  else
                     Info := (Kind => Group_Configuration, Configuration => Configuration_Type (Config),
                       Value => Configuration_Value (Dword (Data, Offset + L.Value_Field)));
                  end if;
               end;
            when Pin_Group => Info := (Kind => Pin_Group);
         end case;
         Role := (if Kind = Pin_Function then Consumes_Resource
           elsif (Flags and (if Kind = Pin_Group then Shared_Flag else Consumer_Flag)) = 0
             then Produces_And_Consumes else Consumes_Resource);
         return (Status => Decoded, Information => Info,
           Shared => Kind /= Pin_Group and then (Flags and Shared_Flag) /= 0,
           Role => Role, Count => Count, Pins => Pin_Span, Source => Source_Span, Label => Label_Span,
           Vendor => (First => Offset + Vendor, Length => Vendor_Length), Next_Offset => Item.Next_Offset);
      end;
   end Decode;
   function Matches_Wire_Pin (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Index : Positive; Item : Pin_Result) return Boolean is
     (declare Description : constant Decode_Result := Decode (Data, Offset); begin
       (if Description.Status /= Decoded or else Index > Description.Count then not Item.Available
        else Item.Available and then
          (declare
              Position : constant Byte_Offset := Description.Pins.First + Byte_Count ((Index - 1) * Pin_Bytes);
              Pin_Byte_Radix : constant Pin_Number := 256;
           begin Item.Value = Pin_Number (Data (Data'First + Natural (Position)))
             + Pin_Byte_Radix * Pin_Number (Data (Data'First + Natural (Position) + 1)))));
   function Read_Pin (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Index : Positive) return Pin_Result is
      Item : constant Decode_Result := Decode (Data, Offset);
   begin
      if Item.Status /= Decoded or else Index > Item.Count then return (Available => False); end if;
      return (Available => True, Value => Pin_Number (Word (Data, Item.Pins.First + Byte_Count ((Index - 1) * Pin_Bytes))));
   end Read_Pin;
end ACPI_Resource_Data.Pin_Descriptors;

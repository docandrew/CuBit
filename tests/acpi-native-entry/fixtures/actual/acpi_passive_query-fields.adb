pragma Ada_2022;
package body ACPI_Passive_Query.Fields with SPARK_Mode is
   Empty_Text : constant Byte_Span := (0, 0);
   function Clear_Value (Item : Field_Record) return Boolean is
     (Item.Number = 0 and then Item.Text = Empty_Text and then Item.ID = No_ID
      and then Item.Logical_Offset = 0 and then Item.Total_Length = 0);
   function Text_Fits (Item : Field_Record; Payload_Bytes : U32) return Boolean is
     (Item.Text.Offset <= Payload_Bytes
      and then Item.Text.Length <= Payload_Bytes - Item.Text.Offset
      and then Item.Logical_Offset <= Item.Total_Length
      and then Word (Item.Text.Length) <= Item.Total_Length - Item.Logical_Offset
      and then (Item.Text.Length > 0 or else
        (Item.Total_Length = 0 and then Item.Logical_Offset = 0 and then Item.Text.Offset = 0)));
   function Valid (Item : Field_Record; Limit : Node_Limit; Payload_Bytes : U32)
     return Boolean is
   begin
      if Item.Name /= Compatible_ID and then Item.Ordinal /= 0 then return False; end if;
      if Item.Status /= Present then
         if Item.Name = Device_Status and then Item.Status = Absent then
            return Item.Kind = Integer_Value and then Item.Defaulted and then Item.Number = Default_STA
              and then Item.Text = Empty_Text and then Item.ID = No_ID
              and then Item.Logical_Offset = 0 and then Item.Total_Length = 0;
         end if;
         return Item.Kind = No_Value and then not Item.Defaulted and then Clear_Value (Item)
           and then Item.Ordinal = 0;
      end if;
      if Item.Defaulted then return False; end if;
      case Item.Name is
         when Hardware_ID =>
            return Item.Kind = Identifier and then Valid_ID (Item.ID) and then Item.Number = 0
              and then Item.Text = Empty_Text and then Item.Logical_Offset = 0 and then Item.Total_Length = 0;
         when Compatible_ID =>
            if Item.Kind = Identifier then
               return Valid_ID (Item.ID) and then Item.Number = 0 and then Item.Text = Empty_Text
                 and then Item.Logical_Offset = 0 and then Item.Total_Length = 0;
            end if;
            -- Bus-specific CID strings are also valid. Keep the complete raw
            -- string; the page builder requests more room rather than slicing
            -- one CID across cursors that only identify the list ordinal.
            return Item.Kind = Text_Chunk and then Item.Number = 0 and then Item.ID = No_ID
              and then Text_Fits (Item, Payload_Bytes) and then Item.Logical_Offset = 0
              and then Item.Total_Length > 0 and then Item.Total_Length = Word (Item.Text.Length);
         when Address | Device_Status =>
            return Item.Kind = Integer_Value and then Item.Text = Empty_Text and then Item.ID = No_ID
              and then Item.Logical_Offset = 0 and then Item.Total_Length = 0;
         when Unique_ID =>
            if Item.Kind = Integer_Value then
               return Item.Text = Empty_Text and then Item.ID = No_ID
                 and then Item.Logical_Offset = 0 and then Item.Total_Length = 0;
            end if;
            return Item.Kind = Text_Chunk and then Item.Number = 0 and then Item.ID = No_ID
              and then Text_Fits (Item, Payload_Bytes);
         when Device_Path =>
            -- One root character plus at most one separator and four name
            -- characters per namespace node. No independent depth constant.
            return Item.Kind = Text_Chunk and then Item.Number = 0 and then Item.ID = No_ID
              and then Item.Total_Length > 0 and then Item.Total_Length <= 1 + 5 * Word (Limit)
              and then Text_Fits (Item, Payload_Bytes);
      end case;
   end Valid;
   function Has_More (Item : Field_Record; Limit : Node_Limit; Payload_Bytes : U32)
     return Boolean is
      pragma Unreferenced (Limit, Payload_Bytes);
   begin
      return Item.Kind = Text_Chunk and then Word (Item.Text.Length) < Item.Total_Length - Item.Logical_Offset;
   end Has_More;
   function From_EISA (Encoded : Word) return ID_Result is
      Byte_Mask : constant Word := 16#FF#;
      Letter_Mask : constant Word := 16#1F#;
      Vendor_Bits : constant := 5;
      Vendor_Base_Shift : constant := 16;
      Product_Bits : constant := 4;
      Product_Mask : constant Word := 16#F#;
      Hex : constant String := "0123456789ABCDEF";
      Swapped, Code : Word;
      Text : String (1 .. 7);
   begin
      if Encoded > Word (U32'Last) then return (Accepted => False); end if;
      Swapped := Interfaces.Shift_Left (Encoded and Byte_Mask, 24)
        or Interfaces.Shift_Left (Interfaces.Shift_Right (Encoded, 8) and Byte_Mask, 16)
        or Interfaces.Shift_Left (Interfaces.Shift_Right (Encoded, 16) and Byte_Mask, 8)
        or (Interfaces.Shift_Right (Encoded, 24) and Byte_Mask);
      if (Swapped and 16#8000_0000#) /= 0 then return (Accepted => False); end if;
      for I in 1 .. 3 loop
         Code := Interfaces.Shift_Right (Swapped, Vendor_Base_Shift + Vendor_Bits * (3 - I)) and Letter_Mask;
         if Code not in 1 .. 26 then return (Accepted => False); end if;
         Text (I) := Character'Val (Character'Pos ('A') + Natural (Code) - 1);
      end loop;
      for I in 0 .. 3 loop
         Code := Interfaces.Shift_Right (Swapped, Product_Bits * (3 - I)) and Product_Mask;
         Text (4 + I) := Hex (1 + Natural (Code));
      end loop;
      return From_Text (Text);
   end From_EISA;
   function Encode (Item : Field_Record; Limit : Node_Limit; Payload_Bytes : U32)
     return Record_Words is
      pragma Unreferenced (Limit, Payload_Bytes);
   begin
      return [Record_Magic, Record_Version, Field_Name'Pos (Item.Name), Field_Status'Pos (Item.Status),
        Value_Kind'Pos (Item.Kind), Item.Number, Word (Item.Text.Offset), Word (Item.Text.Length),
        Item.Logical_Offset, Item.Total_Length, Word (Item.Ordinal), (if Item.Defaulted then 1 else 0),
        Word (Item.ID.Length), Item.ID.Data, 0, 0];
   end Encode;
   function Decode (Data : Record_Words; Limit : Node_Limit; Payload_Bytes : U32)
     return Field_Result is
      Item : Field_Record;
   begin
      if Data (0) /= Record_Magic or else Data (1) /= Record_Version
        or else Data (2) > Field_Name'Pos (Field_Name'Last)
        or else Data (3) > Field_Status'Pos (Field_Status'Last)
        or else Data (4) > Value_Kind'Pos (Value_Kind'Last)
        or else Data (6) > Word (U32'Last) or else Data (7) > Word (U32'Last)
        or else Data (10) > Word (U32'Last) or else Data (11) > 1 or else Data (12) > Maximum_ID_Length
      then return (Accepted => False); end if;
      Item := (Field_Name'Val (Natural (Data (2))), Field_Status'Val (Natural (Data (3))),
        Value_Kind'Val (Natural (Data (4))), Data (5), (U32 (Data (6)), U32 (Data (7))),
        Data (8), Data (9), U32 (Data (10)), Data (11) = 1, (ID_Length (Data (12)), Data (13), Inline_ID));
      if Valid (Item, Limit, Payload_Bytes) and then Encode (Item, Limit, Payload_Bytes) = Data then
         return (True, Item);
      end if;
      return (Accepted => False);
   end Decode;
   type Word_Result (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Word;
         when False => null;
      end case;
   end record;
   function Read_Word (Payload : Byte_IO.Byte_Array; Offset : Word) return Word_Result is
      Value : Word := 0;
   begin
      if Payload'Length < Byte_IO.Bytes_Per_Word
        or else Offset > Word (Natural'Last) - (Byte_IO.Bytes_Per_Word - 1)
      then return (Accepted => False); end if;
      if Natural (Offset) > Payload'Last - Payload'First - (Byte_IO.Bytes_Per_Word - 1)
      then return (Accepted => False); end if;
      for I in 0 .. Byte_IO.Bytes_Per_Word - 1 loop
         Value := Value or Interfaces.Shift_Left
           (Word (Payload (Payload'First + (Natural (Offset) + I))), Byte_IO.Byte'Size * I);
      end loop;
      return (True, Value);
   end Read_Word;
   function Field_At (Payload : Byte_IO.Byte_Array; Count, Position : U32; Limit : Node_Limit)
     return Field_Result is
      Data : Record_Words := [others => 0];
   begin
      if Position >= Count or else Word (Count) * Record_Byte_Count > Word (Payload'Length)
      then return (Accepted => False); end if;
      for I in Data'Range loop
         declare R : constant Word_Result := Read_Word
           (Payload, Word (Position) * Record_Byte_Count + Word (I * Byte_IO.Bytes_Per_Word)); begin
            if not R.Accepted then return (Accepted => False); end if;
            Data (I) := R.Value;
         end;
      end loop;
      declare R : constant Field_Result := Decode (Data, Limit, U32 (Payload'Length)); begin
         if not R.Accepted then return R; end if;
         if R.Value.Kind = Text_Chunk and then R.Value.Text.Length > 0
           and then Word (R.Value.Text.Offset) < Word (Count) * Record_Byte_Count
         then return (Accepted => False); end if;
         return R;
      end;
   end Field_At;
end ACPI_Passive_Query.Fields;

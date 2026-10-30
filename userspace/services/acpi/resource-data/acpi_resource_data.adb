pragma Ada_2022;
package body ACPI_Resource_Data with SPARK_Mode is
   use type Firmware_Tables.Byte;
   Large_Bit : constant Firmware_Tables.Byte := 16#80#;
   Large_Name_Mask : constant Firmware_Tables.Byte := 16#7F#;
   Small_Length_Mask : constant Firmware_Tables.Byte := 7;
   Small_Name_Divisor : constant Firmware_Tables.Byte := 8;
   Byte_Radix : constant Positive := 256;
   Low_Length_Offset : constant Byte_Offset := 1;
   High_Length_Offset : constant Byte_Offset := 2;
   End_Name : constant Item_Name := 15;
   End_Length : constant Payload_Length := 1;
   function Read_Frame (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
     return Frame is
      Extent : constant Byte_Count := Byte_Count (Data'Length);
      Remaining : Byte_Count;
      Tag : Firmware_Tables.Byte;
      Encoding : Item_Encoding;
      Name : Item_Name;
      Header : Header_Length;
      Size : Payload_Length;
   begin
      if Offset >= Extent then return (Status => No_Header); end if;
      Remaining := Extent - Offset;
      Tag := Data (Data'First + Natural (Offset));
      if (Tag and Large_Bit) /= 0 then
         Encoding := Large_Item; Header := Large_Header_Bytes;
         if Remaining < Byte_Count (Header) then return (Status => Truncated_Header); end if;
         Name := Natural (Tag and Large_Name_Mask);
         Size := Natural (Data (Data'First + Natural (Offset + Low_Length_Offset)))
           + Byte_Radix * Natural (Data (Data'First + Natural (Offset + High_Length_Offset)));
      else
         Encoding := Small_Item; Header := Small_Header_Bytes;
         Name := Natural (Tag / Small_Name_Divisor);
         Size := Natural (Tag and Small_Length_Mask);
      end if;
      if Byte_Count (Size) > Remaining - Byte_Count (Header) then
         return (Status => Truncated_Payload);
      end if;
      return (Decoded, Encoding, Name, Header, Size,
         Offset + Byte_Count (Header), Offset + Byte_Count (Header) + Byte_Count (Size));
   end Read_Frame;
   function Scan (Data : Firmware_Tables.Bytes;
      Maximum_Items : Item_Count := Default_Item_Limit) return Scan_Result is
      Cursor : Byte_Offset := 0;
      Items : Item_Count := 0;
      Extent : constant Byte_Count := Byte_Count (Data'Length);
      Sum : Firmware_Tables.Byte := 0;
   begin
      while Cursor < Extent loop
         pragma Loop_Invariant (Cursor <= Extent and then Items <= Maximum_Items);
         pragma Loop_Variant (Decreases => Extent - Cursor);
         declare Item : constant Frame := Read_Frame (Data, Cursor); begin
            if Item.Status /= Decoded then return (Malformed_Frame, Items, Cursor); end if;
            if Item.Encoding = Small_Item and then Item.Name = End_Name then
               if Item.Length /= End_Length then return (Bad_End_Tag, Items, Cursor); end if;
               if Item.Next_Offset /= Extent then return (Trailing_Data, Items, Cursor); end if;
               if Data (Data'First + Natural (Item.Payload)) /= 0 then
                  for B of Data loop Sum := Sum + B; end loop;
                  if Sum /= 0 then return (Bad_Checksum, Items, Cursor); end if;
               end if;
               return (Complete, Items, Extent);
            end if;
            if Items = Maximum_Items then return (Item_Limit, Items, Cursor); end if;
            Items := Items + 1;
            Cursor := Item.Next_Offset;
         end;
      end loop;
      return (Missing_End_Tag, Items, Cursor);
   end Scan;
end ACPI_Resource_Data;

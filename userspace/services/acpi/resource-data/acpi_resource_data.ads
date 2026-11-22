pragma Ada_2022;
with Firmware_Tables;
-- Copied firmware descriptions only. Framing never establishes hardware rights,
-- descriptor-specific validity, platform ownership or an admitted register.
package ACPI_Resource_Data with SPARK_Mode, Pure is
   type Byte_Count is range 0 .. Natural'Last;
   subtype Byte_Offset is Byte_Count;
   type Item_Count is range 0 .. Natural'Last;
   Default_Item_Limit : constant Item_Count := 1_024;
   type Item_Encoding is (Small_Item, Large_Item);
   subtype Item_Name is Natural range 0 .. 127;
   subtype Small_Item_Name is Item_Name range 0 .. 15;
   subtype Header_Length is Positive range 1 .. 3;
   Small_Header_Bytes : constant Header_Length := 1;
   Large_Header_Bytes : constant Header_Length := 3;
   subtype Payload_Length is Natural range 0 .. 65_535;
   type Frame_Status is (Decoded, No_Header, Truncated_Header, Truncated_Payload);
   type Frame (Status : Frame_Status := No_Header) is record
      case Status is
         when Decoded =>
            Encoding : Item_Encoding;
            Name : Item_Name;
            Header : Header_Length;
            Length : Payload_Length;
            Payload, Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- Zero-based offset into Data; no borrowed slice or address is returned.
   -- Unknown/reserved item names are framed, not treated as valid descriptors.
   function Read_Frame (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
     return Frame with Global => null,
     Post => (if Read_Frame'Result.Status = Decoded then
       Read_Frame'Result.Payload > Offset
       and then Read_Frame'Result.Payload <= Read_Frame'Result.Next_Offset
       and then Read_Frame'Result.Next_Offset <= Byte_Count (Data'Length)
       and then Read_Frame'Result.Next_Offset > Offset
       and then Read_Frame'Result.Payload = Offset + Byte_Count (Read_Frame'Result.Header)
       and then Read_Frame'Result.Next_Offset - Read_Frame'Result.Payload =
         Byte_Count (Read_Frame'Result.Length)
       and then (if Read_Frame'Result.Encoding = Small_Item then
         Read_Frame'Result.Name in Small_Item_Name
           and then Read_Frame'Result.Header = Small_Header_Bytes
         else Read_Frame'Result.Header = Large_Header_Bytes));
   type Template_Status is (Complete, Missing_End_Tag, Malformed_Frame,
      Bad_End_Tag, Trailing_Data, Bad_Checksum, Item_Limit);
   type Scan_Result is record
      Status : Template_Status := Missing_End_Tag;
      Items : Item_Count := 0; -- Non-EndTag frames, including unknown items.
      Position : Byte_Offset := 0; -- Failing frame, or full extent on Complete.
   end record;
   -- Full-buffer framing only. EndTag must be exactly 0x79 plus one checksum
   -- byte, with no trailing data. A zero checksum disables checksum checking as
   -- specified by ACPI; otherwise the entire template must sum to zero.
   function Scan (Data : Firmware_Tables.Bytes;
      Maximum_Items : Item_Count := Default_Item_Limit) return Scan_Result
     with Global => null,
       Post => Scan'Result.Position <= Byte_Count (Data'Length)
         and then Scan'Result.Items <= Maximum_Items
         and then (if Scan'Result.Status = Complete then
           Scan'Result.Position = Byte_Count (Data'Length));
end ACPI_Resource_Data;

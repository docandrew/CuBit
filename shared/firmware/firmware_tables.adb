pragma Ada_2022;
with Interfaces; use Interfaces;

package body Firmware_Tables with SPARK_Mode is
   function U32 (Data : Bytes; Offset : Natural) return Unsigned_32
   is
     (Unsigned_32 (Data (Data'First + Offset)) or
      Shift_Left (Unsigned_32 (Data (Data'First + Offset + 1)), 8) or
      Shift_Left (Unsigned_32 (Data (Data'First + Offset + 2)), 16) or
      Shift_Left (Unsigned_32 (Data (Data'First + Offset + 3)), 24))
     with Pre => Data'Length >= 4 and then Offset <= Data'Length - 4;

   function Sum (Data : Bytes) return Byte is
      Result : Byte := 0;
   begin
      for B of Data loop
         Result := Result + B;
      end loop;
      return Result;
   end Sum;

   function Read_Root (Data : Bytes) return Root_Result is
      Magic : constant String := "RSD PTR ";
      Extended_Root_Header_Size : constant := 36;
      Revision : Byte;
      Length : Long_Long_Integer;
      Extent : Positive;
      Address : Address_Value;
   begin
      if Data'Length < 20 then
         return (Status => Truncated);
      end if;
      for I in Magic'Range loop
         if Data (Data'First + I - 1) /= Character'Pos (Magic (I)) then
            return (Status => Wrong_Signature);
         end if;
      end loop;
      if Sum (Data (Data'First .. Data'First + 19)) /= 0 then
         return (Status => Bad_Checksum);
      end if;
      Revision := Data (Data'First + 15);
      if Revision = 0 then
         Address := Address_Value (U32 (Data, 16));
         if Address = 0 then
            return (Status => Missing_Root);
         end if;
         return (Accepted, RSDT, Address, 20);
      elsif Revision = 1 then
         return (Status => Unsupported_Revision);
      end if;
      if Data'Length < Extended_Root_Header_Size then
         return (Status => Truncated);
      end if;
      Length := Long_Long_Integer (U32 (Data, 20));
      if Length < Extended_Root_Header_Size then
         return (Status => Invalid_Length);
      elsif Length > Long_Long_Integer (Positive'Last) then
         return (Status => Truncated);
      end if;
      Extent := Positive (Length);
      if Extent < Extended_Root_Header_Size or else Extent > Data'Length then
         return (Status => Truncated);
      end if;
      pragma Assert_And_Cut (Extent in Extended_Root_Header_Size .. Data'Length);
      if Sum (Data (Data'First .. Data'First + (Extent - 1))) /= 0 then
         return (Status => Bad_Checksum);
      end if;
      Address := Unsigned_64 (U32 (Data, 24)) or
        Shift_Left (Unsigned_64 (U32 (Data, 28)), 32);
      if Address /= 0 then
         return (Accepted, XSDT, Address, Extent);
      end if;
      Address := Address_Value (U32 (Data, 16));
      if Address = 0 then
         return (Status => Missing_Root);
      end if;
      return (Accepted, RSDT, Address, Extent);
   end Read_Root;

   function Read_Table
     (Data : Bytes; Expected : Signature) return Table_Result
   is
      Length : Long_Long_Integer;
      Extent : Positive;
   begin
      if Data'Length < Table_Header_Size then
         return (Status => Truncated);
      end if;
      for I in Expected'Range loop
         if Data (Data'First + I - 1) /= Character'Pos (Expected (I)) then
            return (Status => Wrong_Signature);
         end if;
      end loop;
      Length := Long_Long_Integer (U32 (Data, 4));
      if Length < Table_Header_Size then
         return (Status => Invalid_Length);
      elsif Length > Long_Long_Integer (Positive'Last) then
         return (Status => Truncated);
      end if;
      Extent := Positive (Length);
      if Extent < Table_Header_Size or else Extent > Data'Length then
         return (Status => Truncated);
      end if;
      pragma Assert_And_Cut (Extent in Table_Header_Size .. Data'Length);
      if Sum (Data (Data'First .. Data'First + (Extent - 1))) /= 0 then
         return (Status => Bad_Checksum);
      end if;
      return (Accepted, Extent, Data (Data'First + 8));
   end Read_Table;
end Firmware_Tables;

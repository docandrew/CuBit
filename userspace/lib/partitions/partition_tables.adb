------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Partition_Tables with SPARK_Mode is
   Byte_Bits : constant := 8;

   --  Protective MBR (UEFI 5.2.3), zero-based offsets.
   MBR_Records       : constant := 446;
   MBR_Record_Bytes  : constant := 16;
   MBR_Record_Count  : constant := 4;
   MBR_Type_Offset   : constant := 4;
   MBR_Signature     : constant := 510;
   Signature_Low     : constant Unsigned_8 := 16#55#;
   Signature_High    : constant Unsigned_8 := 16#AA#;
   Protective_Type   : constant Unsigned_8 := 16#EE#;
   Empty_Type        : constant Unsigned_8 := 16#00#;

   --  GPT header (UEFI 5.3.2), zero-based offsets.
   GPT_Signature : constant Bytes (1 .. 8) :=
     [Character'Pos ('E'), Character'Pos ('F'), Character'Pos ('I'),
      Character'Pos (' '), Character'Pos ('P'), Character'Pos ('A'),
      Character'Pos ('R'), Character'Pos ('T')];
   Header_Size_Offset  : constant := 12;
   Header_CRC_Offset   : constant := 16;
   My_LBA_Offset       : constant := 24;
   Backup_LBA_Offset   : constant := 32;
   First_Usable_Offset : constant := 40;
   Last_Usable_Offset  : constant := 48;
   Disk_GUID_Offset    : constant := 56;
   Entries_LBA_Offset  : constant := 72;
   Entry_Count_Offset  : constant := 80;
   Entry_Size_Offset   : constant := 84;
   Entries_CRC_Offset  : constant := 88;
   Minimum_Header      : constant := 92;
   --  Arrays larger than this are refused (the usual table has 128).
   Maximum_Entries     : constant := 1024;

   --  Partition entry (UEFI 5.3.3), zero-based offsets.
   Entry_Unique_Offset : constant := 16;
   Entry_First_Offset  : constant := 32;
   Entry_Last_Offset   : constant := 40;
   Entry_Flags_Offset  : constant := 48;

   CRC_Polynomial : constant Unsigned_32 := 16#EDB8_8320#;

   function Little_Endian (Data : Bytes; Offset, Width : Natural)
     return Unsigned_64
     with Pre => Width in 1 .. 8 and then Data'Length >= Width
                 and then Offset <= Data'Length - Width;

   function Little_Endian (Data : Bytes; Offset, Width : Natural)
     return Unsigned_64
   is
      Result : Unsigned_64 := 0;
   begin
      for Index in reverse 0 .. Width - 1 loop
         Result := Shift_Left (Result, Byte_Bits)
           or Unsigned_64 (Data (Data'First + Offset + Index));
      end loop;
      return Result;
   end Little_Endian;

   function Word (Data : Bytes; Offset : Natural) return Unsigned_32 is
     (Unsigned_32 (Data (Data'First + Offset))
      or Shift_Left (Unsigned_32 (Data (Data'First + Offset + 1)), Byte_Bits)
      or Shift_Left (Unsigned_32 (Data (Data'First + Offset + 2)), 2 * Byte_Bits)
      or Shift_Left (Unsigned_32 (Data (Data'First + Offset + 3)), 3 * Byte_Bits))
     with Pre => Data'Length >= 4 and then Offset <= Data'Length - 4;

   function Double (Data : Bytes; Offset : Natural) return Unsigned_64 is
     (Little_Endian (Data, Offset, 8))
     with Pre => Data'Length >= 8 and then Offset <= Data'Length - 8;

   function GUID_At (Data : Bytes; Offset : Natural) return GUID is
     ([for Index in GUID'Range => Data (Data'First + (Offset + Index - 1))])
     with Pre => Data'Length >= GUID'Length
                 and then Offset <= Data'Length - GUID'Length;

   function Classify_MBR (Data : Sector) return MBR_Kind is
      Protective, Other : Boolean := False;
   begin
      if Data (MBR_Signature + 1) /= Signature_Low
        or else Data (MBR_Signature + 2) /= Signature_High
      then
         return No_Table;
      end if;
      for Index in 0 .. MBR_Record_Count - 1 loop
         declare
            Kind : constant Unsigned_8 :=
              Data (MBR_Records + Index * MBR_Record_Bytes + MBR_Type_Offset + 1);
         begin
            if Kind = Protective_Type then
               Protective := True;
            elsif Kind /= Empty_Type then
               Other := True;
            end if;
         end;
      end loop;
      return (if Protective and then not Other then Protective_GPT
              else Legacy_MBR);
   end Classify_MBR;

   function CRC32 (Data : Bytes) return Unsigned_32 is
      CRC : Unsigned_32 := Unsigned_32'Last;
   begin
      for Value of Data loop
         CRC := CRC xor Unsigned_32 (Value);
         for Bit in 1 .. Byte_Bits loop
            CRC := (if (CRC and 1) /= 0
                    then Shift_Right (CRC, 1) xor CRC_Polynomial
                    else Shift_Right (CRC, 1));
         end loop;
      end loop;
      return not CRC;
   end CRC32;

   procedure Decode_Header
     (Data : Bytes; Location, Blocks : Unsigned_64;
      Header : out GPT_Header; Result : out Header_Result)
   is
      Frame : Sector := Data (Data'First .. Data'First + (Sector_Bytes - 1));
      Size : constant Unsigned_32 := Word (Frame, Header_Size_Offset);
      Stored_CRC : constant Unsigned_32 := Word (Frame, Header_CRC_Offset);
   begin
      Header := (others => <>);
      if Frame (1 .. GPT_Signature'Length) /= GPT_Signature then
         Result := Bad_Signature;
         return;
      elsif Size < Minimum_Header or else Size > Sector_Bytes then
         Result := Bad_Size;
         return;
      end if;
      Frame (Header_CRC_Offset + 1 .. Header_CRC_Offset + 4) := [others => 0];
      if CRC32 (Frame (1 .. Natural (Size))) /= Stored_CRC then
         Result := Bad_Checksum;
         return;
      end if;
      Header :=
        (Current_LBA  => Double (Frame, My_LBA_Offset),
         Backup_LBA   => Double (Frame, Backup_LBA_Offset),
         First_Usable => Double (Frame, First_Usable_Offset),
         Last_Usable  => Double (Frame, Last_Usable_Offset),
         Disk         => GUID_At (Frame, Disk_GUID_Offset),
         Entries_LBA  => Double (Frame, Entries_LBA_Offset),
         Entry_Count  => Word (Frame, Entry_Count_Offset),
         Entry_Bytes  => Word (Frame, Entry_Size_Offset),
         Entries_CRC  => Word (Frame, Entries_CRC_Offset));
      if Header.Current_LBA /= Location
        or else Header.First_Usable > Header.Last_Usable
        or else Header.Last_Usable >= Blocks
        or else Header.Backup_LBA >= Blocks
      then
         Result := Bad_Location;
      elsif Header.Entry_Count = 0
        or else Header.Entry_Count > Maximum_Entries
        or else Header.Entry_Bytes < Minimum_Entry_Bytes
        or else Header.Entry_Bytes mod Minimum_Entry_Bytes /= 0
        or else Header.Entries_LBA >= Blocks
      then
         Result := Bad_Entries;
      else
         Result := Header_Valid;
      end if;
   end Decode_Header;

   function Decode_Entry (Data : Bytes; Offset : Natural)
     return Partition_Entry is
     ((Kind       => GUID_At (Data, Offset),
       Unique     => GUID_At (Data, Offset + Entry_Unique_Offset),
       First      => Double (Data, Offset + Entry_First_Offset),
       Last       => Double (Data, Offset + Entry_Last_Offset),
       Attributes => Double (Data, Offset + Entry_Flags_Offset)));
end Partition_Tables;

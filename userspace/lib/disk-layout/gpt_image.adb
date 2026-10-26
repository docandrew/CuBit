------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Partition_Tables; use Partition_Tables;

package body GPT_Image with SPARK_Mode is

   subtype Bytes is Disk_Layout.Bytes;
   subtype GUID is Disk_Layout.GUID;

   Largest_MBR_Size : constant := 16#FFFF_FFFF#;
   CHS_Bytes : constant := 3;
   Byte_Bits : constant := 8;
   Byte_Mask : constant := 16#FF#;

   procedure Put_CHS (Data : in out Bytes; Offset : Natural; Value : Unsigned_32)
     with Pre => Data'First = 1 and then Data'Length >= CHS_Bytes
                 and then Offset <= Data'Length - CHS_Bytes
   is
   begin
      for I in 0 .. CHS_Bytes - 1 loop
         Put_8 (Data, Offset + I,
                Unsigned_8 (Shift_Right (Value, Byte_Bits * (CHS_Bytes - 1 - I))
                            and Byte_Mask));
      end loop;
   end Put_CHS;

   function Protective_MBR (Shape : Disk_Shape) return Block_Image is
      Result : Block_Image (1 .. Shape.Block_Size) := [others => 0];
      Record_At : constant := MBR_Records;
   begin
      Put_CHS (Result, Record_At + MBR_CHS_Start_Offset, Protective_Start_CHS);
      Put_8 (Result, Record_At + MBR_Type_Offset, Protective_Type);
      Put_CHS (Result, Record_At + MBR_CHS_End_Offset, Protective_End_CHS);
      Put_32 (Result, Record_At + MBR_First_LBA_Offset, 1);
      Put_32 (Result, Record_At + MBR_Size_Offset,
              Unsigned_32 (Unsigned_64'Min (Shape.Blocks - 1, Largest_MBR_Size)));
      Put_8 (Result, MBR_Signature, Signature_Low);
      Put_8 (Result, MBR_Signature + 1, Signature_High);
      return Result;
   end Protective_MBR;

   --  An entry's name, UTF-16LE (ASCII names only).
   procedure Put_Name (Data : in out Bytes; Offset : Natural; Name : String)
     with Pre => Data'First = 1 and then Name'Length <= Entry_Name_Bytes / 2
                 and then Data'Length >= Entry_Name_Bytes
                 and then Offset <= Data'Length - Entry_Name_Bytes
   is
   begin
      for I in 0 .. Name'Length - 1 loop
         Put_16 (Data, Offset + 2 * I, Character'Pos (Name (Name'First + I)));
      end loop;
   end Put_Name;

   procedure Put_Entry
     (Data : in out Entry_Array; Index : Natural; Kind, Unique : GUID;
      First, Last : Block_Address; Name : String)
     with Pre => Index < Entry_Count and then Name'Length <= Entry_Name_Bytes / 2
   is
      At_Byte : constant Natural := Index * Entry_Bytes;
   begin
      Put_GUID (Data, At_Byte, Kind);
      Put_GUID (Data, At_Byte + Entry_Unique_Offset, Unique);
      Put_64 (Data, At_Byte + Entry_First_Offset, First);
      Put_64 (Data, At_Byte + Entry_Last_Offset, Last);
      Put_64 (Data, At_Byte + Entry_Flags_Offset, 0);
      Put_Name (Data, At_Byte + Entry_Name_Offset, Name);
   end Put_Entry;

   ESP_Entry  : constant := 0;
   Root_Entry : constant := 1;

   function Entries (Shape : Disk_Shape; Request : Layout_Request)
     return Entry_Array
   is
      Result : Entry_Array := [others => 0];
   begin
      Put_Entry (Result, ESP_Entry, EFI_System, Request.ESP_Id,
                 ESP_First (Shape), ESP_Last (Shape, Request), ESP_Name);
      Put_Entry (Result, Root_Entry, Linux_Filesystem, Request.Root_Id,
                 Root_First (Shape, Request), Root_Last (Shape, Request), Root_Name);
      return Result;
   end Entries;

   function Header
     (Shape : Disk_Shape; Request : Layout_Request; Copy : Header_Copy;
      Entries_CRC : Unsigned_32) return Block_Image
   is
      Result : Block_Image (1 .. Shape.Block_Size) := [others => 0];
      Other : constant Header_Copy :=
        (case Copy is when Primary => Backup, when Backup => Primary);
   begin
      for I in GPT_Signature'Range loop
         Result (I) := GPT_Signature (I);
      end loop;
      Put_32 (Result, Revision_Offset, GPT_Revision);
      Put_32 (Result, Header_Size_Offset, Minimum_Header);
      Put_64 (Result, My_LBA_Offset, Header_LBA (Shape, Copy));
      Put_64 (Result, Backup_LBA_Offset, Header_LBA (Shape, Other));
      Put_64 (Result, First_Usable_Offset, First_Usable (Shape));
      Put_64 (Result, Last_Usable_Offset, Last_Usable (Shape));
      Put_GUID (Result, Disk_GUID_Offset, Request.Disk_Id);
      Put_64 (Result, Entries_LBA_Offset, Entries_LBA (Shape, Copy));
      Put_32 (Result, Entry_Count_Offset, Entry_Count);
      Put_32 (Result, Entry_Size_Offset, Entry_Bytes);
      Put_32 (Result, Entries_CRC_Offset, Entries_CRC);
      --  The header CRC covers the header's bytes with its own field zero.
      Put_32 (Result, Header_CRC_Offset, CRC32 (Result (1 .. Minimum_Header)));
      return Result;
   end Header;

end GPT_Image;

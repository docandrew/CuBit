------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Read-only decoding of a disk's partition table: the protective MBR and
--  the GUID partition table (UEFI 2.10 section 5). Fields are
--  little-endian. Header and entry-array CRC32s are checked.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Partition_Tables with SPARK_Mode, Pure is
   type Bytes is array (Positive range <>) of Unsigned_8;

   Sector_Bytes : constant := 512;
   subtype Sector is Bytes (1 .. Sector_Bytes);

   type MBR_Kind is (No_Table, Protective_GPT, Legacy_MBR);

   --  LBA 0: a protective MBR (one type EEh record) announces a GPT.
   function Classify_MBR (Data : Sector) return MBR_Kind;

   type GUID is array (1 .. 16) of Unsigned_8;

   type GPT_Header is record
      Current_LBA     : Unsigned_64 := 0;
      Backup_LBA      : Unsigned_64 := 0;
      First_Usable    : Unsigned_64 := 0;
      Last_Usable     : Unsigned_64 := 0;
      Disk            : GUID := [others => 0];
      Entries_LBA     : Unsigned_64 := 0;
      Entry_Count     : Unsigned_32 := 0;
      Entry_Bytes     : Unsigned_32 := 0;
      Entries_CRC     : Unsigned_32 := 0;
   end record;

   --  The entry size UEFI requires the array to use: 128 * 2 ** n bytes.
   Minimum_Entry_Bytes : constant := 128;

   type Header_Result is
     (Header_Valid, Bad_Signature, Bad_Size, Bad_Checksum, Bad_Location,
      Bad_Entries);

   --  The header in Data (one logical block of Block_Bytes) read from LBA
   --  Location of a disk of Blocks blocks.
   procedure Decode_Header
     (Data : Bytes; Location, Blocks : Unsigned_64;
      Header : out GPT_Header; Result : out Header_Result)
     with Pre => Data'Length >= Sector_Bytes;

   --  CRC32 (IEEE 802.3, reflected), as the GPT stores it.
   function CRC32 (Data : Bytes) return Unsigned_32;

   type Partition_Entry is record
      Kind       : GUID := [others => 0];
      Unique     : GUID := [others => 0];
      First      : Unsigned_64 := 0;
      Last       : Unsigned_64 := 0;
      Attributes : Unsigned_64 := 0;
   end record;

   function Is_Unused (Item : Partition_Entry) return Boolean is
     (Item.Kind = [1 .. 16 => 0]);

   --  The entry at Data (Data'First + Offset ..), Offset + 128 <= length.
   function Decode_Entry (Data : Bytes; Offset : Natural)
     return Partition_Entry
     with Pre => Data'Length >= Minimum_Entry_Bytes
                 and then Offset <= Data'Length - Minimum_Entry_Bytes;

   --  Well-known partition types, as stored on disk (the first three GUID
   --  fields little-endian).
   EFI_System : constant GUID :=
     [16#28#, 16#73#, 16#2A#, 16#C1#, 16#1F#, 16#F8#, 16#D2#, 16#11#,
      16#BA#, 16#4B#, 16#00#, 16#A0#, 16#C9#, 16#3E#, 16#C9#, 16#3B#];
   Microsoft_Reserved : constant GUID :=
     [16#16#, 16#E3#, 16#C9#, 16#E3#, 16#5C#, 16#0B#, 16#B8#, 16#4D#,
      16#81#, 16#7D#, 16#F9#, 16#2D#, 16#F0#, 16#02#, 16#15#, 16#AE#];
   Microsoft_Basic_Data : constant GUID :=
     [16#A2#, 16#A0#, 16#D0#, 16#EB#, 16#E5#, 16#B9#, 16#33#, 16#44#,
      16#87#, 16#C0#, 16#68#, 16#B6#, 16#B7#, 16#26#, 16#99#, 16#C7#];
   Windows_Recovery : constant GUID :=
     [16#A4#, 16#BB#, 16#94#, 16#DE#, 16#D1#, 16#06#, 16#40#, 16#4D#,
      16#A1#, 16#6A#, 16#BF#, 16#D5#, 16#01#, 16#79#, 16#D6#, 16#AC#];
   Linux_Filesystem : constant GUID :=
     [16#AF#, 16#3D#, 16#C6#, 16#0F#, 16#83#, 16#84#, 16#72#, 16#47#,
      16#8E#, 16#79#, 16#3D#, 16#69#, 16#D8#, 16#47#, 16#7D#, 16#E4#];

   --  A short name for a log line.
   function Type_Name (Kind : GUID) return String is
     (if Kind = EFI_System then "esp"
      elsif Kind = Microsoft_Reserved then "msr"
      elsif Kind = Microsoft_Basic_Data then "basic"
      elsif Kind = Windows_Recovery then "recovery"
      elsif Kind = Linux_Filesystem then "linux"
      else "other");

   --  An entry is usable if unused, or inside the usable range and ordered.
   function Entry_In_Range (Header : GPT_Header; Item : Partition_Entry)
     return Boolean is
     (Is_Unused (Item)
      or else (Item.First <= Item.Last
               and then Item.First >= Header.First_Usable
               and then Item.Last <= Header.Last_Usable));
end Partition_Tables;

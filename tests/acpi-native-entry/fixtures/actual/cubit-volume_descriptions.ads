------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Volume.Description.V1 (docs/filesystem-protocol-v2.md step 7): what the
--  filesystem service answers Queue_Describe_Volume with, one record per
--  volume: its name, format, block size, total and free blocks and inodes,
--  and whether it is read-only, journaled and can flush durably.
--
--  @description
--  Little-endian, Record_Bytes long:
--    0 version (u16, 1)     2 kind (u8)           3 flags (u8)
--    4 name length (u8)     5 .. 7 zero
--    8 block size (u32)    12 .. 15 zero
--   16 total blocks (u64)  24 free blocks (u64)
--   32 releasing blocks (u64): freed, free once the running transaction
--      commits (they count as free to a client that asks how much room)
--   40 total inodes (u64)  48 free inodes (u64)
--   56 name (Maximum_Name_Bytes, zero-padded)
--  Free counts are the service's in-memory superblock's, not the disk's.
--  Read-only formats (ISO 9660, the boot archive) report no free space.
--
--  Proved (tests/volume-descriptions, level 2): no run-time errors for any
--  bytes; Decode accepts only records Valid describes. Tested: Decode reads
--  back what Encode wrote; damaged records are refused.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Volume_Descriptions with Pure, SPARK_Mode is

   Version : constant := 1;
   --  A volume name (the service's Volume_List.Maximum_Name_Bytes).
   Maximum_Name_Bytes : constant := 48;

   Version_At         : constant := 0;
   Kind_At            : constant := 2;
   Flags_At           : constant := 3;
   Name_Length_At     : constant := 4;
   Block_Size_At      : constant := 8;
   Total_Blocks_At    : constant := 16;
   Free_Blocks_At     : constant := 24;
   Releasing_Blocks_At : constant := 32;
   Total_Inodes_At    : constant := 40;
   Free_Inodes_At     : constant := 48;
   Name_At            : constant := 56;
   Record_Bytes       : constant := Name_At + Maximum_Name_Bytes;

   type Volume_Kind is (Ext2, Ext3, ISO_9660, Boot_Archive);
   Kind_Codes : constant array (Volume_Kind) of Unsigned_8 :=
     [Ext2 => 1, Ext3 => 2, ISO_9660 => 3, Boot_Archive => 4];

   --  Flags.
   Read_Only     : constant := 1;
   Journaled     : constant := 2;   --  ext3 with a journal this service writes
   Durable_Flush : constant := 4;   --  a flush makes completed writes durable
   Known_Flags   : constant := Read_Only + Journaled + Durable_Flush;

   Smallest_Block : constant := 512;
   Largest_Block  : constant := 65_536;
   subtype Block_Size is Unsigned_32 range Smallest_Block .. Largest_Block;
   function Power_Of_Two (Value : Unsigned_32) return Boolean is
     (Value /= 0 and then (Value and (Value - 1)) = 0);

   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;
   subtype Name_Index is Positive range 1 .. Maximum_Name_Bytes;
   type Name_Bytes is array (Name_Index) of Unsigned_8;
   subtype Record_Index is Natural range 0 .. Record_Bytes - 1;
   type Record_Image is array (Record_Index) of Unsigned_8;

   type Description is record
      Kind             : Volume_Kind := Ext2;
      Flags            : Unsigned_8 := 0;
      Block            : Block_Size := Smallest_Block;
      Total_Blocks     : Unsigned_64 := 0;
      Free_Blocks      : Unsigned_64 := 0;
      Releasing_Blocks : Unsigned_64 := 0;
      Total_Inodes     : Unsigned_64 := 0;
      Free_Inodes      : Unsigned_64 := 0;
      Name             : Name_Bytes := [others => 0];
      Length           : Name_Length := 0;
   end record;

   --  A volume name: nonempty, printable ASCII, no '/' and no '@'.
   function Valid_Name (Name : Name_Bytes; Length : Name_Length) return Boolean is
     (Length > 0 and then
      (for all I in 1 .. Length => Name (I) in 16#21# .. 16#7E#
         and then Name (I) /= Character'Pos ('/') and then Name (I) /= Character'Pos ('@')));

   function Valid (Item : Description) return Boolean is
     (Valid_Name (Item.Name, Item.Length)
      and then (Item.Flags and not Known_Flags) = 0
      and then Power_Of_Two (Item.Block)
      and then Item.Free_Blocks <= Item.Total_Blocks
      and then Item.Releasing_Blocks <= Item.Total_Blocks - Item.Free_Blocks
      and then Item.Free_Inodes <= Item.Total_Inodes
      and then (if Item.Kind in ISO_9660 | Boot_Archive then
                  (Item.Flags and Read_Only) /= 0 and then Item.Free_Blocks = 0
                  and then Item.Releasing_Blocks = 0 and then Item.Free_Inodes = 0)
      and then (if (Item.Flags and Journaled) /= 0 then Item.Kind = Ext3));

   procedure Encode (Item : Description; Into : out Record_Image)
   with Pre => Valid (Item);

   procedure Decode (From : Record_Image; Item : out Description; OK : out Boolean)
   with Post => (if OK then Valid (Item));

end CuBit.Volume_Descriptions;

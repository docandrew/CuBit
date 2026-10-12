------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The ext2/ext3 on-disk format: its numbers, constants and the codecs of
--  its byte-level structures (directory records, inode mode and size
--  fields). Pure and proved (SPARK); no storage, no addresses. Values and
--  names follow Linux's include/linux/ext2_fs.h and e2fsprogs' ext2_fs.h,
--  so a volume stays exactly Linux-compatible.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Ext2_Format with Pure, SPARK_Mode is

   --  Proved free of run-time errors (SPARK, level 2): no checks needed.
   pragma Suppress (All_Checks);

   --  Numbers. Each is its own type, so a block number can't silently be
   --  used as an inode number, a group, a count or a byte position, and
   --  none wraps around: arithmetic on them is checked (and proved). They
   --  are stored in 32 bits, as on disk.
   Word_Last : constant := 2 ** 32 - 1;   --  the on-disk fields are 32-bit
   type Block_Number is range 0 .. Word_Last with Size => 32;   --  a block of the volume
   type Inode_Number is range 0 .. Word_Last with Size => 32;   --  1-based; 0 is no inode
   type Group_Number is range 0 .. Word_Last with Size => 32;   --  a block group
   type Count_32 is range 0 .. Word_Last with Size => 32;       --  an on-disk count or size
   type Byte_Offset is range 0 .. 2 ** 63 - 1;  --  a byte position on the volume

   No_Block : constant Block_Number := 0;  --  an absent block pointer
   No_Inode : constant Inode_Number := 0;  --  an unused directory record

   type Byte_Array is array (Natural range <>) of Unsigned_8;

   ---------------------------------------------------------------------------
   --  Superblock
   ---------------------------------------------------------------------------
   Superblock_Offset : constant := 1024;   --  bytes from the volume start
   Superblock_Bytes  : constant := 1024;
   Signature         : constant := 16#EF53#;   --  EXT2_SUPER_MAGIC
   Root_Inode        : constant := 2;          --  EXT2_ROOT_INO

   --  s_log_block_size is log2 (block size) - Block_Shift_Base. The driver
   --  admits 1, 2 and 4 KiB blocks.
   Block_Shift_Base      : constant := 10;
   Minimum_Block_Bytes   : constant := 2 ** Block_Shift_Base;
   Maximum_Block_Shift   : constant := 2;
   Maximum_Block_Bytes   : constant := Minimum_Block_Bytes * 2 ** Maximum_Block_Shift;

   --  Revision 0 ("good old") volumes have fixed inode geometry.
   Dynamic_Revision     : constant := 1;    --  EXT2_DYNAMIC_REV
   Good_Old_Inode_Bytes : constant := 128;  --  EXT2_GOOD_OLD_INODE_SIZE
   Good_Old_First_Inode : constant := 11;   --  EXT2_GOOD_OLD_FIRST_INO

   --  ext3 fields past the 104-byte prefix Ext2.Superblock overlays.
   Journal_Inode_Offset  : constant := 16#E0#;   --  s_journal_inum
   Journal_Device_Offset : constant := 16#E4#;   --  s_journal_dev
   Last_Orphan_Offset    : constant := 16#E8#;   --  s_last_orphan

   ---------------------------------------------------------------------------
   --  Block groups
   ---------------------------------------------------------------------------
   Group_Descriptor_Bytes : constant := 32;   --  struct ext2_group_desc
   Bits_Per_Byte          : constant := 8;
   --  The largest bitmap this driver reads: one 4 KiB bitmap block.
   Maximum_Bitmap_Bytes   : constant := Maximum_Block_Bytes;
   Block_Pointer_Bytes    : constant := 4;    --  an indirect-block entry

   ---------------------------------------------------------------------------
   --  Inodes
   ---------------------------------------------------------------------------
   --  i_flags
   Index_Flag   : constant Unsigned_32 := 16#0000_1000#;   --  EXT2_INDEX_FL (htree)
   Extents_Flag : constant Unsigned_32 := 16#0008_0000#;   --  EXT4_EXTENTS_FL

   --  i_mode: the file type is its top four bits.
   Mode_Type_Shift : constant := 12;
   subtype Mode_Type is Unsigned_8 range 0 .. 16#F#;
   Type_FIFO      : constant Mode_Type := 16#1#;   --  S_IFIFO
   Type_Character : constant Mode_Type := 16#2#;   --  S_IFCHR
   Type_Directory : constant Mode_Type := 16#4#;   --  S_IFDIR
   Type_Block     : constant Mode_Type := 16#6#;   --  S_IFBLK
   Type_Regular   : constant Mode_Type := 16#8#;   --  S_IFREG
   Type_Symlink   : constant Mode_Type := 16#A#;   --  S_IFLNK
   Type_Socket    : constant Mode_Type := 16#C#;   --  S_IFSOCK

   --  New inodes: rw-r--r-- regular files and rwxr-xr-x directories.
   Regular_Mode   : constant Unsigned_16 := 16#81A4#;   --  S_IFREG | 0644
   Directory_Mode : constant Unsigned_16 := 16#41ED#;   --  S_IFDIR | 0755

   --  The file type of an i_mode value.
   function Inode_Type (Mode : Unsigned_16) return Mode_Type is
     (Mode_Type (Shift_Right (Mode, Mode_Type_Shift)));

   --  A file's size is i_size (low word) and i_size_high (high word).
   Word_Modulus : constant := 2 ** 32;
   function File_Size (Low, High : Unsigned_32) return Unsigned_64 is
     (Unsigned_64 (High) * Word_Modulus + Unsigned_64 (Low));
   function Size_Low (Size : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 (Size mod Word_Modulus));
   function Size_High (Size : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 (Size / Word_Modulus));

   ---------------------------------------------------------------------------
   --  Directory records (struct ext2_dir_entry_2)
   ---------------------------------------------------------------------------
   --  inode (4), rec_len (2), name_len (1), file_type (1), then the name.
   Record_Header_Bytes : constant := 8;
   Record_Alignment    : constant := 4;
   Maximum_Name_Bytes  : constant := 255;
   Inode_Field    : constant := 0;
   Span_Field     : constant := 4;
   Length_Field   : constant := 6;
   Type_Field     : constant := 7;

   --  file_type (EXT2_FT_*)
   Filetype_Unknown   : constant Unsigned_8 := 0;
   Filetype_Regular   : constant Unsigned_8 := 1;
   Filetype_Directory : constant Unsigned_8 := 2;
   Filetype_Character : constant Unsigned_8 := 3;
   Filetype_Block     : constant Unsigned_8 := 4;
   Filetype_FIFO      : constant Unsigned_8 := 5;
   Filetype_Socket    : constant Unsigned_8 := 6;
   Filetype_Symlink   : constant Unsigned_8 := 7;

   --  The record file type of an inode of this mode type.
   function Directory_File_Type (Kind : Mode_Type) return Unsigned_8 is
     (case Kind is
        when Type_Regular   => Filetype_Regular,
        when Type_Directory => Filetype_Directory,
        when Type_FIFO      => Filetype_FIFO,
        when Type_Character => Filetype_Character,
        when Type_Block     => Filetype_Block,
        when Type_Symlink   => Filetype_Symlink,
        when Type_Socket    => Filetype_Socket,
        when others         => Filetype_Unknown);

   subtype Record_Span is Natural range 0 .. 2 ** 16 - 1;
   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;

   type Record_Header is record
      Inode     : Inode_Number := No_Inode;
      Span      : Record_Span := 0;   --  rec_len: bytes to the next record
      Length    : Name_Length := 0;   --  name_len
      File_Type : Unsigned_8 := Filetype_Unknown;
   end record;

   --  Little-endian fields.
   function Le16 (Data : Byte_Array; Position : Natural) return Unsigned_16 is
     (Unsigned_16 (Data (Position)) or
      Shift_Left (Unsigned_16 (Data (Position + 1)), 8))
     with Pre => Position >= Data'First and then Position < Data'Last;

   function Le32 (Data : Byte_Array; Position : Natural) return Unsigned_32 is
     (Unsigned_32 (Data (Position)) or
      Shift_Left (Unsigned_32 (Data (Position + 1)), 8) or
      Shift_Left (Unsigned_32 (Data (Position + 2)), 16) or
      Shift_Left (Unsigned_32 (Data (Position + 3)), 24))
     with Pre => Position >= Data'First and then Data'Last >= 3
                 and then Position <= Data'Last - 3;

   --  The header of the record at Position.
   function Header (Data : Byte_Array; Position : Natural) return Record_Header
     with Pre  => Position >= Data'First and then Data'Last >= Record_Header_Bytes - 1
                  and then Position <= Data'Last - (Record_Header_Bytes - 1),
          Post => Header'Result.Inode =
                    Inode_Number (Le32 (Data, Position + Inode_Field)) and then
                  Header'Result.Span =
                    Natural (Le16 (Data, Position + Span_Field)) and then
                  Header'Result.Length = Natural (Data (Position + Length_Field)) and then
                  Header'Result.File_Type = Data (Position + Type_Field);

   --  A record's shape is valid within Room bytes (to the end of its
   --  block, or of the directory): a whole header, 4-byte aligned, inside
   --  the room, with its name inside the record. Only then may the name be
   --  viewed.
   function Fits (Item : Record_Header; Room : Natural) return Boolean is
     (Item.Span >= Record_Header_Bytes and then
      Item.Span mod Record_Alignment = 0 and then
      Item.Span <= Room and then
      Item.Length <= Item.Span - Record_Header_Bytes);

   --  The bytes a record with a name of Length needs (EXT2_DIR_REC_LEN).
   function Record_Bytes (Length : Name_Length) return Record_Span is
     ((Record_Header_Bytes + Length + Record_Alignment - 1) / Record_Alignment *
        Record_Alignment)
     with Post => Record_Bytes'Result >= Record_Header_Bytes + Length and then
                  Record_Bytes'Result mod Record_Alignment = 0;

   pragma Inline_Always (Inode_Type, File_Size, Size_Low, Size_High, Le16, Le32, Header, Fits, Record_Bytes, Directory_File_Type);

end Ext2_Format;

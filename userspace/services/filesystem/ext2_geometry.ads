------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Volume geometry: where an inode, a group descriptor or a block's bitmap
--  bit is, from the superblock's counts. Admission checks a volume's
--  geometry once (Supported); every location function then requires a
--  valid geometry and is proved free of overflow and in range (SPARK), so
--  a malformed superblock can't make an on-disk count a divisor, an array
--  bound or a bitmap index on the I/O path.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Ext2_Format; use Ext2_Format;

package Ext2_Geometry with Pure, SPARK_Mode is

   --  Proved free of run-time errors (SPARK, level 2): no checks needed.
   pragma Suppress (All_Checks);

   --  The superblock fields geometry depends on.
   type Geometry is record
      Block_Count      : Count_32 := 0;   --  s_blocks_count
      First_Data_Block : Count_32 := 0;   --  s_first_data_block
      Blocks_Per_Group : Count_32 := 0;   --  s_blocks_per_group
      Inode_Count      : Count_32 := 0;   --  s_inodes_count
      Inodes_Per_Group : Count_32 := 0;   --  s_inodes_per_group
      Block_Bytes      : Count_32 := 0;   --  1024 << s_log_block_size
      Inode_Bytes      : Count_32 := 0;   --  s_inode_size (128 in revision 0)
   end record;

   function Power_Of_Two (Value : Count_32) return Boolean is
     (Value in 1 | 2 | 4 | 8 | 16 | 32 | 64 | 128 | 256 | 512 | 1024 | 2048 | 4096 |
               8192 | 16384 | 32768 | 65536);

   --  What the driver relies on: an admitted block size; at least one
   --  data block, in groups no larger than one bitmap block can map; at
   --  least one inode, in groups no larger than one bitmap block can map,
   --  in power-of-two slots of at least the classic 128 bytes that fit in a
   --  block. Each location function requires only the part it uses.
   function Valid_Block_Size (Shape : Geometry) return Boolean is
     (Shape.Block_Bytes in Minimum_Block_Bytes | 2 * Minimum_Block_Bytes |
                           Maximum_Block_Bytes);

   function Valid_Blocks (Shape : Geometry) return Boolean is
     (Valid_Block_Size (Shape) and then
      Shape.Block_Count > Shape.First_Data_Block and then
      Shape.Blocks_Per_Group in 1 .. Shape.Block_Bytes * Bits_Per_Byte);

   function Valid_Inodes (Shape : Geometry) return Boolean is
     (Valid_Block_Size (Shape) and then
      Shape.Inode_Count > 0 and then
      Shape.Inodes_Per_Group in 1 .. Shape.Block_Bytes * Bits_Per_Byte and then
      Shape.Inode_Bytes >= Good_Old_Inode_Bytes and then
      Shape.Inode_Bytes <= Shape.Block_Bytes and then
      Power_Of_Two (Shape.Inode_Bytes));

   function Valid (Shape : Geometry) return Boolean is
     (Valid_Blocks (Shape) and then Valid_Inodes (Shape));

   --  The superblock fields admission checks.
   type Superblock_Fields is record
      Signature        : Unsigned_16 := 0;
      Block_Shift      : Unsigned_32 := 0;   --  s_log_block_size
      Block_Count      : Count_32 := 0;
      First_Data_Block : Count_32 := 0;
      Blocks_Per_Group : Count_32 := 0;
      Inode_Count      : Count_32 := 0;
      Inodes_Per_Group : Count_32 := 0;
      Free_Blocks      : Count_32 := 0;
      Free_Inodes      : Count_32 := 0;
      Revision         : Unsigned_32 := 0;   --  s_rev_level
      Inode_Bytes      : Unsigned_16 := 0;   --  s_inode_size (revision >= 1)
   end record;

   function Block_Bytes_Of (Shift : Unsigned_32) return Count_32 is
     (case Shift is
        when 0      => Minimum_Block_Bytes,
        when 1      => 2 * Minimum_Block_Bytes,
        when others => Maximum_Block_Bytes)
     with Pre => Shift <= Maximum_Block_Shift;

   function Inode_Bytes_Of (Fields : Superblock_Fields) return Count_32 is
     (if Fields.Revision >= Dynamic_Revision then Count_32 (Fields.Inode_Bytes)
      else Good_Old_Inode_Bytes);

   function Shape_Of (Fields : Superblock_Fields) return Geometry is
     ((Block_Count      => Fields.Block_Count,
       First_Data_Block => Fields.First_Data_Block,
       Blocks_Per_Group => Fields.Blocks_Per_Group,
       Inode_Count      => Fields.Inode_Count,
       Inodes_Per_Group => Fields.Inodes_Per_Group,
       Block_Bytes      =>
         (if Fields.Block_Shift <= Maximum_Block_Shift
          then Block_Bytes_Of (Fields.Block_Shift) else 0),
       Inode_Bytes      => Inode_Bytes_Of (Fields)));

   --  Admission: an ext2 signature, a valid geometry and free counts no
   --  larger than what they count.
   function Supported (Fields : Superblock_Fields) return Boolean
     with Post => (if Supported'Result then Valid (Shape_Of (Fields)));

   ---------------------------------------------------------------------------
   --  Locations (valid geometry)
   ---------------------------------------------------------------------------
   function Volume_Bytes (Shape : Geometry) return Byte_Offset is
     (Byte_Offset (Shape.Block_Count) * Byte_Offset (Shape.Block_Bytes))
     with Pre => Valid_Block_Size (Shape);

   --  The byte position of a block.
   function Block_Offset (Shape : Geometry; Block : Block_Number) return Byte_Offset is
     (Byte_Offset (Block) * Byte_Offset (Shape.Block_Bytes))
     with Pre => Valid_Block_Size (Shape);

   function Inode_Group_Count (Shape : Geometry) return Count_32 is
     (1 + (Shape.Inode_Count - 1) / Shape.Inodes_Per_Group)
     with Pre => Valid_Inodes (Shape);

   function Block_Group_Count (Shape : Geometry) return Count_32 is
     (1 + (Shape.Block_Count - Shape.First_Data_Block - 1) / Shape.Blocks_Per_Group)
     with Pre => Valid_Blocks (Shape);

   --  An inode's group and its index there.
   function Group_Of_Inode (Shape : Geometry; Inode : Inode_Number) return Group_Number is
     (Group_Number ((Count_32 (Inode) - 1) / Shape.Inodes_Per_Group))
     with Pre  => Valid_Inodes (Shape) and then Inode /= No_Inode,
          Post => (if Count_32 (Inode) <= Shape.Inode_Count then
                     Count_32 (Group_Of_Inode'Result) < Inode_Group_Count (Shape));

   function Index_In_Group (Shape : Geometry; Inode : Inode_Number) return Count_32 is
     ((Count_32 (Inode) - 1) mod Shape.Inodes_Per_Group)
     with Pre  => Valid_Inodes (Shape) and then Inode /= No_Inode,
          Post => Index_In_Group'Result < Shape.Inodes_Per_Group;

   --  Where a group's descriptor is: the table starts in the block after
   --  the superblock's.
   function Descriptor_Offset (Shape : Geometry; Group : Group_Number) return Byte_Offset is
     ((Byte_Offset (Shape.First_Data_Block) + 1) * Byte_Offset (Shape.Block_Bytes) +
        Byte_Offset (Group) * Group_Descriptor_Bytes)
     with Pre => Valid_Block_Size (Shape);

   --  Where an inode is in its group's inode table.
   function Inode_Offset
     (Shape : Geometry; Table : Block_Number; Index : Count_32) return Byte_Offset is
     (Block_Offset (Shape, Table) + Byte_Offset (Index) * Byte_Offset (Shape.Inode_Bytes))
     with Pre => Valid_Inodes (Shape) and then Index < Shape.Inodes_Per_Group;

   --  Block groups map blocks from First_Data_Block on.
   function In_Volume (Shape : Geometry; Block : Block_Number) return Boolean is
     (Count_32 (Block) >= Shape.First_Data_Block and then
      Count_32 (Block) < Shape.Block_Count);

   function Group_Of_Block (Shape : Geometry; Block : Block_Number) return Group_Number is
     (Group_Number ((Count_32 (Block) - Shape.First_Data_Block) / Shape.Blocks_Per_Group))
     with Pre  => Valid_Blocks (Shape) and then In_Volume (Shape, Block),
          Post => Count_32 (Group_Of_Block'Result) < Block_Group_Count (Shape);

   function Group_First_Block (Shape : Geometry; Group : Group_Number) return Block_Number
     with Pre  => Valid_Blocks (Shape) and then Count_32 (Group) < Block_Group_Count (Shape),
          Post => Count_32 (Group_First_Block'Result) =
                    Shape.First_Data_Block + Count_32 (Group) * Shape.Blocks_Per_Group
                  and then In_Volume (Shape, Group_First_Block'Result);

   --  A block's bit in its group's block bitmap.
   function Bit_Of_Block (Shape : Geometry; Block : Block_Number) return Count_32 is
     ((Count_32 (Block) - Shape.First_Data_Block) mod Shape.Blocks_Per_Group)
     with Pre  => Valid_Blocks (Shape) and then In_Volume (Shape, Block),
          Post => Bit_Of_Block'Result < Shape.Blocks_Per_Group and then
                  Block = Group_First_Block (Shape, Group_Of_Block (Shape, Block)) +
                            Block_Number (Bit_Of_Block'Result);

   --  The blocks a group maps (the last group may be short).
   function Blocks_In_Group (Shape : Geometry; Group : Group_Number) return Count_32 is
     (Count_32'Min (Shape.Blocks_Per_Group,
                    Shape.Block_Count - Count_32 (Group_First_Block (Shape, Group))))
     with Pre  => Valid_Blocks (Shape) and then Count_32 (Group) < Block_Group_Count (Shape),
          Post => Blocks_In_Group'Result in 1 .. Shape.Blocks_Per_Group;

   --  The bytes of a group's block bitmap and inode bitmap this driver uses.
   function Block_Bitmap_Bytes (Shape : Geometry) return Count_32 is
     (Count_32'Min ((Shape.Blocks_Per_Group + Bits_Per_Byte - 1) / Bits_Per_Byte,
                    Maximum_Bitmap_Bytes))
     with Pre  => Valid_Blocks (Shape),
          Post => Block_Bitmap_Bytes'Result in 1 .. Maximum_Bitmap_Bytes and then
                  Block_Bitmap_Bytes'Result <= Shape.Block_Bytes;

   function Inode_Bitmap_Bytes (Shape : Geometry) return Count_32 is
     ((Shape.Inodes_Per_Group + Bits_Per_Byte - 1) / Bits_Per_Byte)
     with Pre  => Valid_Inodes (Shape),
          Post => Inode_Bitmap_Bytes'Result in 1 .. Shape.Block_Bytes;

   --  The first inode new files may use (s_first_ino, or 11 in revision 0).
   function First_Usable_Inode (Revision : Unsigned_32; First_Non_Reserved : Count_32)
      return Inode_Number is
     (if Revision >= Dynamic_Revision and then First_Non_Reserved > 0
      then Inode_Number (First_Non_Reserved) else Good_Old_First_Inode);

   pragma Inline_Always (Volume_Bytes, Block_Offset, Inode_Group_Count, Block_Group_Count, Group_Of_Inode, Index_In_Group, Descriptor_Offset, Inode_Offset, In_Volume, Group_Of_Block, Group_First_Block, Bit_Of_Block, Blocks_In_Group, Block_Bitmap_Bytes, Inode_Bitmap_Bytes);

end Ext2_Geometry;

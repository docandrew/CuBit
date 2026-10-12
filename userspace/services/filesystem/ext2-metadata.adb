------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 metadata I/O (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.System_Facts;
with CuBit.Messages; use CuBit.Messages;
with System.Storage_Elements; use System.Storage_Elements;
with Block_Cache_Index;
with Ext2.Block_Cache; use Ext2.Block_Cache;
with Ext2.Journal; use Ext2.Journal;

package body Ext2.Metadata is

   use all type Block_Cache_Index.Block_Class;
   package Geometry renames Ext2_Geometry;

   function Fields_Of (sb : Superblock) return Ext2_Geometry.Superblock_Fields is
     ((Signature        => sb.signature,
       Block_Shift      => sb.blockShift,
       Block_Count      => Count_32 (sb.blockCount),
       First_Data_Block => Count_32 (sb.firstDataBlock),
       Blocks_Per_Group => Count_32 (sb.blocksPerBlockGroup),
       Inode_Count      => Count_32 (sb.inodeCount),
       Inodes_Per_Group => Count_32 (sb.inodesPerBlockGroup),
       Free_Blocks      => Count_32 (sb.freeBlocks),
       Free_Inodes      => Count_32 (sb.freeInodes),
       Revision         => sb.majorVersion,
       Inode_Bytes      => sb.inodeSize));

   function Shape (fs : Filesystem) return Ext2_Geometry.Geometry is
     ((Block_Count      => Count_32 (fs.sb.blockCount),
       First_Data_Block => Count_32 (fs.sb.firstDataBlock),
       Blocks_Per_Group => Count_32 (fs.sb.blocksPerBlockGroup),
       Inode_Count      => Count_32 (fs.sb.inodeCount),
       Inodes_Per_Group => Count_32 (fs.sb.inodesPerBlockGroup),
       Block_Bytes      => Count_32 (fs.blkSize),
       Inode_Bytes      =>
         (if fs.sb.majorVersion >= Dynamic_Revision then Count_32 (fs.sb.inodeSize)
          else Good_Old_Inode_Bytes)));

   procedure readInode
     (fs : Filesystem; inodeNum : Unsigned_32;
      ino : out Inode; status : out Read_Status)
   is
      layout : constant Geometry.Geometry := Shape (fs);
      number : constant Inode_Number := Inode_Number (inodeNum);
      bgd : BlockGroupDescriptor;
      inodeTableByteOffset : Storage_Offset;
   begin
      ino := NULL_INODE;
      status := Read_Out_Of_Range;
      if inodeNum = 0 or else inodeNum > fs.sb.inodeCount then
         return;
      end if;
      readBytes
        (fs, Storage_Offset (Geometry.Descriptor_Offset
                               (layout, Geometry.Group_Of_Inode (layout, number))),
         bgd'Address, Group_Descriptor_Bytes, status);
      if status /= Read_Complete then
         return;
      end if;
      if bgd.inodeTableAddr = 0 or else
        bgd.inodeTableAddr >= fs.sb.blockCount
      then
         status := Read_Out_Of_Range;
         return;
      end if;
      inodeTableByteOffset := Storage_Offset
        (Geometry.Inode_Offset (layout, Block_Number (bgd.inodeTableAddr),
                                Geometry.Index_In_Group (layout, number)));
      if inodeTableByteOffset >
        Storage_Offset (Geometry.Volume_Bytes (layout)) - Inode'Size / 8
      then
         status := Read_Out_Of_Range;
         return;
      end if;
      readBytes
        (fs, inodeTableByteOffset, ino'Address, Inode'Size / 8, status);
      if status /= Read_Complete then
         ino := NULL_INODE;
      end if;
   end readInode;

   function Now_Seconds return Unsigned_32 is
      Offset : constant Unsigned_64 := CuBit.System_Facts.Wall_Clock_Offset;
      Monotonic : Unsigned_64;
      MS_PER_SECOND : constant := 1_000;
   begin
      if Offset = 0 or else Offset = Unsigned_64'Last then
         return 0;
      end if;
      Monotonic := syscall (SYSCALL_GETTIME);
      if Monotonic = Unsigned_64'Last or else Offset > Unsigned_64'Last - Monotonic then
         return 0;
      end if;
      return Unsigned_32 (Unsigned_64'Min ((Offset + Monotonic) / MS_PER_SECOND, Unsigned_64 (Unsigned_32'Last)));
   end Now_Seconds;

   procedure Stamp (ino : in out Inode; touch : Inode_Touch) is
      Now : constant Unsigned_32 := Now_Seconds;
   begin
      if Now = 0 then
         return;
      end if;
      ino.creationTime := Now;   --  ext2's i_ctime: the inode's change time
      if touch in Content_Changed | Inode_Created then
         ino.modifiedTime := Now;
      end if;
      if touch = Inode_Created then
         ino.accessedTime := Now;
      end if;
   end Stamp;

   procedure Stamp (ino : in out Inode; touch : Inode_Touch; changed : out Boolean) is
      before : constant Inode := ino;
   begin
      Stamp (ino, touch);
      changed := ino /= before;
   end Stamp;

   procedure writeInode
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      ino      : Inode;
      status   : out Write_Status;
      mode     : Inode_Write_Mode := Update_Existing_Inode)
   is
      layout : constant Geometry.Geometry := Shape (fs);
      number : constant Inode_Number := Inode_Number (inodeNum);
      bgd : BlockGroupDescriptor;
      inodeTableByteOffset : Storage_Offset;
      inoSize : constant Unsigned_32 := Unsigned_32 (layout.Inode_Bytes);
      readStatus : Read_Status;
   begin
      if number = No_Inode then
         --  No group holds inode 0: as a descriptor beyond the volume.
         status := Write_Out_Of_Range;
         return;
      end if;
      readBytes
        (fs, Storage_Offset (Geometry.Descriptor_Offset
                               (layout, Geometry.Group_Of_Inode (layout, number))),
         bgd'Address, Group_Descriptor_Bytes, readStatus);
      if readStatus /= Read_Complete then
         status :=
           (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
            else Write_Device_Error);
         return;
      end if;

      inodeTableByteOffset := Storage_Offset
        (Geometry.Inode_Offset (layout, Block_Number (bgd.inodeTableAddr),
                                Geometry.Index_In_Group (layout, number)));

      case mode is
         when Update_Existing_Inode | Exact_Inode =>
            declare
               written : Inode := ino;
               current : Inode;
            begin
               if mode = Update_Existing_Inode and then fs.journal.Active and then
                 ino.numHardLinks = 0
               then
                  readBytes (fs, inodeTableByteOffset, current'Address,
                             Inode'Size / 8, readStatus);
                  if readStatus /= Read_Complete then
                     status := Write_Device_Error;
                     return;
                  end if;
                  written.deletedTime := current.deletedTime;
               end if;
               writeBytes
                 (fs, inodeTableByteOffset, written'Address, Inode'Size / 8, status,
                  Inode_Table);
            end;
         when Initialize_New_Inode =>
            declare
               --  Admission bounds the power-of-two inode slot by blkSize.
               slot : String (1 .. Natural (inoSize)) := [others => Character'Val (0)]
                 with Alignment => Unsigned_32'Alignment;
               header : Inode with Import, Address => slot'Address;
            begin
               header := ino;
               writeBytes
                 (fs, inodeTableByteOffset, slot'Address, Storage_Count (inoSize), status,
                  Inode_Table);
            end;
      end case;
   end writeInode;

   procedure writeSuperblock
     (fs : in out Filesystem;
      status : out Write_Status)
   is
   begin
      writeBytes (fs, SUPERBLOCK_OFFSET, fs.sb'Address,
                  Superblock'Size / 8, status, Allocation_Metadata);
   end writeSuperblock;

   procedure writeBGD
     (fs         : in out Filesystem;
      blockGroup : Group_Number;
      bgd        : BlockGroupDescriptor;
      status     : out Write_Status)
   is
   begin
      writeBytes (fs, Storage_Offset (Geometry.Descriptor_Offset (Shape (fs), blockGroup)),
                  bgd'Address, Group_Descriptor_Bytes, status, Allocation_Metadata);
   end writeBGD;

   procedure readBGD
     (fs         : Filesystem;
      blockGroup : Group_Number;
      bgd        : out BlockGroupDescriptor;
      status     : out Read_Status)
   is
   begin
      readBytes (fs, Storage_Offset (Geometry.Descriptor_Offset (Shape (fs), blockGroup)),
                 bgd'Address, Group_Descriptor_Bytes, status);
   end readBGD;

end Ext2.Metadata;

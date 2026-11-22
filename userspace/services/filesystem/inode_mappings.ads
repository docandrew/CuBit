with Interfaces; use Interfaces;
with Ext2_Inodes; use Ext2_Inodes;
with Sector_Accounting;

--  Production value transformation: mapping and allocation count cannot be
--  updated separately. Storage reservation and publication remain in Ext2.
package Inode_Mappings with Pure, SPARK_Mode is
   function Added_Blocks
     (Current : Inode; Logical : Unsigned_32)
      return Sector_Accounting.Attached_Blocks is
     (if Logical >= NUM_DIRECT_BLOCKS and then Current.singleIndirectBlock = 0
      then 2 else 1);

   function Valid_Attachment
     (Current : Inode; Logical, Data_Block, Indirect_Block : Unsigned_32;
      Sectors : Sector_Accounting.Block_Sectors) return Boolean is
     (Data_Block /= 0 and then
      (if Logical < NUM_DIRECT_BLOCKS then
         Current.directBlocks (Natural (Logical)) = 0 and Indirect_Block = 0
       else Logical - NUM_DIRECT_BLOCKS < Sectors * 128 and then
         Indirect_Block /= 0 and then Data_Block /= Indirect_Block and then
         (Current.singleIndirectBlock = 0 or
          Current.singleIndirectBlock = Indirect_Block)));

   function Fits
     (Current : Inode; Logical : Unsigned_32;
      Sectors : Sector_Accounting.Block_Sectors) return Boolean is
     (Unsigned_64 (Current.numDiskSectors) + Unsigned_64 (Sectors) *
        Unsigned_64 (Added_Blocks (Current, Logical)) <=
      Unsigned_64 (Unsigned_32'Last));

   procedure Prepare_Attachment
     (Current : Inode; Logical, Data_Block, Indirect_Block : Unsigned_32;
      Sectors : Sector_Accounting.Block_Sectors;
      Updated : out Inode; Accepted : out Boolean)
   with Post =>
     Accepted = (Valid_Attachment
       (Current, Logical, Data_Block, Indirect_Block, Sectors) and
       Fits (Current, Logical, Sectors)) and then
     (if not Accepted then Updated = Current
      else
        Unsigned_64 (Updated.numDiskSectors) =
          Unsigned_64 (Current.numDiskSectors) + Unsigned_64 (Sectors) *
            Unsigned_64 (Added_Blocks (Current, Logical)) and then
        (if Logical < NUM_DIRECT_BLOCKS then
           Updated = (Current with delta
             directBlocks => (Current.directBlocks with delta
               Natural (Logical) => Data_Block),
             numDiskSectors => Updated.numDiskSectors)
         else
           Updated = (Current with delta
             singleIndirectBlock => Indirect_Block,
             numDiskSectors => Updated.numDiskSectors)));
end Inode_Mappings;

with Interfaces; use Interfaces;
with Ext2_Inodes; use Ext2_Inodes;
with Sector_Accounting;

--  Triple-indirect counterpart of Double_Mappings: the inode's root pointer
--  and allocation count change together. Pointer block contents, storage
--  reservation and publication order remain in Ext2.
package Triple_Mappings with Pure, SPARK_Mode is
   function Added (Current : Inode; New_Middle, New_Leaf : Boolean)
      return Sector_Accounting.Attached_Blocks is
     (1 + (if Current.tripleIndirectBlock = 0 then 1 else 0) +
          (if New_Middle then 1 else 0) + (if New_Leaf then 1 else 0));

   function Fits (Current : Inode; New_Middle, New_Leaf : Boolean;
                  Sectors : Sector_Accounting.Block_Sectors) return Boolean is
     (Unsigned_64 (Current.numDiskSectors) + Unsigned_64 (Sectors) *
        Unsigned_64 (Added (Current, New_Middle, New_Leaf)) <=
      Unsigned_64 (Unsigned_32'Last));

   --  A new root needs a new middle block; a new middle needs a new leaf.
   function Valid (Current : Inode; Root, Middle, Leaf, Data : Unsigned_32;
                   New_Middle, New_Leaf : Boolean) return Boolean is
     (Root /= 0 and Middle /= 0 and Leaf /= 0 and Data /= 0 and
      Root /= Middle and Root /= Leaf and Root /= Data and
      Middle /= Leaf and Middle /= Data and Leaf /= Data and
      (if New_Middle then New_Leaf) and
      (if Current.tripleIndirectBlock = 0 then New_Middle
       else Current.tripleIndirectBlock = Root));

   procedure Prepare
     (Current : Inode; Root, Middle, Leaf, Data : Unsigned_32;
      New_Middle, New_Leaf : Boolean;
      Sectors : Sector_Accounting.Block_Sectors;
      Updated : out Inode; Accepted : out Boolean)
     with Post =>
       Accepted = (Valid (Current, Root, Middle, Leaf, Data, New_Middle, New_Leaf) and
                   Fits (Current, New_Middle, New_Leaf, Sectors)) and then
       (if not Accepted then Updated = Current
        else Unsigned_64 (Updated.numDiskSectors) =
          Unsigned_64 (Current.numDiskSectors) + Unsigned_64 (Sectors) *
            Unsigned_64 (Added (Current, New_Middle, New_Leaf)) and then
          Updated = (Current with delta tripleIndirectBlock => Root,
                     numDiskSectors => Updated.numDiskSectors));
end Triple_Mappings;

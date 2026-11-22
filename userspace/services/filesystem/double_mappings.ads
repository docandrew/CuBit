with Interfaces; use Interfaces;
with Ext2_Inodes; use Ext2_Inodes;
with Sector_Accounting;

package Double_Mappings with Pure, SPARK_Mode is
   function Added (Current : Inode; New_Leaf : Boolean)
      return Sector_Accounting.Attached_Blocks is
     (1 + (if Current.doubleIndirectBlock = 0 then 1 else 0) +
          (if New_Leaf then 1 else 0));

   function Fits (Current : Inode; New_Leaf : Boolean;
                  Sectors : Sector_Accounting.Block_Sectors) return Boolean is
     (Unsigned_64 (Current.numDiskSectors) + Unsigned_64 (Sectors) *
        Unsigned_64 (Added (Current, New_Leaf)) <= Unsigned_64 (Unsigned_32'Last));

   function Valid (Current : Inode; Root, Leaf, Data : Unsigned_32;
                   New_Leaf : Boolean) return Boolean is
     (Root /= 0 and Leaf /= 0 and Data /= 0 and
      Root /= Leaf and Root /= Data and Leaf /= Data and
      (if Current.doubleIndirectBlock = 0 then New_Leaf
       else Current.doubleIndirectBlock = Root));

   procedure Prepare
     (Current : Inode; Root, Leaf, Data : Unsigned_32; New_Leaf : Boolean;
      Sectors : Sector_Accounting.Block_Sectors;
      Updated : out Inode; Accepted : out Boolean)
     with Post =>
       Accepted = (Valid (Current, Root, Leaf, Data, New_Leaf) and
                   Fits (Current, New_Leaf, Sectors)) and then
       (if not Accepted then Updated = Current
        else Unsigned_64 (Updated.numDiskSectors) =
          Unsigned_64 (Current.numDiskSectors) + Unsigned_64 (Sectors) *
            Unsigned_64 (Added (Current, New_Leaf)) and then
          Updated = (Current with delta doubleIndirectBlock => Root,
                     numDiskSectors => Updated.numDiskSectors));
end Double_Mappings;

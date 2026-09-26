with Interfaces; use Interfaces;
with Ext2_Inodes;
with Sector_Accounting;

--  Value-only decoding of standard Ext2 direct/single/double block paths.
--  No disk access, pointers, allocation or claim that a decoded path exists.
package Block_Paths with Pure, SPARK_Mode is
   subtype Direct_Index is Natural range 0 .. Ext2_Inodes.NUM_DIRECT_BLOCKS - 1;
   subtype Pointer_Index is Natural range 0 .. 1023;
   type Path_Kind is (Direct, Single_Indirect, Double_Indirect, Unsupported);
   type Block_Path (Kind : Path_Kind := Unsupported) is record
      case Kind is
         when Direct =>
            Direct_Slot : Direct_Index;
         when Single_Indirect =>
            Single_Slot : Pointer_Index;
         when Double_Indirect =>
            Root_Slot, Leaf_Slot : Pointer_Index;
         when Unsupported => null;
      end case;
   end record;

   function Pointer_Count (Sectors : Sector_Accounting.Block_Sectors)
      return Unsigned_64 is (Unsigned_64 (Sectors) * 128);

   function Block_Limit (Sectors : Sector_Accounting.Block_Sectors)
      return Unsigned_64 is
     (Ext2_Inodes.NUM_DIRECT_BLOCKS + Pointer_Count (Sectors) +
      Pointer_Count (Sectors) * Pointer_Count (Sectors));

   function Matches
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors;
      Path : Block_Path) return Boolean is
     (case Path.Kind is
         when Direct => Logical = Unsigned_64 (Path.Direct_Slot),
         when Single_Indirect =>
           Logical >= Ext2_Inodes.NUM_DIRECT_BLOCKS and then
           Logical < Ext2_Inodes.NUM_DIRECT_BLOCKS + Pointer_Count (Sectors) and then
           Unsigned_64 (Path.Single_Slot) < Pointer_Count (Sectors) and then
           Unsigned_64 (Path.Single_Slot) = Logical - Ext2_Inodes.NUM_DIRECT_BLOCKS,
         when Double_Indirect =>
           Logical >= Ext2_Inodes.NUM_DIRECT_BLOCKS + Pointer_Count (Sectors) and then
           Logical < Block_Limit (Sectors) and then
           Unsigned_64 (Path.Root_Slot) < Pointer_Count (Sectors) and then
           Unsigned_64 (Path.Leaf_Slot) < Pointer_Count (Sectors) and then
           Unsigned_64 (Path.Root_Slot) =
             (Logical - (Ext2_Inodes.NUM_DIRECT_BLOCKS + Pointer_Count (Sectors))) /
               Pointer_Count (Sectors) and then
           Unsigned_64 (Path.Leaf_Slot) =
             (Logical - (Ext2_Inodes.NUM_DIRECT_BLOCKS + Pointer_Count (Sectors))) mod
               Pointer_Count (Sectors),
         when Unsupported => Logical >= Block_Limit (Sectors))
     with Ghost;

   function Decode
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Block_Path
     with Post => Matches (Logical, Sectors, Decode'Result);
end Block_Paths;

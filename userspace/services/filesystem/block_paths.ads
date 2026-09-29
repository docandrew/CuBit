with Interfaces; use Interfaces;
with Ext2_Inodes;
with Sector_Accounting;

--  Value-only decoding of standard Ext2 direct/single/double/triple block
--  paths. No disk access, pointers, allocation or claim that a path exists.
package Block_Paths with Pure, SPARK_Mode is
   --  Four-byte block numbers: 128 per 512-byte sector, 256/512/1024 per block.
   Pointers_Per_Sector : constant := 128;
   Maximum_Pointers : constant := 1024;
   subtype Pointers_Per_Block is Unsigned_64 range 256 .. Maximum_Pointers;

   --  Complete 4 KiB direct/single/double/triple extent. It stays below
   --  2**32, so every admitted logical index is also a valid Unsigned_32.
   Maximum_Logical_Blocks : constant :=
     Ext2_Inodes.NUM_DIRECT_BLOCKS + Maximum_Pointers +
     Maximum_Pointers ** 2 + Maximum_Pointers ** 3;
   subtype Logical_Block_Count is Unsigned_64 range 0 .. Maximum_Logical_Blocks;

   subtype Direct_Index is Natural range 0 .. Ext2_Inodes.NUM_DIRECT_BLOCKS - 1;
   subtype Pointer_Index is Natural range 0 .. Maximum_Pointers - 1;
   type Path_Kind is
     (Direct, Single_Indirect, Double_Indirect, Triple_Indirect, Unsupported);
   type Block_Path (Kind : Path_Kind := Unsupported) is record
      case Kind is
         when Direct =>
            Direct_Slot : Direct_Index;
         when Single_Indirect =>
            Single_Slot : Pointer_Index;
         when Double_Indirect =>
            Root_Slot, Leaf_Slot : Pointer_Index;
         when Triple_Indirect =>
            --  Triple root -> middle block -> leaf block -> data.
            Top_Slot, Middle_Slot, Bottom_Slot : Pointer_Index;
         when Unsupported => null;
      end case;
   end record;

   function Pointer_Count (Sectors : Sector_Accounting.Block_Sectors)
      return Unsigned_64 is (Unsigned_64 (Sectors) * Pointers_Per_Sector)
     with Post => Pointer_Count'Result in Pointers_Per_Block;

   --  The same count in the index domain, for bounds on decoded slots.
   function Slot_Count (Sectors : Sector_Accounting.Block_Sectors)
      return Positive is (Natural (Sectors) * Pointers_Per_Sector)
     with Post => Slot_Count'Result <= Maximum_Pointers;

   --  Logical blocks covered by one middle block (one leaf per slot).
   function Middle_Span (Sectors : Sector_Accounting.Block_Sectors)
      return Unsigned_64 is (Pointer_Count (Sectors) * Pointer_Count (Sectors))
     with Post => Middle_Span'Result <= Maximum_Pointers ** 2;

   function First_Double (Sectors : Sector_Accounting.Block_Sectors)
      return Logical_Block_Count is
     (Ext2_Inodes.NUM_DIRECT_BLOCKS + Pointer_Count (Sectors));

   function First_Triple (Sectors : Sector_Accounting.Block_Sectors)
      return Logical_Block_Count is
     (First_Double (Sectors) + Middle_Span (Sectors));

   function Block_Limit (Sectors : Sector_Accounting.Block_Sectors)
      return Logical_Block_Count is
     (First_Triple (Sectors) + Middle_Span (Sectors) * Pointer_Count (Sectors));

   function Matches
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors;
      Path : Block_Path) return Boolean is
     (case Path.Kind is
         when Direct => Logical = Unsigned_64 (Path.Direct_Slot),
         when Single_Indirect =>
           Logical >= Ext2_Inodes.NUM_DIRECT_BLOCKS and then
           Logical < First_Double (Sectors) and then
           Path.Single_Slot < Slot_Count (Sectors) and then
           Unsigned_64 (Path.Single_Slot) = Logical - Ext2_Inodes.NUM_DIRECT_BLOCKS,
         when Double_Indirect =>
           Logical >= First_Double (Sectors) and then
           Logical < First_Triple (Sectors) and then
           Path.Root_Slot < Slot_Count (Sectors) and then
           Path.Leaf_Slot < Slot_Count (Sectors) and then
           Unsigned_64 (Path.Root_Slot) =
             (Logical - First_Double (Sectors)) / Pointer_Count (Sectors) and then
           Unsigned_64 (Path.Leaf_Slot) =
             (Logical - First_Double (Sectors)) mod Pointer_Count (Sectors),
         when Triple_Indirect =>
           Logical >= First_Triple (Sectors) and then
           Logical < Block_Limit (Sectors) and then
           Path.Top_Slot < Slot_Count (Sectors) and then
           Path.Middle_Slot < Slot_Count (Sectors) and then
           Path.Bottom_Slot < Slot_Count (Sectors) and then
           Unsigned_64 (Path.Top_Slot) =
             (Logical - First_Triple (Sectors)) / Middle_Span (Sectors) and then
           Unsigned_64 (Path.Middle_Slot) =
             (Logical - First_Triple (Sectors)) / Pointer_Count (Sectors) mod
               Pointer_Count (Sectors) and then
           Unsigned_64 (Path.Bottom_Slot) =
             (Logical - First_Triple (Sectors)) mod Pointer_Count (Sectors),
         when Unsupported => Logical >= Block_Limit (Sectors))
     with Ghost;

   function Decode
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Block_Path
     with Post => Matches (Logical, Sectors, Decode'Result);
end Block_Paths;

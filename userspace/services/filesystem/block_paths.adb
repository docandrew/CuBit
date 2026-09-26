package body Block_Paths with SPARK_Mode is
   function Decode
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Block_Path
   is
      Pointers : constant Unsigned_64 := Pointer_Count (Sectors);
      First_Double : constant Unsigned_64 :=
        Ext2_Inodes.NUM_DIRECT_BLOCKS + Pointers;
   begin
      if Logical < Ext2_Inodes.NUM_DIRECT_BLOCKS then
         return (Kind => Direct, Direct_Slot => Direct_Index (Logical));
      elsif Logical < First_Double then
         return (Kind => Single_Indirect,
                  Single_Slot => Pointer_Index (Logical - Ext2_Inodes.NUM_DIRECT_BLOCKS));
      elsif Logical < Block_Limit (Sectors) then
         declare
            Within_Double : constant Unsigned_64 := Logical - First_Double;
         begin
            return (Kind => Double_Indirect,
                     Root_Slot => Pointer_Index (Within_Double / Pointers),
                     Leaf_Slot => Pointer_Index (Within_Double mod Pointers));
         end;
      else
         return (Kind => Unsupported);
      end if;
   end Decode;
end Block_Paths;

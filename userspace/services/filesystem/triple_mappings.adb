package body Triple_Mappings with SPARK_Mode is
   procedure Prepare
     (Current : Inode; Root, Middle, Leaf, Data : Unsigned_32;
      New_Middle, New_Leaf : Boolean;
      Sectors : Sector_Accounting.Block_Sectors;
      Updated : out Inode; Accepted : out Boolean)
   is
      Count : Unsigned_32;
      Count_Fits : Boolean;
   begin
      Updated := Current;
      Accepted := False;
      if not Valid (Current, Root, Middle, Leaf, Data, New_Middle, New_Leaf) then
         return;
      end if;
      Sector_Accounting.Plan_Addition
        (Current.numDiskSectors, Sectors, Added (Current, New_Middle, New_Leaf),
         Count, Count_Fits);
      if not Count_Fits then return; end if;
      Updated.tripleIndirectBlock := Root;
      Updated.numDiskSectors := Count;
      Accepted := True;
   end Prepare;
end Triple_Mappings;

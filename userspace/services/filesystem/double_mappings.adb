package body Double_Mappings with SPARK_Mode is
   procedure Prepare
     (Current : Inode; Root, Leaf, Data : Unsigned_32; New_Leaf : Boolean;
      Sectors : Sector_Accounting.Block_Sectors;
      Updated : out Inode; Accepted : out Boolean)
   is
      Count : Unsigned_32;
      Count_Fits : Boolean;
   begin
      Updated := Current;
      Accepted := False;
      if not Valid (Current, Root, Leaf, Data, New_Leaf) then return; end if;
      Sector_Accounting.Plan_Addition
        (Current.numDiskSectors, Sectors, Added (Current, New_Leaf), Count, Count_Fits);
      if not Count_Fits then return; end if;
      Updated.doubleIndirectBlock := Root;
      Updated.numDiskSectors := Count;
      Accepted := True;
   end Prepare;
end Double_Mappings;

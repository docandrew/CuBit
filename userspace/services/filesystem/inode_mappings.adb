package body Inode_Mappings with SPARK_Mode is
   procedure Prepare_Attachment
     (Current : Inode; Logical, Data_Block, Indirect_Block : Unsigned_32;
      Sectors : Sector_Accounting.Block_Sectors;
      Updated : out Inode; Accepted : out Boolean)
   is
      Count : Unsigned_32;
      Count_Fits : Boolean;
   begin
      Updated := Current;
      Accepted := False;
      if not Valid_Attachment
        (Current, Logical, Data_Block, Indirect_Block, Sectors)
      then
         return;
      end if;
      Sector_Accounting.Plan_Addition
        (Current.numDiskSectors, Sectors, Added_Blocks (Current, Logical),
         Count, Count_Fits);
      if not Count_Fits then
         return;
      end if;
      if Logical < NUM_DIRECT_BLOCKS then
         Updated.directBlocks (Natural (Logical)) := Data_Block;
      else
         Updated.singleIndirectBlock := Indirect_Block;
      end if;
      Updated.numDiskSectors := Count;
      Accepted := True;
   end Prepare_Attachment;
end Inode_Mappings;

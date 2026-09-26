package body Ext2_Support with SPARK_Mode is
   function Check_File (Item : Ext2_Inodes.Inode) return File_Admission is
   begin
      if (Item.typeAndPermissions and 16#F000#) /= 16#8000# then
         return Not_A_Regular_File;
      elsif Item.numHardLinks /= 1 then
         return Not_A_Single_Link;
      elsif Item.deletedTime /= 0 or else Item.flags /= 0 or else
        Item.fragmentBlockAddr /= 0
      then
         return Unsupported_Metadata;
      end if;
      return File_Allowed;
   end Check_File;
end Ext2_Support;

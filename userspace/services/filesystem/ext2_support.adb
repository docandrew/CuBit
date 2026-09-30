package body Ext2_Support with SPARK_Mode is
   function Check_File
     (Item : Ext2_Inodes.Inode; Unlinked_Allowed : Boolean := False)
      return File_Admission is
   begin
      if (Item.typeAndPermissions and 16#F000#) /= 16#8000# then
         return Not_A_Regular_File;
      elsif Item.numHardLinks /= 1 and then
        not (Unlinked_Allowed and then Item.numHardLinks = 0)
      then
         return Not_A_Single_Link;
      elsif (Item.deletedTime /= 0 and then
             not (Unlinked_Allowed and then Item.numHardLinks = 0)) or else
        Item.flags /= 0 or else
        Item.fragmentBlockAddr /= 0
      then
         return Unsupported_Metadata;
      end if;
      return File_Allowed;
   end Check_File;
end Ext2_Support;

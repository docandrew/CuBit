package body Ext2_Support with SPARK_Mode is
   function Check_File
     (Item : Ext2_Inodes.Inode; Unlinked_Allowed : Boolean := False)
      return File_Admission is
   begin
      if (Item.typeAndPermissions and 16#F000#) /= 16#8000# then
         return Not_A_Regular_File;
      elsif Item.numHardLinks not in Named_Links and then
        not (Unlinked_Allowed and then Item.numHardLinks = 0)
      then
         return Link_Count_Unsupported;
      elsif (Item.deletedTime /= 0 and then
             not (Unlinked_Allowed and then Item.numHardLinks = 0)) or else
        Item.flags /= 0 or else
        Item.fragmentBlockAddr /= 0
      then
         return Unsupported_Metadata;
      end if;
      return File_Allowed;
   end Check_File;

   function Removal_Of (Links : Named_Links; Held : Boolean) return Name_Removal is
     (if Links > 1 then Other_Names_Remain
      elsif Held then Freed_At_Last_Close
      else Freed_Now);
end Ext2_Support;

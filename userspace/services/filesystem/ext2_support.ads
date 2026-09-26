with Interfaces; use Interfaces;
with Ext2_Inodes;

--  Format support is not authority. These gates reject structures the driver
--  cannot safely interpret, after the service checks the caller's authority.
package Ext2_Support with Pure, SPARK_Mode is
   Compat_Extended_Attributes : constant Unsigned_32 := 16#0008#;
   Compat_Resize_Inode        : constant Unsigned_32 := 16#0010#;
   Compat_Directory_Index     : constant Unsigned_32 := 16#0020#;
   Incompat_Directory_Types   : constant Unsigned_32 := 16#0002#;
   Read_Only_Sparse_Super     : constant Unsigned_32 := 16#0001#;
   Read_Only_Large_File       : constant Unsigned_32 := 16#0002#;

   Supported_Compatible : constant Unsigned_32 :=
     Compat_Extended_Attributes or Compat_Resize_Inode or Compat_Directory_Index;
   Supported_Read_Only : constant Unsigned_32 :=
     Read_Only_Sparse_Super or Read_Only_Large_File;

   --  Conservative initial profile: Linux revision 1, typed directory records,
   --  no journal/recovery/extents/checksums or other unimplemented features.
   --  Unknown RO_COMPAT features reject admission rather than silently enabling
   --  writes; a separate explicit read-only admission mode can be added later.
   function Supported_Volume
     (Revision, Creator_OS, Compatible, Incompatible, Read_Only : Unsigned_32)
      return Boolean is
     (Revision = 1 and Creator_OS = 0 and
      (Compatible and not Supported_Compatible) = 0 and
      Incompatible = Incompat_Directory_Types and
      (Read_Only and not Supported_Read_Only) = 0);

   type File_Admission is
     (File_Allowed, Not_A_Regular_File, Not_A_Single_Link, Unsupported_Metadata);

   function Admits_Ordinary_File (Item : Ext2_Inodes.Inode) return Boolean
     with Ghost;

   function Check_File (Item : Ext2_Inodes.Inode) return File_Admission
     with Post => (Check_File'Result = File_Allowed) = Admits_Ordinary_File (Item);

private
   function Admits_Ordinary_File (Item : Ext2_Inodes.Inode) return Boolean is
     ((Item.typeAndPermissions and 16#F000#) = 16#8000# and
      Item.numHardLinks = 1 and Item.deletedTime = 0 and
      Item.flags = 0 and Item.fragmentBlockAddr = 0);
end Ext2_Support;

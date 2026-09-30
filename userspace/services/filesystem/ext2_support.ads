with Interfaces; use Interfaces;
with Ext2_Inodes;

--  Format support is not authority. These gates reject structures the driver
--  cannot safely interpret, after the service checks the caller's authority.
package Ext2_Support with Pure, SPARK_Mode is
   Compat_Has_Journal         : constant Unsigned_32 := 16#0004#;
   Compat_Extended_Attributes : constant Unsigned_32 := 16#0008#;
   Compat_Resize_Inode        : constant Unsigned_32 := 16#0010#;
   Compat_Directory_Index     : constant Unsigned_32 := 16#0020#;
   Incompat_Directory_Types   : constant Unsigned_32 := 16#0002#;
   --  The internal journal holds committed transactions not yet replayed.
   Incompat_Recover           : constant Unsigned_32 := 16#0004#;
   Read_Only_Sparse_Super     : constant Unsigned_32 := 16#0001#;
   Read_Only_Large_File       : constant Unsigned_32 := 16#0002#;

   --  Without LARGE_FILE a regular file's size must fit a signed 32-bit value.
   Small_File_Size_Limit : constant Unsigned_64 := 16#7FFF_FFFF#;

   function Size_Admitted (Size : Unsigned_64; Read_Only : Unsigned_32)
      return Boolean is
     (Size <= Small_File_Size_Limit or else
      (Read_Only and Read_Only_Large_File) /= 0);

   Supported_Compatible : constant Unsigned_32 :=
     Compat_Has_Journal or Compat_Extended_Attributes or Compat_Resize_Inode or
     Compat_Directory_Index;
   Supported_Read_Only : constant Unsigned_32 :=
     Read_Only_Sparse_Super or Read_Only_Large_File;

   --  Conservative profile: Linux revision 1, typed directory records, and
   --  an internal JBD2 journal (ext3), whose pending transactions the service
   --  replays before use. No extents/checksums or other unimplemented
   --  features. Unknown RO_COMPAT features reject admission rather than
   --  silently enabling writes.
   function Supported_Volume
     (Revision, Creator_OS, Compatible, Incompatible, Read_Only : Unsigned_32)
      return Boolean is
     (Revision = 1 and Creator_OS = 0 and
      (Compatible and not Supported_Compatible) = 0 and
      (Incompatible = Incompat_Directory_Types or else
       (Incompatible = (Incompat_Directory_Types or Incompat_Recover) and
        (Compatible and Compat_Has_Journal) /= 0)) and
      (Read_Only and not Supported_Read_Only) = 0);

   type File_Admission is
     (File_Allowed, Not_A_Regular_File, Not_A_Single_Link, Unsupported_Metadata);

   function Admits_Ordinary_File
     (Item : Ext2_Inodes.Inode; Unlinked_Allowed : Boolean) return Boolean
     with Ghost;

   --  Opening requires exactly one link. A file unlinked while open (zero
   --  links; the service frees it at last close) stays usable through its
   --  handles: those paths pass Unlinked_Allowed.
   function Check_File
     (Item : Ext2_Inodes.Inode; Unlinked_Allowed : Boolean := False)
      return File_Admission
     with Post => (Check_File'Result = File_Allowed) =
                    Admits_Ordinary_File (Item, Unlinked_Allowed);

private
   function Admits_Ordinary_File
     (Item : Ext2_Inodes.Inode; Unlinked_Allowed : Boolean) return Boolean is
     ((Item.typeAndPermissions and 16#F000#) = 16#8000# and
      (Item.numHardLinks = 1 or (Unlinked_Allowed and Item.numHardLinks = 0)) and
      --  An unlinked inode's dtime links the ext3 orphan list.
      (Item.deletedTime = 0 or (Unlinked_Allowed and Item.numHardLinks = 0)) and
      Item.flags = 0 and Item.fragmentBlockAddr = 0);
end Ext2_Support;

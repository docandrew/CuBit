with Interfaces; use Interfaces;

--  Shared on-disk value types, independent of storage transport/address overlays.
package Ext2_Inodes with Pure, SPARK_Mode is
   NUM_DIRECT_BLOCKS : constant := 12;
   type DirectBlockArray is array (0 .. NUM_DIRECT_BLOCKS - 1) of Unsigned_32
     with Convention => C;

   type Inode is record
      typeAndPermissions   : Unsigned_16;
      uid                  : Unsigned_16;
      sizeLo               : Unsigned_32;
      accessedTime         : Unsigned_32;
      creationTime         : Unsigned_32;
      modifiedTime         : Unsigned_32;
      deletedTime          : Unsigned_32;
      gid                  : Unsigned_16;
      numHardLinks         : Unsigned_16;
      numDiskSectors       : Unsigned_32;
      flags                : Unsigned_32;
      osSpecific1          : Unsigned_32;
      directBlocks         : DirectBlockArray;
      singleIndirectBlock  : Unsigned_32;
      doubleIndirectBlock  : Unsigned_32;
      tripleIndirectBlock  : Unsigned_32;
      generationNumber     : Unsigned_32;
      fileACL              : Unsigned_32;
      sizeHi_DirACL        : Unsigned_32;
      fragmentBlockAddr    : Unsigned_32;
      osSpecific2A         : Unsigned_32;
      osSpecific2B         : Unsigned_32;
      osSpecific2C         : Unsigned_32;
   end record with Convention => C;
   pragma Compile_Time_Error (Inode'Size /= 128 * 8,
                              "Ext2 inode layout must remain 128 bytes");
end Ext2_Inodes;

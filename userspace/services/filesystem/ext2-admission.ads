------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Volume admission: the block-device description, the superblock
--  (Ext2_Geometry.Supported), JBD2 replay and the journal opened for the
--  session, orphan processing; and Detach, the clean end of a session.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Format;
with Ext2_Geometry;
with Ext2.Metadata;

private package Ext2.Admission is

   use Volume_Admission;

   --  Superblock journal fields beyond the prefix record (little-endian).
   Journal_Inode_Offset  : constant := Ext2_Format.Journal_Inode_Offset;

   Journal_Device_Offset : constant := Ext2_Format.Journal_Device_Offset;

   procedure Detach (fs : in out Filesystem; status : out Flush_Status);

   procedure initBlockDevice
     (fs         : out Filesystem;
      endpoint   : CuBit.Handles.Endpoint;
      volume     : Unsigned_64;
      grant      : CuBit.Memory_Grants.Grant_Reference;
      grantBuf   : System.Address;
      grantBytes : Unsigned_32;
      result     : out Admission_Result)
     --  The geometry every location function relies on (Ext2_Geometry).
     with Post => (if result = Admitted then Ext2_Geometry.Valid (Ext2.Metadata.Shape (fs)));

end Ext2.Admission;

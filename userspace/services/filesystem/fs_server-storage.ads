------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Volumes of the filesystem service: admission, startup volumes, descriptions, the periodic commit.
------------------------------------------------------------------------------
private package Fs_Server.Storage is

   --  Generic endpoint admission. The registry role is only a boot-order hint;
   --  grant creation is authorized by the endpoint already installed by devmgr.
   procedure ensureVolume (Volume : Volume_Index; Result : out Admission_Result);

   --  Startup: the boot image (a CPIO archive) devmgr mapped for the
   --  service, and the writable memory volume in it, if any.
   procedure mountBootImage (address, size : Unsigned_64);

   --  Startup: the bootstrap block-volume bindings. File operations never
   --  test these driver roles. False if the list could not be made.
   procedure registerVolumes (ok : out Boolean);

   --  Queue_Describe_Volume (step 7): the volume of a path the caller may
   --  read, from the service's in-memory superblock (with the blocks freed
   --  by the running transaction, which are free once it commits).
   procedure handleDescribeVolume (owner : Process_ID; pathLength, rangeBytes : Unsigned_64);

   --  The periodic commit (Linux's commit=5): clients' buffered writes are
   --  harvested, and each volume's journal commits its dirty blocks.
   procedure commitTick;

end Fs_Server.Storage;

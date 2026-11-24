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

   --  devmgr attaches a block driver that started after the filesystem:
   --  its endpoint, offered with the call (Fs_Attachments).
   procedure handleAttach (sender : Process_ID; msg : Message);

   --  devmgr detaches a block device that went away (Fs_Attachments
   --  Detach_Label, FS-034): its handles are closed (REPLY_VOLUME_GONE,
   --  Volume_Removed), its watches end, its caches and versions are
   --  forgotten, its endpoint is closed; the name waits for the next attach.
   procedure handleDetach (sender : Process_ID; msg : Message);

   --  Queue_Eject (FS-034, CuBit.Volume_Ejection): the volume locator in the
   --  arena (pathLength bytes of rangeBytes), the open-handle policy in
   --  options. Safe_To_Remove once every buffered write is written, the
   --  volume committed and marked clean, and detached as handleDetach does
   --  (Volume_Ejected); otherwise the typed reason. Needs write access to
   --  the whole volume.
   procedure handleEject (owner : Process_ID; pathLength, rangeBytes : Unsigned_64; options : Unsigned_32);

   --  Queue_Describe_Volume (step 7): the volume of a path the caller may
   --  read, from the service's in-memory superblock (with the blocks freed
   --  by the running transaction, which are free once it commits).
   procedure handleDescribeVolume (owner : Process_ID; pathLength, rangeBytes : Unsigned_64);

   --  Queue_List_Places: the caller's scopes whose volume is present now
   --  (attached and admitted, the boot archive, a mounted optical disc),
   --  selected by CuBit.Filesystem_Places into the arena range.
   procedure handleListPlaces (owner : Process_ID; rangeBytes : Unsigned_64);

   --  The periodic commit (Linux's commit=5): clients' buffered writes are
   --  harvested, and each volume's journal commits its dirty blocks.
   procedure commitTick;

   --  The root stop's storage stage (PWR-003, CuBit.System_Stops.
   --  Storage_Operation), only from procmgr: every client's buffered
   --  writes, then each admitted writable volume committed and detached (an
   --  ext3 journal emptied, needs_recovery cleared), and its writes refused
   --  from then on. Answered when done.
   procedure handleStorageStop (sender : Process_ID; msg : Message);

   --  The service's Stop (CuBit.Messages.On_Stop): the same commit and
   --  detach for every volume the storage stage has not detached already
   --  (none written twice), before the process ends.
   procedure commitOnStop;

end Fs_Server.Storage;

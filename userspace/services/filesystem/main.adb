------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace Ext2 filesystem server.
--
--  Serves file operations from a ramdisk Ext2 image via IPC.
--  Receives OP_OPEN/OP_READ/OP_SEEK/OP_CLOSE messages from clients
--  and replies with file data.
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Filesystem_Queues;
with CuBit.Busy_Poll;
with CuBit.Grant_References;
with CuBit.Directory_Paths;
with CuBit.File_Access;
with Cpio;
with Ext2;
with Ext2_Support;
with ISO_Records;
with ISO9660;
with Shared_Objects;
with Volume_List; use Volume_List;
with Volume_Admission; use Volume_Admission;

procedure main is
   use ASCII;
   use type Ext2.Read_Status;
   use type Ext2.Write_Status;
   use type Ext2.Truncate_Status;
   use type Ext2.Directory_Lookup_Status;
   use type Ext2.Remove_Status;

   --  Sysinfo query for ramdisk address
   --  (uses SYSINFO_RAMDISK_ADDRESS from CuBit.Messages)

   --  Maximum open files and path length
   MAX_OPEN_FILES : constant := 32;

   --  Filesystem format is independent of the block provider.
   type Filesystem_Kind is (CPIO_ARCHIVE, ISO_FILESYSTEM, EXT2_FILESYSTEM);

   type Inode_Identity is record
      volume : Volume_Index;
      number : Unsigned_32;
   end record;
   package Open_Inodes is new Shared_Objects
     (Capacity => MAX_OPEN_FILES, Object_Key => Inode_Identity,
      Empty_Key => (Volume_Index'First, 0), Object_Value => Ext2.Inode,
      Empty_Value => Ext2.NULL_INODE);
   use type Open_Inodes.Attach_Result;
   inodeObjects : Open_Inodes.State;

   type Open_Object_Kind is (FILE_OBJECT, DIRECTORY_OBJECT);

   --  File handles retain format, volume identity, ownership and rights.
   type FileEntry is record
      active      : Boolean      := False;
      retired     : Boolean      := False;
      generation  : Unsigned_32  := 1;
      filesystemKind     : Filesystem_Kind  := CPIO_ARCHIVE;
      volume      : Volume_Index := Volume_Index'First;
      inodeNum    : Unsigned_32  := 0;      --  ext2 only
      directoryInode : Ext2.Inode; -- Directory enumeration snapshot only.
      opticalFile : ISO_Records.File_Record;
      cpioFileIdx : Natural      := 0;      --  cpio only
      offset      : Unsigned_64  := 0;
      ownerPID    : ProcessID    := NO_PROCESS;
      openRights  : Unsigned_8   := 0;
      objectKind  : Open_Object_Kind := FILE_OBJECT;
      directoryPath : CuBit.Directory_Paths.Path;
   end record;

   type FileTable is array (0 .. MAX_OPEN_FILES - 1) of FileEntry;
   files : FileTable;

   --  CPIO ramdisk archive
   cpioArchive : Cpio.Archive;
   cpioOk      : Boolean := False;

   --  Append-only identities and per-volume storage sessions. No slot reuse
   --  while handles or cached metadata can refer to an entry.
   Volumes : Volume_List.State;
   type Volume_Context is record
      Status : Admission_Result := Provider_Not_Ready;
      Fs : Ext2.Filesystem;
      Buffer : System.Address := System.Null_Address;
      Grant : CuBit.Memory_Grants.Grant_Reference;
   end record;
   Contexts : array (Volume_Index) of Volume_Context;
   Default_Write_Volume : Volume_Reference := No_Volume;

   ---------------------------------------------------------------------------
   --  Per-process file ACL infrastructure
   ---------------------------------------------------------------------------

   ACL_READ   : constant Unsigned_8 := 1;
   ACL_WRITE  : constant Unsigned_8 := 2;
   ACL_CREATE : constant Unsigned_8 := 8;

   MAX_ACL_ENTRIES : constant := CuBit.File_Access.Maximum_Entries;
   MAX_ACL_PROFILES : constant := 32;

   type ACLProfile is record
      pid    : ProcessID := NO_PROCESS;
      active : Boolean   := False;
      policy : CuBit.File_Access.Policy;
   end record;

   aclProfiles : array (0 .. MAX_ACL_PROFILES - 1) of ACLProfile;

   --  Administrative identity comes only from the kernel's authenticated
   --  service registry. Query it for each rare policy operation so a cached
   --  raw PID cannot become authority after process death and PID reuse.
   function isAdmin (sender : ProcessID) return Boolean is
      devmgrAdmin : constant Unsigned_64 :=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DEVMGR);
      procmgrAdmin : constant Unsigned_64 :=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_PROCMGR);
   begin
      return
        (devmgrAdmin /= 0 and then devmgrAdmin /= Unsigned_64'Last and then
         sender = devmgrAdmin) or else
        (procmgrAdmin /= 0 and then procmgrAdmin /= Unsigned_64'Last and then
         sender = procmgrAdmin);
   end isAdmin;

   --  Check if sender has access rights for the given path.  A scope prefix
   --  must end at a path-component boundary: authority for "apps/foo" must
   --  not also authorize "apps/foobar".
   function checkAccess
     (sender : ProcessID;
      path : String;
      rights : Unsigned_8) return Boolean
   is
   begin
      for profile of aclProfiles loop
         if profile.active and then profile.pid = sender then
            return CuBit.File_Access.Allows
              (profile.policy, path, CuBit.File_Access.Rights_From_Wire (rights));
         end if;
      end loop;
      return False;
   end checkAccess;

   --  Conversion helpers
   function toAddr is new Ada.Unchecked_Conversion
     (Unsigned_64, System.Address);

   --  File handles are opaque service objects.  The low word contains the
   --  one-based table slot and the high word contains its generation.  A
   --  closed handle therefore cannot become valid merely because its slot is
   --  reused for a different file.
   procedure allocHandle
     (handle  : out Unsigned_64;
      slot    : out Integer;
      success : out Boolean)
   is
   begin
      for i in files'Range loop
         if not files (i).active and then not files (i).retired then
            slot := i;
            handle := Shift_Left (Unsigned_64 (files (i).generation), 32) or
              Unsigned_64 (i + 1);
            success := True;
            return;
         end if;
      end loop;

      handle := 0;
      slot := -1;
      success := False;
   end allocHandle;

   function resolveHandle
     (handle : Unsigned_64;
      sender : ProcessID;
      expectedKind : Open_Object_Kind) return Integer
   is
      slotCode : constant Unsigned_64 := handle and 16#FFFF_FFFF#;
      generation : constant Unsigned_32 :=
        Unsigned_32 (Shift_Right (handle, 32));
      slot : Integer;
   begin
      if slotCode = 0 or else slotCode > Unsigned_64 (MAX_OPEN_FILES) then
         return -1;
      end if;

      slot := Integer (slotCode - 1);
      if not files (slot).active or else
         files (slot).retired or else
         files (slot).generation /= generation or else
         files (slot).ownerPID /= sender or else
         files (slot).objectKind /= expectedKind
      then
         return -1;
      end if;

      return slot;
   end resolveHandle;

   ---------------------------------------------------------------------------
   --  [filesystem-journal agent] Files unlinked while handles held them
   --  (handleUnlink): freed when the last handle goes (POSIX). Each needs a
   --  live handle, so MAX_OPEN_FILES entries always suffice.
   ---------------------------------------------------------------------------
   type Inode_Key_Array is array (0 .. MAX_OPEN_FILES - 1) of Inode_Identity;
   NO_ORPHAN : constant Inode_Identity := (Volume_Index'First, 0);
   orphans : Inode_Key_Array := [others => NO_ORPHAN];

   procedure reclaimOrphan (key : Inode_Identity) is
      status : Ext2.Remove_Status;
   begin
      for slot in files'Range loop
         if files (slot).active and then files (slot).objectKind = FILE_OBJECT and then
           files (slot).filesystemKind = EXT2_FILESYSTEM and then
           files (slot).volume = key.volume and then files (slot).inodeNum = key.number
         then
            return; --  still held
         end if;
      end loop;
      for orphan of orphans loop
         if orphan.number /= 0 and then orphan = key then
            orphan := NO_ORPHAN;
            Ext2.reclaimInode (Contexts (key.volume).Fs, key.number, status);
            if status /= Ext2.Remove_Complete then
               debugPrint ("FS Server: unlinked inode not reclaimed: " &
                           Ext2.Remove_Status'Image (status) & LF);
            end if;
         end if;
      end loop;
   end reclaimOrphan;

   procedure releaseHandle (slot : Integer) is
      --  [filesystem-journal agent] last-close reclaim of an unlinked file.
      heldFile : constant Boolean :=
        files (slot).active and then files (slot).objectKind = FILE_OBJECT and then
        files (slot).filesystemKind = EXT2_FILESYSTEM;
      key : constant Inode_Identity := (files (slot).volume, files (slot).inodeNum);
   begin
      Open_Inodes.Detach (inodeObjects, Open_Inodes.Owner_Index (slot));
      files (slot).active := False;
      files (slot).ownerPID := NO_PROCESS;
      files (slot).openRights := 0;
      files (slot).offset := 0;
      files (slot).objectKind := FILE_OBJECT;

      --  Never wrap a generation: retire the slot instead.  This makes stale
      --  handle rejection unconditional rather than merely probabilistic.
      if files (slot).generation = Unsigned_32'Last then
         files (slot).retired := True;
      else
         files (slot).generation := files (slot).generation + 1;
      end if;
      if heldFile then
         reclaimOrphan (key);
      end if;
   end releaseHandle;

   procedure releaseHandlesForOwner (owner : ProcessID) is
   begin
      for slot in files'Range loop
         if files (slot).active and then files (slot).ownerPID = owner then
            releaseHandle (slot);
         end if;
      end loop;
   end releaseHandlesForOwner;

   --  Decode an untrusted wire reference only after checking that both fields
   --  fit the strongly typed userspace representation.  The kernel then
   --  authenticates the owner, current grantee, generation, access, and full
   --  byte range before returning an address.
   ---------------------------------------------------------------------------
   --  Client request queues (CuBit.Filesystem_Queues,
   --  docs/filesystem-data-plane.md): a client lends a queue pair and a
   --  transfer arena once; each request is then an entry, handled by the
   --  same handler as its message twin, with its data in the arena and its
   --  answer on the queue.
   ---------------------------------------------------------------------------
   package FQ renames CuBit.Filesystem_Queues;
   package FQueues renames CuBit.Filesystem_Queues.Queues;
   use type FQueues.Submissions.Index;
   MAX_CLIENT_QUEUES : constant := 16;
   subtype Client_Queue_Count is Natural range 0 .. MAX_CLIENT_QUEUES;
   subtype Client_Queue_Index is Client_Queue_Count range 1 .. MAX_CLIENT_QUEUES;
   No_Client_Queue : constant Client_Queue_Count := 0;
   --  Saved reply capabilities for deferred WAITs, one per queue.
   WAIT_REPLY_SLOT_BASE : constant := 40;

   type Client_Queue is record
      owner      : ProcessID := NO_PROCESS;
      queueGrant : CuBit.Memory_Grants.Grant_Reference;
      arenaGrant : CuBit.Memory_Grants.Grant_Reference;
      base       : System.Address := System.Null_Address;
      arena      : System.Address := System.Null_Address;
      arenaBytes : Unsigned_64 := 0;
      server     : FQueues.Server;
      waiting    : Boolean := False;   --  a WAIT's reply is saved
   end record;
   clientQueues : array (Client_Queue_Index) of Client_Queue;
   clientQueueEpoch : Unsigned_32 := 0;

   --  While a queue entry is handled: where its answer goes, and the arena
   --  range it names (checked against the arena before it is set).
   type Reply_Route is record
      queue : Client_Queue_Count := No_Client_Queue;
      token : FQueues.Token := 0;
   end record;
   curRoute : Reply_Route;
   curArena : System.Address := System.Null_Address;
   curArenaBytes : Unsigned_64 := 0;

   function queueWord (q : Client_Queue_Index; offset : Natural) return System.Address is
     (clientQueues (q).base + Storage_Offset (offset));

   ---------------------------------------------------------------------------
   --  Read delegations (FQ.Delegations_At): a handle whose client may answer
   --  reads from its own cache while no other handle can write the file.
   --  Every change to a file revokes the other handles' delegations first
   --  and moves the file's version on.
   ---------------------------------------------------------------------------
   pragma Compile_Time_Error
     (MAX_OPEN_FILES > FQ.Maximum_Delegations,
      "every handle slot needs a delegation entry");
   subtype Handle_Slot is Natural range 0 .. MAX_OPEN_FILES - 1;
   delegated : array (Handle_Slot) of Boolean := [others => False];

   --  Versions of files seen during this service's life. An entry reused
   --  for another file gives the old one a fresh version when it is seen
   --  again, so no client keeps pages of an older version.
   MAX_INODE_VERSIONS : constant := 256;
   subtype Version_Slot is Natural range 0 .. MAX_INODE_VERSIONS - 1;
   type Inode_Version is record
      used    : Boolean := False;
      key     : Inode_Identity := (Volume_Index'First, 0);
      version : Unsigned_64 := 0;
   end record;
   inodeVersions : array (Version_Slot) of Inode_Version;
   versionClock : Unsigned_64 := 0;
   nextVersionSlot : Version_Slot := Version_Slot'First;

   function versionSlotOf (key : Inode_Identity) return Version_Slot is
      fresh : constant Version_Slot := nextVersionSlot;
   begin
      for i in inodeVersions'Range loop
         if inodeVersions (i).used and then inodeVersions (i).key = key then
            return i;
         end if;
      end loop;
      nextVersionSlot := (if nextVersionSlot = Version_Slot'Last then Version_Slot'First
                          else nextVersionSlot + 1);
      versionClock := versionClock + 1;
      inodeVersions (fresh) := (used => True, key => key, version => versionClock);
      return fresh;
   end versionSlotOf;

   procedure bumpVersion (key : Inode_Identity) is
      i : constant Version_Slot := versionSlotOf (key);
   begin
      versionClock := versionClock + 1;
      inodeVersions (i).version := versionClock;
   end bumpVersion;

   function queueOf (owner : ProcessID) return Client_Queue_Count is
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner = owner then
            return q;
         end if;
      end loop;
      return No_Client_Queue;
   end queueOf;

   function isFileOf (slot : Handle_Slot; key : Inode_Identity) return Boolean is
     (files (slot).active and then files (slot).objectKind = FILE_OBJECT and then
      files (slot).filesystemKind = EXT2_FILESYSTEM and then
      files (slot).volume = key.volume and then files (slot).inodeNum = key.number);

   --  Grant (publish the file's identity, version and size, then the valid
   --  word) or revoke (clear the valid word) slot's delegation in its
   --  owner's queue page.
   procedure delegate (slot : Handle_Slot; grant : Boolean) is
      q : constant Client_Queue_Count := queueOf (files (slot).ownerPID);
   begin
      delegated (slot) := grant and then q /= No_Client_Queue;
      if q = No_Client_Queue then
         return;
      end if;
      declare
         entryAt : constant Natural := FQ.Delegations_At + slot * FQ.Delegation_Bytes;
         valid : Unsigned_32 with Volatile, Import,
           Address => queueWord (q, entryAt + FQ.Delegation_Valid_At);
         inode : Unsigned_64 with Volatile, Import,
           Address => queueWord (q, entryAt + FQ.Delegation_Inode_At);
         version : Unsigned_64 with Volatile, Import,
           Address => queueWord (q, entryAt + FQ.Delegation_Version_At);
         size : Unsigned_64 with Volatile, Import,
           Address => queueWord (q, entryAt + FQ.Delegation_Size_At);
         key : constant Inode_Identity := (files (slot).volume, files (slot).inodeNum);
      begin
         valid := 0;
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         if grant then
            inode := Shift_Left (Unsigned_64 (key.volume), 32) or Unsigned_64 (key.number);
            version := inodeVersions (versionSlotOf (key)).version;
            size := Ext2.fileSize
              (Open_Inodes.Value (inodeObjects, Open_Inodes.Owner_Index (slot)));
            System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
            valid := 1;
         end if;
      end;
   end delegate;

   --  The file key is about to change through slot except: every other
   --  handle's delegation goes first, and the version moves on.
   procedure fileChanging (key : Inode_Identity; except : Integer) is
   begin
      for j in Handle_Slot loop
         if j /= except and then delegated (j) and then isFileOf (j, key) then
            delegate (j, False);
         end if;
      end loop;
      bumpVersion (key);
   end fileChanging;

   --  After slot's own change: its delegation, if held, carries the new
   --  version and size.
   procedure refreshDelegation (slot : Handle_Slot) is
   begin
      if delegated (slot) then
         delegate (slot, True);
      end if;
   end refreshDelegation;

   --  A handle was opened: a writable open revokes the others; the handle
   --  is delegated if no other handle can write the file.
   procedure handleOpened (slot : Handle_Slot) is
      key : constant Inode_Identity := (files (slot).volume, files (slot).inodeNum);
      otherWriter : Boolean := False;
   begin
      for j in Handle_Slot loop
         if j /= slot and then isFileOf (j, key) then
            if (files (slot).openRights and ACL_WRITE) /= 0 and then delegated (j) then
               delegate (j, False);
            end if;
            if (files (j).openRights and ACL_WRITE) /= 0 then
               otherWriter := True;
            end if;
         end if;
      end loop;
      if not otherWriter then
         delegate (slot, True);
      end if;
   end handleOpened;

   --  Answer the entry being handled on its queue, and wake the client's
   --  WAIT if one is saved.
   procedure answerEntry (label : Unsigned_32; word0, word1 : Unsigned_64) is
      q : constant Client_Queue_Count := curRoute.queue;
   begin
      if q = No_Client_Queue or else clientQueues (q).server.Owed = 0 then
         return;
      end if;
      declare
         ring : FQueues.Completions.Ring with Import,
           Address => queueWord (q, FQ.Answers_At);
         produced : Unsigned_32 with Volatile, Import,
           Address => queueWord (q, FQ.Completions_At + FQ.Produced_At);
         ignore : Unsigned_64;
      begin
         --  Owed <= Space (FQueues.Valid): the answer has its slot.
         FQueues.Complete
           (clientQueues (q).server, ring, curRoute.token,
            (Status => label, Value => word0, Spare => word1, others => <>));
         --  The answer is written before the count that hands it over.
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         produced := Unsigned_32 (clientQueues (q).server.Answers.Produced);
         if clientQueues (q).waiting then
            clientQueues (q).waiting := False;
            ignore := replyCap
              (CapabilitySlot (WAIT_REPLY_SLOT_BASE + q),
               (tag => (label => REPLY_OK, length => 0, flags => 0, reserved => 0),
                authorityTag => 0, words => [others => 0]));
         end if;
      end;
   end answerEntry;

   procedure acquireClientMemory
     (sender        : ProcessID;
      rawSlot       : Unsigned_64;
      rawGeneration : Unsigned_64;
      byteLength    : Unsigned_64;
      requiredAccess : CuBit.Memory_Grants.Required_Access;
      address       : out System.Address;
      success       : out Boolean)
   is
   begin
      --  A queue entry's data is in the arena range already checked.
      if curRoute.queue /= No_Client_Queue then
         address := curArena;
         success := curArena /= System.Null_Address and then
                    byteLength <= curArenaBytes;
         return;
      end if;
      address := System.Null_Address;
      success := False;
      if rawSlot > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
         rawGeneration = 0 or else
         rawGeneration > CuBit.Memory_Grants.MAXIMUM_GENERATION or else
         byteLength = 0
      then
         return;
      end if;

      CuBit.Memory_Grants.Acquire
        (reference      =>
           (slot => CuBit.Memory_Grants.Global_Grant_Slot (rawSlot),
            generation =>
              CuBit.Memory_Grants.Grant_Generation (rawGeneration)),
         expectedOwner => sender,
         byteOffset     => 0,
         byteLength     => byteLength,
         requiredAccess => requiredAccess,
         mappedAddress  => address,
         success        => success);
   end acquireClientMemory;

   procedure returnClientMemory
     (rawSlot       : Unsigned_64;
      rawGeneration : Unsigned_64;
      success       : out Boolean)
   is
   begin
      if curRoute.queue /= No_Client_Queue then
         success := True;   --  the arena stays mapped
         return;
      end if;
      CuBit.Memory_Grants.Return_Acquisition
        ((slot => CuBit.Memory_Grants.Global_Grant_Slot (rawSlot),
          generation =>
            CuBit.Memory_Grants.Grant_Generation (rawGeneration)),
         success);
   end returnClientMemory;

   --  Send a reply with the given label and word0 value
   procedure sendReply
     (dest   : ProcessID;
      label  : Unsigned_32;
      word0  : Unsigned_64)
   is
      replyMsg : Message;
      ignore   : Unsigned_64;
   begin
      if curRoute.queue /= No_Client_Queue then
         answerEntry (label, word0, 0);
         return;
      end if;
      replyMsg.tag := (label  => label,
                       length => 1,
                       flags  => 0,
                       reserved  => 0);
      replyMsg.words := (0 => word0, others => 0);
      ignore := reply (dest, replyMsg);
   end sendReply;

   --  Handle OP_SET_ACL
   --  words(0) = target PID
   --  words(1) = entry count (0 = wildcard full access)
   --  words(2) = grant slot (when count > 0)
   --  words(3) = grant generation (when count > 0)
   procedure handleSetACL (sender : ProcessID; msg : Message) is
      targetPID     : constant ProcessID := msg.words (0);
      entryCountRaw : constant Unsigned_64 := msg.words (1);
      entryCount    : Natural := 0;
      slotIdx       : Integer := -1;
      grantAddr     : System.Address := System.Null_Address;
      grantOk       : Boolean := False;
      returned      : Boolean := False;
      candidate     : CuBit.File_Access.Policy;
      decoded       : Boolean := False;
   begin
      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      if msg.tag.length /= 4 or else targetPID = NO_PROCESS or else
        entryCountRaw > Unsigned_64 (MAX_ACL_ENTRIES)
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      entryCount := Natural (entryCountRaw);

      --  Find existing profile or allocate a free slot
      for i in aclProfiles'Range loop
         if aclProfiles (i).active and then
            aclProfiles (i).pid = targetPID
         then
            slotIdx := i;
            exit;
         end if;
      end loop;

      if slotIdx < 0 then
         for i in aclProfiles'Range loop
            if not aclProfiles (i).active then
               slotIdx := i;
               exit;
            end if;
         end loop;
      end if;

      if slotIdx < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if entryCount > 0 then
         acquireClientMemory
           (sender, msg.words (2), msg.words (3),
            Unsigned_64 (entryCount * 72),
            CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
         if not grantOk then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;
      end if;

      if entryCount = 0 then
         --  Existing trusted bootstrap operation, not the default Policy.
         CuBit.File_Access.Allow_All_For_Bootstrap (candidate);
      else
         declare
            buf : CuBit.File_Access.Wire_Bytes (1 .. entryCount * 72)
              with Import, Address => grantAddr;
            snapshot : constant CuBit.File_Access.Wire_Bytes := buf;
         begin
            CuBit.File_Access.Decode (snapshot, candidate, decoded);
         end;
         returnClientMemory
           (msg.words (2), msg.words (3), returned);
         if not returned or else not decoded then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;
      end if;

      --  Publish one validated policy. Invalid requests above neither change
      --  the old policy nor revoke its handles. The service serializes updates.
      releaseHandlesForOwner (targetPID);
      aclProfiles (slotIdx).pid := targetPID;
      aclProfiles (slotIdx).policy := candidate;
      aclProfiles (slotIdx).active := True;
      debugPrint ("FS Server: ACL set for PID" & LF);
      sendReply (sender, REPLY_OK, 0);
   end handleSetACL;

   --  Handle OP_REVOKE_ACL
   --  words(0) = target PID
   procedure handleRevokeACL (sender : ProcessID; msg : Message) is
      targetPID : constant ProcessID := msg.words (0);
   begin
      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      for i in aclProfiles'Range loop
         if aclProfiles (i).active and then
            aclProfiles (i).pid = targetPID
         then
            aclProfiles (i).active := False;
            aclProfiles (i).pid    := NO_PROCESS;
            CuBit.File_Access.Clear (aclProfiles (i).policy);
         end if;
      end loop;

      releaseHandlesForOwner (targetPID);

      sendReply (sender, REPLY_OK, 0);
   end handleRevokeACL;

   FS_PAGE_SIZE : constant := 4096;

   --  Generic endpoint admission. The registry role is only a boot-order hint;
   --  grant creation is authorized by the endpoint already installed by devmgr.
   procedure ensureVolume (Volume : Volume_Index; Result : out Admission_Result) is
      Device : constant Device_Binding := Binding (Volumes, Volume);
      Context : Volume_Context renames Contexts (Volume);
      Grant_OK, Revoked : Boolean;
      Provider : Unsigned_64;
      Raw : Unsigned_64;
   begin
      Result := Context.Status;
      if Context.Status /= Provider_Not_Ready then
         return;
      end if;
      if Device.Ready_Role /= 0 then
         Provider := getInfo (SYSINFO_REGISTERED_DRIVER, Device.Ready_Role);
         if Provider = 0 or else Provider = Unsigned_64'Last then
            return; -- not supplied yet; no session or identity was replaced
         end if;
      end if;
      if Context.Buffer = System.Null_Address then
         Raw := syscall
           (SYSCALL_SBRK, Unsigned_64 (Device.Transfer_Pages + 1) * FS_PAGE_SIZE);
         if Raw = 0 or else Raw = Unsigned_64'Last then
            Result := Insufficient_Resources;
            return;
         end if;
         Context.Buffer := toAddr
           ((Raw + FS_PAGE_SIZE - 1) and not (FS_PAGE_SIZE - 1));
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (Device.Endpoint, Context.Buffer, Device.Transfer_Pages, True,
         Context.Grant, Grant_OK);
      if not Grant_OK then
         Result := Grant_Rejected;
         return;
      end if;
      Ext2.initBlockDevice
        (Context.Fs, Device.Endpoint, Context.Grant, Context.Buffer,
         Unsigned_32 (Device.Transfer_Pages) * FS_PAGE_SIZE, Result);
      Context.Status := Result;
      if Result = Admitted then
         debugPrint ("FS Server: volume " & Volume_List.Name (Volumes, Volume) & " ready." & LF);
      else
         CuBit.Memory_Grants.Revoke (Context.Grant, Revoked);
         --  No rebinding/retry after admission uncertainty. A pending revoked
         --  grant must not be overwritten with a new reference.
         debugPrint ("FS Server: volume " & Volume_List.Name (Volumes, Volume) &
                     " rejected: " & Description (Result) & LF);
      end if;
   end ensureVolume;

   --  Reject paths containing ".." traversal components.
   --  Checks for: exact "..", leading "../", trailing "/..", and "/../".
   function hasTraversal (path : String) return Boolean is
   begin
      if path'Length = 2 and then
         path (path'First) = '.' and then path (path'First + 1) = '.'
      then
         return True;
      end if;

      for i in path'First .. path'Last - 1 loop
         if path (i) = '.' and then path (i + 1) = '.' then
            declare
               atStart : constant Boolean :=
                 (i = path'First) or else path (i - 1) = '/';
               atEnd   : constant Boolean :=
                 (i + 1 = path'Last) or else path (i + 2) = '/';
            begin
               if atStart and atEnd then
                  return True;
               end if;
            end;
         end if;
      end loop;

      return False;
   end hasTraversal;

   function Lookup_Reply_Label
     (status : Ext2.Directory_Lookup_Status) return Unsigned_32 is
   begin
      case status is
         when Ext2.Lookup_Found => return REPLY_OK;
         when Ext2.Lookup_Not_Found => return REPLY_NOT_FOUND;
         when Ext2.Lookup_Malformed => return REPLY_MALFORMED_FILESYSTEM;
         when Ext2.Lookup_Device_Error => return REPLY_IO_ERROR;
         when Ext2.Lookup_Out_Of_Range => return REPLY_OUT_OF_RANGE;
         when Ext2.Lookup_Range_Unsupported => return REPLY_FILE_RANGE_UNSUPPORTED;
      end case;
   end Lookup_Reply_Label;

   function replyForWrite
     (status : Ext2.Write_Status) return Unsigned_32
   is
   begin
      case status is
         when Ext2.Write_Complete =>
            return REPLY_OK;
         when Ext2.Write_Read_Only =>
            return REPLY_READ_ONLY;
         when Ext2.Write_Out_Of_Range =>
            return REPLY_OUT_OF_RANGE;
         when Ext2.Write_Device_Error =>
            return REPLY_IO_ERROR;
         when Ext2.Write_Recovery_Required =>
            return REPLY_RECOVERY_REQUIRED;
         when Ext2.Write_Already_Exists =>
            return REPLY_ALREADY_EXISTS;
         when Ext2.Write_No_Space =>
            return REPLY_NO_SPACE;
         when Ext2.Write_File_Range_Unsupported =>
            return REPLY_FILE_RANGE_UNSUPPORTED;
         when Ext2.Write_Object_Unsupported =>
            return REPLY_UNSUPPORTED_OBJECT;
      end case;
   end replyForWrite;

   --  Handle OP_OPEN: path grant, path length, open options, grant generation.
   procedure handleOpen (sender : ProcessID; msg : Message) is
      pathLen   : constant Unsigned_64 := msg.words (1);
      openFlags : constant Open_Options := Open_Options (msg.words (2));
      grantAddr : System.Address := System.Null_Address;
      grantOk   : Boolean := False;
      returned  : Boolean := False;
      pathBuffer : String (1 .. Natural (MAXIMUM_PATH_BYTES));

      handle     : Integer;
      handleId   : Unsigned_64;
      allocated  : Boolean;
      inodeNum   : Unsigned_32 := 0;
      selection  : Path_Selection;
      reference  : Volume_Reference;
      useVolume  : Volume_Index := Volume_Index'First;
      relStart   : Natural;
      selectedKind : Filesystem_Kind := CPIO_ARCHIVE;
      cpioIdx    : Natural := 0;
      opticalFile : ISO_Records.File_Record;
      opticalFound : Boolean;
      objectInode : Ext2.Inode := Ext2.NULL_INODE;
      inodeStatus : Ext2.Read_Status;
      truncateStatus : Ext2.Truncate_Status;
      createStatus : Ext2.Write_Status := Ext2.Write_File_Range_Unsupported;
      pathStatus : Ext2.Directory_Lookup_Status := Ext2.Lookup_Not_Found;
      attached : Open_Inodes.Attach_Result;
   begin
      if msg.tag.length /= 4 or else
         pathLen = 0 or else pathLen > MAXIMUM_PATH_BYTES or else
         not Valid_Open_Options (openFlags)
      then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (0), msg.words (3), pathLen,
         CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
      if not grantOk then
         sendReply (sender, REPLY_ACCESS_DENIED, Unsigned_64'Last);
         return;
      end if;

      declare
         grantedPath : String (1 .. Natural (pathLen))
           with Import, Address => grantAddr;
      begin
         pathBuffer (1 .. Natural (pathLen)) := grantedPath;
      end;
      returnClientMemory (msg.words (0), msg.words (3), returned);
      if not returned then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      --  Read the path and select an authorized volume binding.
      declare
         pathStr : String renames pathBuffer (1 .. Natural (pathLen));

      begin
         if hasTraversal (pathStr) then
            sendReply (sender, REPLY_ERR, Unsigned_64'Last);
            return;
         end if;

         declare
            requiredRights : Unsigned_8 := 0;
         begin
            if Requests_Read (openFlags) then
               requiredRights := requiredRights or ACL_READ;
            end if;
            if Requests_Write (openFlags) then
               requiredRights := requiredRights or ACL_WRITE;
            end if;

            if (openFlags and OPEN_CREATE) /= 0 then
               requiredRights := requiredRights or ACL_WRITE or ACL_CREATE;
            end if;

            if not checkAccess (sender, pathStr, requiredRights) then
               debugPrint ("FS: access denied for PID" & LF);
               sendReply (sender, REPLY_ACCESS_DENIED, Unsigned_64'Last);
               return;
            end if;
         end;

         Select_Path (Volumes, pathStr, selection, reference, relStart);
         if selection in Unknown_Volume | Invalid_Path then
            sendReply (sender, REPLY_NOT_FOUND, 0);
            return;
         elsif selection = Known_Volume then
            selectedKind := EXT2_FILESYSTEM;
            useVolume := Volume_Index (reference);
            declare
               admission : Admission_Result;
            begin
               ensureVolume (useVolume, admission);
               if admission /= Admitted then
                  sendReply (sender, REPLY_IO_ERROR, 0);
                  return;
               end if;
            end;
            Ext2.resolvePath
              (Contexts (useVolume).Fs, pathStr (relStart .. pathStr'Last),
               inodeNum, pathStatus);
         else
            --  Preserve bootstrap lookup order. Names confer no authority:
            --  the caller's original path was checked before selection.
            if cpioOk then
               cpioIdx := Cpio.findFile (cpioArchive, pathStr);
               if cpioIdx < cpioArchive.count then
                  selectedKind := CPIO_ARCHIVE;
                  inodeNum := 1;
               end if;
            end if;
            if inodeNum = 0 then
               ISO9660.Find (pathStr, opticalFile, opticalFound);
               if opticalFound then
                  selectedKind := ISO_FILESYSTEM;
                  inodeNum := 1;
               elsif ISO9660.Media_Failed then
                  sendReply (sender, REPLY_IO_ERROR, 0);
                  return;
               end if;
            end if;
            for V in 1 .. Count (Volumes) loop
               exit when inodeNum /= 0 or else pathStatus /= Ext2.Lookup_Not_Found;
               declare
                  admission : Admission_Result;
               begin
                  ensureVolume (V, admission);
                  if admission = Admitted then
                     Ext2.resolvePath (Contexts (V).Fs, pathStr, inodeNum, pathStatus);
                     if inodeNum /= 0 then
                        selectedKind := EXT2_FILESYSTEM;
                        useVolume := V;
                     end if;
                  elsif not May_Search_Next (admission) then
                     sendReply (sender, REPLY_IO_ERROR, 0);
                     return;
                  end if;
               end;
            end loop;
            --  Automatic creation retains its existing RAM-workspace default,
            --  rather than selecting an arbitrary disk that missed the name.
            if inodeNum = 0 and then Default_Write_Volume /= No_Volume and then
              Contexts (Volume_Index (Default_Write_Volume)).Status = Admitted
            then
               selectedKind := EXT2_FILESYSTEM;
               useVolume := Volume_Index (Default_Write_Volume);
            end if;
         end if;
      end;

      if pathStatus not in Ext2.Lookup_Found | Ext2.Lookup_Not_Found then
         sendReply (sender, Lookup_Reply_Label (pathStatus), 0);
         return;
      end if;

      if inodeNum /= 0 and then selectedKind in CPIO_ARCHIVE | ISO_FILESYSTEM and then
        (Requests_Write (openFlags) or else
         (openFlags and (OPEN_CREATE or OPEN_TRUNCATE or OPEN_EXCLUSIVE)) /= 0)
      then
         sendReply (sender, REPLY_READ_ONLY, 0);
         return;
      end if;
      if selectedKind = ISO_FILESYSTEM and then opticalFile.Directory then
         sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
         return;
      end if;

      if (openFlags and OPEN_EXCLUSIVE) /= 0 then
         --  Lookup and creation execute in one request in this single-threaded
         --  service. A future concurrent dispatcher must preserve that
         --  serialization; a client-side exists-then-create is not equivalent.
         if selectedKind in CPIO_ARCHIVE | ISO_FILESYSTEM then
            sendReply (sender, REPLY_READ_ONLY, 0);
            return;
         end if;
         if inodeNum /= 0 then
            sendReply (sender, REPLY_ALREADY_EXISTS, 0);
            return;
         end if;
      end if;

      --  Check handle capacity before create/truncate can mutate storage.
      --  This is a preflight, not a reservation: dispatch is still serialized.
      allocHandle (handleId, handle, allocated);
      if not allocated then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      if inodeNum = 0 then
         --  OPEN_CREATE: create the file if it doesn't exist
         if (openFlags and OPEN_CREATE) /= 0 and
            selectedKind = EXT2_FILESYSTEM
         then
            declare
               pathStr : String renames
                 pathBuffer (1 .. Natural (pathLen));

               --  Select_Path already removed the complete volume name.
               relPath   : String renames
                 pathStr (relStart .. Natural (pathLen));
               fileStart : Natural := relPath'First;
               dirEnd    : Natural := 0;
               nameFirst : Natural;
            begin
               if fileStart > relPath'Last then
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;

               --  Find last '/' in the relative path to split dir/name
               for i in reverse fileStart .. relPath'Last loop
                  if relPath (i) = '/' then
                     dirEnd := i;
                     exit;
                  end if;
               end loop;

               if dirEnd = 0 then
                  nameFirst := fileStart;
               else
                  nameFirst := dirEnd + 1;
               end if;

               if nameFirst > relPath'Last then
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;

               declare
                  dirInodeNum : Unsigned_32 := Ext2.ROOT_INODE;
               begin
                  --  Resolve parent directory (relative path only)
                  if dirEnd > 0 then
                     Ext2.resolvePath
                       (Contexts (useVolume).Fs, relPath (fileStart .. dirEnd - 1),
                        dirInodeNum, pathStatus);
                  end if;

                  if dirEnd > 0 and then pathStatus /= Ext2.Lookup_Found then
                     sendReply (sender, Lookup_Reply_Label (pathStatus), 0);
                     return;
                  elsif dirInodeNum = 0 then
                     debugPrint ("FS: parent dir not found" & LF);
                     sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                     return;
                  end if;

                  Ext2.createFile
                    (Contexts (useVolume).Fs, dirInodeNum,
                     relPath (nameFirst .. relPath'Last), inodeNum, createStatus);
                  if createStatus /= Ext2.Write_Complete then
                     sendReply (sender, replyForWrite (createStatus), 0);
                     return;
                  end if;
               end;
            end;
         end if;

         if inodeNum = 0 then
            debugPrint ("FS: file not found" & LF);
            sendReply (sender, REPLY_NOT_FOUND, Unsigned_64'Last);
            return;
         end if;
      end if;

      --  Resolve metadata with checked I/O, then join the shared object.
      --  Ownership, rights and cursor remain in the per-process handle.
      if selectedKind = EXT2_FILESYSTEM then
         Ext2.readInode (Contexts (useVolume).Fs, inodeNum, objectInode, inodeStatus);
         if inodeStatus /= Ext2.Read_Complete then
            sendReply (sender, REPLY_IO_ERROR, 0);
            return;
         end if;
         --  Gate the actual inode, not the untrusted directory-entry type.
         --  No handle/alias is attached and no truncate occurs on rejection.
         case Ext2_Support.Check_File (objectInode) is
            when Ext2_Support.File_Allowed => null;
            when Ext2_Support.Not_A_Regular_File =>
               sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
               return;
            when Ext2_Support.Not_A_Single_Link | Ext2_Support.Unsupported_Metadata =>
               sendReply (sender, REPLY_UNSUPPORTED_OBJECT, 0);
               return;
         end case;
         Open_Inodes.Attach
           (inodeObjects, Open_Inodes.Owner_Index (handle),
            (useVolume, inodeNum), objectInode, attached,
            (if (openFlags and OPEN_DENY_SHARING) /= 0 then Open_Inodes.Deny_Sharing
             else Open_Inodes.Allow_Sharing));
         if attached = Open_Inodes.Sharing_Conflict then
            sendReply (sender, REPLY_SHARING_VIOLATION, 0);
            return;
         elsif attached not in Open_Inodes.Created | Open_Inodes.Shared then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;

         if (openFlags and OPEN_TRUNCATE) /= 0 then
            fileChanging ((useVolume, inodeNum), handle);
            Ext2.truncateToEmpty
              (Contexts (useVolume).Fs, inodeNum, objectInode, truncateStatus);
            if truncateStatus /= Ext2.Truncate_Complete then
               --  Never leave old, possibly freed block mappings reachable.
               for Other in files'Range loop
                  if truncateStatus = Ext2.Truncate_Recovery_Required and then
                    files (Other).active and then
                    files (Other).objectKind = FILE_OBJECT and then
                    files (Other).filesystemKind = selectedKind and then
                    files (Other).volume = useVolume and then
                    files (Other).inodeNum = inodeNum
                  then
                     releaseHandle (Other);
                  end if;
               end loop;
               Open_Inodes.Detach
                 (inodeObjects, Open_Inodes.Owner_Index (handle));
               case truncateStatus is
                  when Ext2.Truncate_Recovery_Required =>
                     sendReply (sender, REPLY_RECOVERY_REQUIRED, 0);
                  when Ext2.Truncate_Read_Only =>
                     sendReply (sender, REPLY_READ_ONLY, 0);
                  when Ext2.Truncate_Unsupported =>
                     sendReply (sender, REPLY_FILE_RANGE_UNSUPPORTED, 0);
                  when Ext2.Truncate_Durability_Unsupported =>
                     sendReply (sender, REPLY_DURABILITY_UNSUPPORTED, 0);
                  when others =>
                     sendReply (sender, REPLY_IO_ERROR, 0);
               end case;
               return;
            end if;
            Open_Inodes.Replace
              (inodeObjects, Open_Inodes.Owner_Index (handle), objectInode);
         end if;
      end if;

      --  Publish the per-client handle only after admission succeeds.
      files (handle).active      := True;
      files (handle).inodeNum    := inodeNum;
      files (handle).offset      := 0;
      files (handle).ownerPID    := sender;
      files (handle).filesystemKind     := selectedKind;
      files (handle).volume      := useVolume;
      files (handle).cpioFileIdx := cpioIdx;
      files (handle).opticalFile := opticalFile;
      files (handle).openRights := 0;
      files (handle).objectKind := FILE_OBJECT;
      if Requests_Read (openFlags) then
         files (handle).openRights :=
           files (handle).openRights or ACL_READ;
      end if;
      if Requests_Write (openFlags) then
         files (handle).openRights :=
           files (handle).openRights or ACL_WRITE;
      end if;
      if selectedKind = EXT2_FILESYSTEM then
         handleOpened (handle);
      end if;

      --  Reply with handle in words(0) and file size in words(1)
      declare
         fsize    : Unsigned_64 := 0;
         replyMsg : Message;
         ignore   : Unsigned_64;
      begin
         case selectedKind is
            when ISO_FILESYSTEM =>
               fsize := Unsigned_64 (opticalFile.Bytes);
            when CPIO_ARCHIVE =>
               fsize := cpioArchive.files (cpioIdx).dataSize;
            when EXT2_FILESYSTEM =>
               fsize := Ext2.fileSize (Open_Inodes.Value
                 (inodeObjects, Open_Inodes.Owner_Index (handle)));
         end case;

         replyMsg.tag := (label  => REPLY_OK,
                          length => 2,
                          flags  => 0,
                          reserved  => 0);
         replyMsg.words := [0 => handleId,
                            1 => fsize,
                            others => 0];
         if curRoute.queue /= No_Client_Queue then
            answerEntry (REPLY_OK, handleId, fsize);
         else
            ignore := reply (sender, replyMsg);
         end if;
      end;
   end handleOpen;

   type File_Position_Mode is (Advance_Cursor, Explicit_Offset);

   --  Shared execution paths: positioned requests do not temporarily seek.
   --  Handle OP_READ
   --  words(0) = file_handle
   --  words(1) = grant slot (buffer to write data into)
   --  words(2) = count (bytes to read)
   --  words(3) = grant generation
   procedure handleRead
     (sender : ProcessID; msg : Message;
      mode : File_Position_Mode := Advance_Cursor;
      explicitOffset : Unsigned_64 := 0)
   is
      transferOffset : Unsigned_64;
      currentInode : Ext2.Inode;
      handle    : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
      count     : constant Unsigned_64 := msg.words (2);
      grantAddr : System.Address := System.Null_Address;
      grantOk   : Boolean := False;
      bytesRead : Unsigned_64;
      readStatus : Ext2.Read_Status := Ext2.Read_Complete;
      returned  : Boolean := False;

      function replyForRead
        (status : Ext2.Read_Status) return Unsigned_32
      is
      begin
         case status is
            when Ext2.Read_Complete =>
               return REPLY_OK;
            when Ext2.Read_Out_Of_Range =>
               return REPLY_OUT_OF_RANGE;
            when Ext2.Read_Device_Error =>
               return REPLY_IO_ERROR;
            when Ext2.Read_File_Range_Unsupported =>
               return REPLY_FILE_RANGE_UNSUPPORTED;
            when Ext2.Read_Object_Unsupported =>
               return REPLY_UNSUPPORTED_OBJECT;
         end case;
      end replyForRead;
   begin
      if msg.tag.length /= 4 or else handle < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if (files (handle).openRights and ACL_READ) = 0 then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      transferOffset := (if mode = Explicit_Offset then explicitOffset
                         else files (handle).offset);
      if count > Unsigned_64'Last - transferOffset then
         sendReply (sender, REPLY_OUT_OF_RANGE, 0);
         return;
      end if;

      if count = 0 then
         sendReply (sender, REPLY_OK, 0);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (1), msg.words (3), count,
         CuBit.Memory_Grants.Write_Access, grantAddr, grantOk);
      if not grantOk then
         debugPrint ("FS: read grant acquisition denied" & LF);
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      if files (handle).filesystemKind = EXT2_FILESYSTEM then
         currentInode := Open_Inodes.Value
           (inodeObjects, Open_Inodes.Owner_Index (handle));
      end if;
      case files (handle).filesystemKind is
         when ISO_FILESYSTEM =>
            declare
               ok : Boolean;
            begin
               ISO9660.Read (files (handle).opticalFile, transferOffset,
                             count, grantAddr, bytesRead, ok);
               readStatus := (if ok then Ext2.Read_Complete
                              else Ext2.Read_Device_Error);
            end;
         when CPIO_ARCHIVE =>
            bytesRead := Cpio.readData
              (cpioArchive,
               files (handle).cpioFileIdx,
               transferOffset,
               grantAddr,
               count);
            readStatus := Ext2.Read_Complete;
         when EXT2_FILESYSTEM =>
            Ext2.readData
              (Contexts (files (handle).volume).Fs,
               currentInode,
               transferOffset,
               grantAddr,
               count,
               bytesRead,
               readStatus);
      end case;

      if mode = Advance_Cursor then
         files (handle).offset := transferOffset + bytesRead;
      end if;

      returnClientMemory (msg.words (1), msg.words (3), returned);
      if not returned then
         debugPrint ("FS: read grant return failed" & LF);
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      sendReply (sender, replyForRead (readStatus), bytesRead);
   end handleRead;

   --  Handle OP_WRITE
   --  words(0) = file_handle
   --  words(1) = grant slot (buffer containing data to write)
   --  words(2) = count (bytes to write)
   --  words(3) = grant generation
   procedure handleWrite
     (sender : ProcessID; msg : Message;
      mode : File_Position_Mode := Advance_Cursor;
      explicitOffset : Unsigned_64 := 0)
   is
      transferOffset : Unsigned_64;
      currentInode : Ext2.Inode;
      handle       : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
      count        : constant Unsigned_64 := msg.words (2);
      grantAddr    : System.Address := System.Null_Address;
      grantOk      : Boolean := False;
      bytesWritten : Unsigned_64;
      writeStatus  : Ext2.Write_Status := Ext2.Write_Complete;
      returned     : Boolean := False;

   begin
      if msg.tag.length /= 4 or else handle < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if (files (handle).openRights and ACL_WRITE) = 0 then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      transferOffset := (if mode = Explicit_Offset then explicitOffset
                         else files (handle).offset);
      if count > Unsigned_64'Last - transferOffset then
         sendReply (sender, REPLY_OUT_OF_RANGE, 0);
         return;
      end if;

      if count = 0 then
         sendReply (sender, REPLY_OK, 0);
         return;
      end if;

      if files (handle).filesystemKind in CPIO_ARCHIVE | ISO_FILESYSTEM then
         -- Immutable bootstrap storage is immutable; reject before acquiring the
         -- caller's memory so every successful acquisition has one exit.
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (1), msg.words (3), count,
         CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
      if not grantOk then
         debugPrint ("FS: write grant acquisition denied" & LF);
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      if files (handle).filesystemKind = EXT2_FILESYSTEM then
         currentInode := Open_Inodes.Value
           (inodeObjects, Open_Inodes.Owner_Index (handle));
      end if;
      case files (handle).filesystemKind is
         when CPIO_ARCHIVE | ISO_FILESYSTEM =>
            bytesWritten := 0; -- Rejected above.
         when EXT2_FILESYSTEM =>
            fileChanging ((files (handle).volume, files (handle).inodeNum), handle);
            Ext2.writeData
              (Contexts (files (handle).volume).Fs,
               files (handle).inodeNum,
               currentInode,
               transferOffset,
               grantAddr,
               count,
               bytesWritten,
               writeStatus);
      end case;

      if writeStatus = Ext2.Write_Recovery_Required then
         declare
            volume : constant Volume_Index := files (handle).volume;
            number : constant Unsigned_32 := files (handle).inodeNum;
         begin
            for Other in files'Range loop
               if files (Other).active and then
                 files (Other).objectKind = FILE_OBJECT and then
                 files (Other).filesystemKind = EXT2_FILESYSTEM and then
                 files (Other).volume = volume and then
                 files (Other).inodeNum = number
               then
                  releaseHandle (Other);
               end if;
            end loop;
         end;
         returnClientMemory (msg.words (1), msg.words (3), returned);
         sendReply (sender, REPLY_RECOVERY_REQUIRED, 0);
         return;
      end if;
      Open_Inodes.Replace
        (inodeObjects, Open_Inodes.Owner_Index (handle), currentInode);
      refreshDelegation (handle);

      --  The completed prefix is part of the file even when a later block
      --  fails.  Keep the handle synchronized with that committed progress.
      if mode = Advance_Cursor then
         files (handle).offset := transferOffset + bytesWritten;
      end if;

      returnClientMemory (msg.words (1), msg.words (3), returned);
      if not returned then
         debugPrint ("FS: write grant return failed" & LF);
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      sendReply (sender, replyForWrite (writeStatus), bytesWritten);
   end handleWrite;

   --  Decode transport metadata once, then use the identical authorization,
   --  grant acquisition, filesystemKind I/O and return path as cursor-based requests.
   procedure handlePositioned (sender : ProcessID; msg : Message) is
      request : Message := msg;
      loan : CuBit.Grant_References.Reference;
   begin
      if msg.tag.length /= 4 or else msg.tag.flags /= 0 or else
        msg.tag.reserved /= 0 or else
        not CuBit.Grant_References.Valid_Wire (msg.words (1))
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      loan := CuBit.Grant_References.Decode (msg.words (1));
      request.words (1) := loan.slot;
      request.words (3) := loan.generation;
      case msg.tag.label is
         when OP_READ_AT =>
            handleRead (sender, request, Explicit_Offset, msg.words (3));
         when OP_WRITE_AT =>
            handleWrite (sender, request, Explicit_Offset, msg.words (3));
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
   end handlePositioned;

   --  Handle OP_SEEK
   --  words(0) = file_handle
   --  words(1) = offset
   --  words(2) = whence (0=SET, 1=CUR, 2=END)
   procedure handleSeek (sender : ProcessID; msg : Message) is
      handle  : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
      seekOff : constant Unsigned_64 := msg.words (1);
      whence  : constant Unsigned_64 := msg.words (2);
      newOff  : Unsigned_64;
      size    : Unsigned_64;
   begin
      if handle < 0 then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      case files (handle).filesystemKind is
         when ISO_FILESYSTEM =>
            size := Unsigned_64 (files (handle).opticalFile.Bytes);
         when CPIO_ARCHIVE =>
            size := cpioArchive.files (files (handle).cpioFileIdx).dataSize;
         when EXT2_FILESYSTEM =>
            size := Ext2.fileSize (Open_Inodes.Value
              (inodeObjects, Open_Inodes.Owner_Index (handle)));
      end case;

      case whence is
         when 0 =>  --  SEEK_SET
            newOff := seekOff;
         when 1 =>  --  SEEK_CUR
            newOff := files (handle).offset + seekOff;
         when 2 =>  --  SEEK_END
            newOff := size + seekOff;
         when others =>
            sendReply (sender, REPLY_ERR, Unsigned_64'Last);
            return;
      end case;

      files (handle).offset := newOff;
      sendReply (sender, REPLY_OK, newOff);
   end handleSeek;

   --  Handle OP_CLOSE
   --  words(0) = file_handle
   procedure handleClose (sender : ProcessID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
   begin
      if handle < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if delegated (handle) then
         delegate (handle, False);
      end if;
      releaseHandle (handle);
      sendReply (sender, REPLY_OK, 0);
   end handleClose;

   procedure handleFlush (sender : ProcessID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
      status : Ext2.Flush_Status;
   begin
      if msg.tag.length /= 1 or else msg.tag.flags /= 0 or else
        msg.tag.reserved /= 0 or else handle < 0
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      elsif (files (handle).openRights and ACL_WRITE) = 0 then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;
      case files (handle).filesystemKind is
         when CPIO_ARCHIVE | ISO_FILESYSTEM =>
            status := Ext2.Flush_Unsupported;
         when EXT2_FILESYSTEM =>
            Ext2.Flush (Contexts (files (handle).volume).Fs, status);
      end case;
      case status is
         when Ext2.Flush_Complete => sendReply (sender, REPLY_OK, 0);
         when Ext2.Flush_Unsupported =>
            sendReply (sender, REPLY_DURABILITY_UNSUPPORTED, 0);
         when Ext2.Flush_IO_Error => sendReply (sender, REPLY_IO_ERROR, 0);
         when Ext2.Flush_Recovery_Required =>
            sendReply (sender, REPLY_RECOVERY_REQUIRED, 0);
      end case;
   end handleFlush;

   procedure handleResize (sender : ProcessID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
      updated : Ext2.Inode;
      status : Ext2.Truncate_Status;
   begin
      if msg.tag.length /= 2 or else msg.tag.flags /= 0 or else
        msg.tag.reserved /= 0 or else handle < 0
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      elsif (files (handle).openRights and ACL_WRITE) = 0 then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      elsif files (handle).filesystemKind /= EXT2_FILESYSTEM then
         sendReply (sender, REPLY_READ_ONLY, 0);
         return;
      end if;
      fileChanging ((files (handle).volume, files (handle).inodeNum), handle);
      Ext2.resizeFile
        (Contexts (files (handle).volume).Fs, files (handle).inodeNum,
         msg.words (1), updated, status);
      case status is
         when Ext2.Truncate_Complete =>
            Open_Inodes.Replace
              (inodeObjects, Open_Inodes.Owner_Index (handle), updated);
            refreshDelegation (handle);
            sendReply (sender, REPLY_OK, 0);
         when Ext2.Truncate_Recovery_Required =>
            declare
               volume : constant Volume_Index := files (handle).volume;
               number : constant Unsigned_32 := files (handle).inodeNum;
            begin
               for Other in files'Range loop
                  if files (Other).active and then
                    files (Other).objectKind = FILE_OBJECT and then
                    files (Other).filesystemKind = EXT2_FILESYSTEM and then
                    files (Other).volume = volume and then
                    files (Other).inodeNum = number
                  then
                     releaseHandle (Other);
                  end if;
               end loop;
            end;
            sendReply (sender, REPLY_RECOVERY_REQUIRED, 0);
         when Ext2.Truncate_Read_Only =>
            sendReply (sender, REPLY_READ_ONLY, 0);
         when Ext2.Truncate_Unsupported =>
            sendReply (sender, REPLY_FILE_RANGE_UNSUPPORTED, 0);
         when Ext2.Truncate_Durability_Unsupported =>
            sendReply (sender, REPLY_DURABILITY_UNSUPPORTED, 0);
         when Ext2.Truncate_Invalid | Ext2.Truncate_IO_Error =>
            sendReply (sender, REPLY_IO_ERROR, 0);
      end case;
   end handleResize;

   --  Open a directory by bootstrap path.  The returned object is a distinct
   --  PID-bound directory handle; subsequent enumeration carries no path.
   procedure handleOpenDirectory (sender : ProcessID; msg : Message) is
      pathLen : constant Unsigned_64 := msg.words (1);
      grantAddr : System.Address := System.Null_Address;
      grantOk : Boolean := False;
      returned : Boolean := False;
      pathBuffer : String (1 .. Natural (MAXIMUM_PATH_BYTES));
      selection : Path_Selection := Unqualified;
      reference : Volume_Reference;
      selectedVolume : Volume_Index := Volume_Index'First;
      relStart : Natural := 1;
      filesystemKind : Filesystem_Kind := CPIO_ARCHIVE;
      inodeNum : Unsigned_32 := 0;
      dirIno : Ext2.Inode;
      pathStatus : Ext2.Directory_Lookup_Status := Ext2.Lookup_Found;
      inodeStatus : Ext2.Read_Status;
      handleSlot : Integer;
      handleId : Unsigned_64;
      allocated : Boolean;

   begin
      if msg.tag.length /= 3 or else pathLen > MAXIMUM_PATH_BYTES then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if pathLen > 0 then
         acquireClientMemory
           (sender, msg.words (0), msg.words (2), pathLen,
            CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
         if not grantOk then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;
         declare
            grantedPath : String (1 .. Natural (pathLen))
              with Import, Address => grantAddr;
         begin
            pathBuffer (1 .. Natural (pathLen)) := grantedPath;
         end;
         returnClientMemory (msg.words (0), msg.words (2), returned);
         if not returned then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;
      end if;

      declare
         path : String renames pathBuffer (1 .. Natural (pathLen));
         onlySeparators : Boolean := True;
      begin
         if pathLen > 0 then
            if hasTraversal (path) then
               sendReply (sender, REPLY_ERR, 0);
               return;
            end if;
            if not checkAccess (sender, path, ACL_READ) then
               sendReply (sender, REPLY_ACCESS_DENIED, 0);
               return;
            end if;
            Select_Path (Volumes, path, selection, reference, relStart);
            for index in path'Range loop
               if path (index) /= '/' then
                  onlySeparators := False;
                  exit;
               end if;
            end loop;
         elsif not checkAccess (sender, "", ACL_READ) then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;

         if pathLen = 0 or else
           (selection = Unqualified and then onlySeparators)
         then
            if not cpioOk then
               sendReply (sender, REPLY_ERR, 0);
               return;
            end if;
            filesystemKind := CPIO_ARCHIVE;
            inodeNum := 1;
         elsif selection = Known_Volume then
            selectedVolume := Volume_Index (reference);
            declare
               admission : Admission_Result;
            begin
               ensureVolume (selectedVolume, admission);
               if admission /= Admitted then
                  sendReply (sender, REPLY_IO_ERROR, 0);
                  return;
               end if;
            end;
            filesystemKind := EXT2_FILESYSTEM;
            Ext2.resolvePath
              (Contexts (selectedVolume).Fs, path (relStart .. path'Last),
               inodeNum, pathStatus);
         elsif selection in Unknown_Volume | Invalid_Path then
            sendReply (sender, REPLY_NOT_FOUND, 0);
            return;
         else
            sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
            return;
         end if;
      end;

      if pathStatus /= Ext2.Lookup_Found then
         sendReply (sender, Lookup_Reply_Label (pathStatus), 0);
         return;
      end if;

      if filesystemKind /= CPIO_ARCHIVE then
         case filesystemKind is
            when EXT2_FILESYSTEM =>
               Ext2.readInode (Contexts (selectedVolume).Fs, inodeNum, dirIno, inodeStatus);
            when CPIO_ARCHIVE | ISO_FILESYSTEM => null;
         end case;
         if inodeStatus /= Ext2.Read_Complete then
            sendReply (sender,
              (if inodeStatus = Ext2.Read_Out_Of_Range then REPLY_OUT_OF_RANGE
               else REPLY_IO_ERROR), 0);
            return;
         end if;
         if Ext2.inodeType (dirIno) /= Ext2.INODE_DIRECTORY then
            sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
            return;
         end if;
      end if;

      allocHandle (handleId, handleSlot, allocated);
      if not allocated then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      files (handleSlot).active := True;
      files (handleSlot).filesystemKind := filesystemKind;
      files (handleSlot).volume := selectedVolume;
      files (handleSlot).inodeNum := inodeNum;
      if filesystemKind /= CPIO_ARCHIVE then
         files (handleSlot).directoryInode := dirIno;
      end if;
      files (handleSlot).offset := 0;
      files (handleSlot).ownerPID := sender;
      files (handleSlot).openRights := ACL_READ;
      files (handleSlot).objectKind := DIRECTORY_OBJECT;
      CuBit.Directory_Paths.Set_Root
        (pathBuffer (1 .. Natural (pathLen)),
         files (handleSlot).directoryPath, allocated);

      declare
         replyMsg : Message := NULL_MESSAGE;
         ignored : Unsigned_64;
      begin
         replyMsg.tag :=
           (label => REPLY_OK, length => 2, flags => 0, reserved => 0);
         replyMsg.words (0) := handleId;
         replyMsg.words (1) :=
           (if filesystemKind = CPIO_ARCHIVE then 0 else
              Unsigned_64 (dirIno.generationNumber));
         ignored := reply (sender, replyMsg);
      end;
   end handleOpenDirectory;

   function Read_Reply_Label (status : Ext2.Read_Status) return Unsigned_32 is
   begin
      case status is
         when Ext2.Read_Complete => return REPLY_OK;
         when Ext2.Read_Out_Of_Range => return REPLY_OUT_OF_RANGE;
         when Ext2.Read_Device_Error => return REPLY_IO_ERROR;
         when Ext2.Read_File_Range_Unsupported =>
            return REPLY_FILE_RANGE_UNSUPPORTED;
         when Ext2.Read_Object_Unsupported =>
            return REPLY_UNSUPPORTED_OBJECT;
      end case;
   end Read_Reply_Label;

   procedure handleOpenChildDirectory (sender : ProcessID; msg : Message) is
      parent : constant Integer :=
        resolveHandle (msg.words (0), sender, DIRECTORY_OBJECT);
      nameLength : constant Unsigned_64 := msg.words (1);
      nameBuffer : String (1 .. MAXIMUM_DIRECTORY_NAME_BYTES);
      childPath : CuBit.Directory_Paths.Path;
      address : System.Address;
      ok : Boolean;
      inodeNum : Unsigned_32 := 0;
      ino : Ext2.Inode;
      slot : Integer;
      identity : Unsigned_64;
      lookupReply : Unsigned_32 := REPLY_ERR;

      procedure Lookup (fs : Ext2.Filesystem) is
         parentInode : Ext2.Inode;
         readStatus : Ext2.Read_Status;
         lookupStatus : Ext2.Directory_Lookup_Status;
      begin
         --  Refresh metadata: directory contents may have grown since open.
         Ext2.readInode (fs, files (parent).inodeNum, parentInode, readStatus);
         if readStatus /= Ext2.Read_Complete then
            lookupReply := Read_Reply_Label (readStatus);
            return;
         elsif Ext2.inodeType (parentInode) /= Ext2.INODE_DIRECTORY then
            lookupReply := REPLY_WRONG_OBJECT_TYPE;
            return;
         end if;
         Ext2.lookupInDir
           (fs, parentInode, nameBuffer (1 .. Natural (nameLength)),
            inodeNum, lookupStatus);
         case lookupStatus is
            when Ext2.Lookup_Found =>
               Ext2.readInode (fs, inodeNum, ino, readStatus);
               lookupReply := Read_Reply_Label (readStatus);
               if readStatus /= Ext2.Read_Complete then
                  inodeNum := 0;
               end if;
            when Ext2.Lookup_Not_Found => lookupReply := REPLY_ERR;
            when Ext2.Lookup_Malformed =>
               lookupReply := REPLY_MALFORMED_FILESYSTEM;
            when Ext2.Lookup_Device_Error => lookupReply := REPLY_IO_ERROR;
            when Ext2.Lookup_Out_Of_Range => lookupReply := REPLY_OUT_OF_RANGE;
            when Ext2.Lookup_Range_Unsupported =>
               lookupReply := REPLY_FILE_RANGE_UNSUPPORTED;
         end case;
      end Lookup;
   begin
      if msg.tag.length /= 4 or else
        nameLength not in 1 .. MAXIMUM_DIRECTORY_NAME_BYTES
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      elsif parent < 0 then
         sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
         return;
      elsif (files (parent).openRights and ACL_READ) = 0 then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (2), msg.words (3), nameLength,
         CuBit.Memory_Grants.Read_Access, address, ok);
      if not ok then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;
      declare
         name : String (1 .. Natural (nameLength))
           with Import, Address => address;
      begin
         nameBuffer (name'Range) := name;
      end;
      returnClientMemory (msg.words (2), msg.words (3), ok);
      if not ok then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      CuBit.Directory_Paths.Append_Child
        (files (parent).directoryPath,
         nameBuffer (1 .. Natural (nameLength)), childPath, ok);
      if not ok then
         sendReply (sender, REPLY_OUT_OF_RANGE, 0);
         return;
      elsif not checkAccess
        (sender, CuBit.Directory_Paths.Value (childPath), ACL_READ)
      then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      case files (parent).filesystemKind is
         when EXT2_FILESYSTEM => Lookup (Contexts (files (parent).volume).Fs);
         when CPIO_ARCHIVE | ISO_FILESYSTEM =>
            --  The bootstrap archive exposes flat names, not directory objects.
            sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
            return;
      end case;
      if inodeNum = 0 then
         sendReply (sender, lookupReply, 0);
         return;
      elsif Ext2.inodeType (ino) /= Ext2.INODE_DIRECTORY then
         --  Do not follow symlinks or trust the directory entry's kind hint.
         sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
         return;
      end if;
      allocHandle (identity, slot, ok);
      if not ok then
         sendReply (sender, REPLY_NO_SPACE, 0);
         return;
      end if;
      files (slot).filesystemKind := files (parent).filesystemKind;
      files (slot).volume := files (parent).volume;
      files (slot).inodeNum := inodeNum;
      files (slot).directoryInode := ino;
      files (slot).offset := 0;
      files (slot).ownerPID := sender;
      files (slot).openRights := ACL_READ;
      files (slot).objectKind := DIRECTORY_OBJECT;
      files (slot).directoryPath := childPath;
      files (slot).active := True;
      sendReply (sender, REPLY_OK, identity);
   end handleOpenChildDirectory;

   procedure handleRewindDirectory (sender : ProcessID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, DIRECTORY_OBJECT);
      candidate : Ext2.Inode;
      status : Ext2.Read_Status := Ext2.Read_Complete;
   begin
      if msg.tag.length /= 1 or else handle < 0 then
         sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
         return;
      end if;
      case files (handle).filesystemKind is
         when EXT2_FILESYSTEM =>
            Ext2.readInode (Contexts (files (handle).volume).Fs, files (handle).inodeNum, candidate, status);
         when CPIO_ARCHIVE | ISO_FILESYSTEM => null;
      end case;
      if status /= Ext2.Read_Complete then
         sendReply (sender, Read_Reply_Label (status), 0);
         return;
      end if;
      if files (handle).filesystemKind /= CPIO_ARCHIVE then
         if Ext2.inodeType (candidate) /= Ext2.INODE_DIRECTORY then
            sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
            return;
         end if;
         files (handle).directoryInode := candidate;
      end if;
      files (handle).offset := 0;
      sendReply (sender, REPLY_OK, 0);
   end handleRewindDirectory;

   --  Return one Directory.Page.V1 into a fixed one-page writable grant.
   procedure handleReadDirectoryPage (sender : ProcessID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, DIRECTORY_OBJECT);
      grantAddr : System.Address := System.Null_Address;
      grantOk : Boolean := False;
      returned : Boolean := False;
      entryCount : Natural := 0;
      nextCursor : Unsigned_64 := 0;
      atEnd : Boolean := False;
      readStatus : Ext2.Directory_Read_Status := Ext2.Directory_Malformed;
      replyLabel : Unsigned_32 := REPLY_OK;
   begin
      if msg.tag.length /= 4 or else
         msg.words (2) /= Unsigned_64 (PROTOCOL_VERSION) or else handle < 0
      then
         sendReply
           (sender, (if handle < 0 then REPLY_WRONG_OBJECT_TYPE else
              REPLY_ERR), 0);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (1), msg.words (3), DIRECTORY_PAGE_BYTES,
         CuBit.Memory_Grants.Write_Access, grantAddr, grantOk);
      if not grantOk then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      declare
         rawPage : String (1 .. DIRECTORY_PAGE_BYTES)
           with Import, Address => grantAddr;
         header : Directory_Page_Header
           with Import, Address => grantAddr;
         pageEntries : Directory_Entries
           with Import,
                Address => grantAddr + DIRECTORY_PAGE_HEADER_BYTES;
      begin
         rawPage := (others => Character'Val (0));

         if files (handle).filesystemKind = CPIO_ARCHIVE then
            if files (handle).offset > Unsigned_64 (cpioArchive.count) then
               replyLabel := REPLY_MALFORMED_FILESYSTEM;
            else
               nextCursor := files (handle).offset;
               while nextCursor < Unsigned_64 (cpioArchive.count) and then
                 entryCount < MAXIMUM_DIRECTORY_PAGE_ENTRIES
               loop
                  declare
                     archiveIndex : constant Natural := Natural (nextCursor);
                     nameLength : constant Natural :=
                       cpioArchive.files (archiveIndex).nameLen;
                  begin
                     if nameLength = 0 or else
                       nameLength > MAXIMUM_DIRECTORY_NAME_BYTES
                     then
                        replyLabel := REPLY_MALFORMED_FILESYSTEM;
                        exit;
                     end if;
                     declare
                        archiveName : String (1 .. nameLength)
                          with Import,
                               Address => cpioArchive.base + Storage_Offset
                                 (cpioArchive.files (archiveIndex).nameOff);
                     begin
                        pageEntries (entryCount).objectHint := nextCursor + 1;
                        pageEntries (entryCount).sizeBytes :=
                          cpioArchive.files (archiveIndex).dataSize;
                        pageEntries (entryCount).nameLength :=
                          Unsigned_16 (nameLength);
                        pageEntries (entryCount).kind := DIRECTORY_KIND_FILE;
                        pageEntries (entryCount).flags :=
                          DIRECTORY_ENTRY_SIZE_VALID;
                        for index in 1 .. nameLength loop
                           pageEntries (entryCount).name (index) :=
                             Unsigned_8 (Character'Pos (archiveName (index)));
                        end loop;
                     end;
                     entryCount := entryCount + 1;
                     nextCursor := nextCursor + 1;
                  end;
               end loop;
               atEnd := nextCursor = Unsigned_64 (cpioArchive.count);
            end if;
         else
            case files (handle).filesystemKind is
               when EXT2_FILESYSTEM =>
                  Ext2.readDirectoryPage
                    (Contexts (files (handle).volume).Fs, files (handle).directoryInode, files (handle).offset,
                     pageEntries, entryCount, nextCursor, readStatus);
               when CPIO_ARCHIVE | ISO_FILESYSTEM => null;
            end case;

            case readStatus is
               when Ext2.Directory_Page_Complete => null;
               when Ext2.Directory_End => atEnd := True;
               when Ext2.Directory_Malformed =>
                  replyLabel := REPLY_MALFORMED_FILESYSTEM;
               when Ext2.Directory_Device_Error =>
                  replyLabel := REPLY_IO_ERROR;
               when Ext2.Directory_Out_Of_Range =>
                  replyLabel := REPLY_OUT_OF_RANGE;
               when Ext2.Directory_Range_Unsupported =>
                  replyLabel := REPLY_FILE_RANGE_UNSUPPORTED;
            end case;
         end if;

         header.version := PROTOCOL_VERSION;
         header.headerBytes := DIRECTORY_PAGE_HEADER_BYTES;
         header.entryBytes := DIRECTORY_ENTRY_BYTES;
         header.entryCount := Unsigned_16 (entryCount);
         header.flags := (if atEnd then DIRECTORY_PAGE_END else 0);
         header.reserved := 0;
         header.nextCursor := nextCursor;
         header.snapshot :=
           (if files (handle).filesystemKind = CPIO_ARCHIVE then 0 else
              Unsigned_64 (files (handle).directoryInode.generationNumber));
      end;

      returnClientMemory (msg.words (1), msg.words (3), returned);
      if not returned then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      if replyLabel = REPLY_OK then
         files (handle).offset := nextCursor;
      end if;
      sendReply (sender, replyLabel, Unsigned_64 (entryCount));
   end handleReadDirectoryPage;

   procedure handleCloseDirectory (sender : ProcessID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, DIRECTORY_OBJECT);
   begin
      if msg.tag.length /= 1 or else handle < 0 then
         sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
         return;
      end if;
      releaseHandle (handle);
      sendReply (sender, REPLY_OK, 0);
   end handleCloseDirectory;

   --  Deferred reply table for async I/O (Phase 3 foundation)
   MAX_PENDING_CLIENTS : constant := 8;
   type PendingClient is record
      clientPID : ProcessID    := NO_PROCESS;
      handle    : Integer      := -1;
      destAddr  : System.Address := System.Null_Address;
      remaining : Unsigned_64  := 0;
      token     : Unsigned_64  := 0;
      active    : Boolean      := False;
   end record;
   pendingClients : array (0 .. MAX_PENDING_CLIENTS - 1) of PendingClient;

   --  Handle OP_RENAME
   --  words(0) = grant slot (buffer with both paths)
   --  words(1) = old_path_length
   --  words(2) = new_path_length
   --  words(3) = grant generation
   --  Grant buffer layout: [old_path][new_path]
   procedure handleRename (sender : ProcessID; msg : Message) is
      oldPathLen : constant Unsigned_64 := msg.words (1);
      newPathLen : constant Unsigned_64 := msg.words (2);
      grantAddr  : System.Address := System.Null_Address;
      grantOk    : Boolean := False;
      returned   : Boolean := False;
      pathBuffer : String
        (1 .. 2 * Natural (MAXIMUM_PATH_BYTES));
   begin
      if msg.tag.length /= 4 or else
         oldPathLen = 0 or else newPathLen = 0 or else
         oldPathLen > MAXIMUM_PATH_BYTES or else
         newPathLen > MAXIMUM_PATH_BYTES
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (0), msg.words (3), oldPathLen + newPathLen,
         CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
      if not grantOk then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      declare
         totalLen : constant Natural := Natural (oldPathLen + newPathLen);
         grantedPaths : String (1 .. totalLen)
           with Import, Address => grantAddr;
      begin
         pathBuffer (1 .. totalLen) := grantedPaths;
      end;
      returnClientMemory (msg.words (0), msg.words (3), returned);
      if not returned then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      declare
         totalLen : constant Natural :=
           Natural (oldPathLen + newPathLen);
         bothPaths : String renames pathBuffer (1 .. totalLen);
         oldPath : String renames
           bothPaths (1 .. Natural (oldPathLen));
         newPath : String renames
           bothPaths (Natural (oldPathLen) + 1 .. totalLen);

         oldSelection, newSelection : Path_Selection;
         oldVolume, newVolume : Volume_Reference;
         oldRelStart, newRelStart : Natural;
      begin
         if hasTraversal (oldPath) or else hasTraversal (newPath) then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;

         if not checkAccess (sender, oldPath, ACL_WRITE) or else
            not checkAccess (sender, newPath, ACL_WRITE)
         then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;

         Select_Path (Volumes, oldPath, oldSelection, oldVolume, oldRelStart);
         Select_Path (Volumes, newPath, newSelection, newVolume, newRelStart);
         if oldSelection = Unqualified then
            oldVolume := Default_Write_Volume;
         end if;
         if newSelection = Unqualified then
            newVolume := Default_Write_Volume;
         end if;
         if oldSelection in Unknown_Volume | Invalid_Path or else
           newSelection in Unknown_Volume | Invalid_Path
         then
            sendReply (sender, REPLY_NOT_FOUND, 0);
            return;
         elsif oldSelection /= newSelection or else oldVolume /= newVolume then
            sendReply (sender, REPLY_ERR, 0);
            return;
         elsif oldVolume = No_Volume then
            sendReply (sender, REPLY_READ_ONLY, 0);
            return;
         end if;
         declare
            status : Ext2.Rename_Status;
            admission : Admission_Result;
            inodeNum : Unsigned_32;
            lookup : Ext2.Directory_Lookup_Status;
         begin
            ensureVolume (Volume_Index (oldVolume), admission);
            if admission /= Admitted then
               sendReply (sender, REPLY_IO_ERROR, 0);
               return;
            end if;
            --  Do not rename an exclusively held inode. Check identity after
            --  authority, before any mutation;
            --  path spelling and caller PID cannot bypass an exclusive hold.
            Ext2.resolvePath
              (Contexts (Volume_Index (oldVolume)).Fs,
               oldPath (oldRelStart .. oldPath'Last), inodeNum, lookup);
            if lookup not in Ext2.Lookup_Found | Ext2.Lookup_Not_Found then
               sendReply (sender, Lookup_Reply_Label (lookup), 0);
               return;
            elsif lookup = Ext2.Lookup_Found and then Open_Inodes.Exclusively_Held
              (inodeObjects, (Volume_Index (oldVolume), inodeNum))
            then
               sendReply (sender, REPLY_SHARING_VIOLATION, 0);
               return;
            end if;
            --  No source identity means no hold can conflict. Let rename's
            --  own preflight distinguish an unsupported parent layout from
            --  an absent source; failed metadata I/O was already stopped above.
            Ext2.renamePath
              (Contexts (Volume_Index (oldVolume)).Fs,
               oldPath (oldRelStart .. oldPath'Last),
               newPath (newRelStart .. newPath'Last), status);

            declare
               label : Unsigned_32;
            begin
               case status is
                  when Ext2.Rename_Complete => label := REPLY_OK;
                  when Ext2.Rename_Source_Not_Found => label := REPLY_NOT_FOUND;
                  when Ext2.Rename_Destination_Exists => label := REPLY_ALREADY_EXISTS;
                  when Ext2.Rename_Invalid_Name => label := REPLY_ERR;
                  when Ext2.Rename_Malformed => label := REPLY_MALFORMED_FILESYSTEM;
                  when Ext2.Rename_Range_Unsupported =>
                     label := REPLY_FILE_RANGE_UNSUPPORTED;
                  when Ext2.Rename_Read_Only => label := REPLY_READ_ONLY;
                  when Ext2.Rename_Out_Of_Range => label := REPLY_OUT_OF_RANGE;
                  when Ext2.Rename_IO_Error => label := REPLY_IO_ERROR;
                  when Ext2.Rename_Recovery_Required =>
                     label := REPLY_RECOVERY_REQUIRED;
                     debugPrint ("FS Server: rename rollback failed; volume write-quarantined" & LF);
               end case;
               sendReply (sender, label, 0);
            end;
         end;
      end;
   end handleRename;

   ---------------------------------------------------------------------------
   --  [filesystem-journal agent] Namespace operations: unlink, mkdir, rmdir.
   --  Not yet dispatched (no protocol constants); the main session wires
   --  them. Request like OP_OPEN: words 0 = path grant slot, 1 = path
   --  length, 3 = grant generation. Authority: ACL_WRITE and ACL_CREATE on
   --  the path, as for OPEN_CREATE.
   ---------------------------------------------------------------------------

   --  Copy the path, check authority, select and admit its writable volume.
   --  On failure a reply has been sent and ok is False.
   procedure namespaceRequest
     (sender : ProcessID; msg : Message; pathBuffer : out String;
      pathLen : out Natural; volume : out Volume_Index;
      relStart : out Natural; ok : out Boolean)
   is
      rawLength : constant Unsigned_64 := msg.words (1);
      grantAddr : System.Address := System.Null_Address;
      grantOk, returned : Boolean := False;
      selection : Path_Selection;
      reference : Volume_Reference;
      admission : Admission_Result;
   begin
      ok := False;
      pathLen := 0;
      volume := Volume_Index'First;
      relStart := pathBuffer'First;
      if msg.tag.length /= 4 or else rawLength = 0 or else
        rawLength > MAXIMUM_PATH_BYTES or else rawLength > pathBuffer'Length
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      acquireClientMemory
        (sender, msg.words (0), msg.words (3), rawLength,
         CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
      if not grantOk then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;
      pathLen := Natural (rawLength);
      declare
         granted : String (1 .. pathLen) with Import, Address => grantAddr;
      begin
         pathBuffer (pathBuffer'First .. pathBuffer'First + pathLen - 1) := granted;
      end;
      returnClientMemory (msg.words (0), msg.words (3), returned);
      if not returned then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      declare
         path : String renames
           pathBuffer (pathBuffer'First .. pathBuffer'First + pathLen - 1);
      begin
         if hasTraversal (path) then
            sendReply (sender, REPLY_ERR, 0);
            return;
         elsif not checkAccess (sender, path, ACL_WRITE or ACL_CREATE) then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;
         Select_Path (Volumes, path, selection, reference, relStart);
      end;
      if selection = Unqualified then
         reference := Default_Write_Volume;
      end if;
      if selection in Unknown_Volume | Invalid_Path then
         sendReply (sender, REPLY_NOT_FOUND, 0);
         return;
      elsif reference = No_Volume then
         sendReply (sender, REPLY_READ_ONLY, 0);
         return;
      end if;
      volume := Volume_Index (reference);
      ensureVolume (volume, admission);
      if admission /= Admitted then
         sendReply (sender, REPLY_IO_ERROR, 0);
         return;
      end if;
      ok := True;
   end namespaceRequest;

   function replyForRemove (status : Ext2.Remove_Status) return Unsigned_32 is
     (case status is
        when Ext2.Remove_Complete => REPLY_OK,
        when Ext2.Remove_Not_Found => REPLY_NOT_FOUND,
        when Ext2.Remove_Invalid_Name => REPLY_ERR,
        when Ext2.Remove_Wrong_Type => REPLY_WRONG_OBJECT_TYPE,
        --  No distinct "not empty" reply exists yet (see coordination note).
        when Ext2.Remove_Not_Empty => REPLY_ERR,
        when Ext2.Remove_Malformed => REPLY_MALFORMED_FILESYSTEM,
        when Ext2.Remove_Unsupported => REPLY_UNSUPPORTED_OBJECT,
        when Ext2.Remove_Read_Only => REPLY_READ_ONLY,
        when Ext2.Remove_Out_Of_Range => REPLY_OUT_OF_RANGE,
        when Ext2.Remove_IO_Error => REPLY_IO_ERROR,
        when Ext2.Remove_Durability_Unsupported => REPLY_DURABILITY_UNSUPPORTED,
        when Ext2.Remove_Recovery_Required => REPLY_RECOVERY_REQUIRED);

   --  Unlink a regular file. While handles hold it, the name and link go
   --  now and the inode at the last close (releaseHandle).
   procedure handleUnlink (sender : ProcessID; msg : Message) is
      pathBuffer : String (1 .. Natural (MAXIMUM_PATH_BYTES));
      pathLen, relStart : Natural;
      volume : Volume_Index;
      ok : Boolean;
      target, unlinkedNumber : Unsigned_32;
      lookup : Ext2.Directory_Lookup_Status;
      unlinked : Ext2.Inode;
      status : Ext2.Remove_Status;
      holder : Integer := -1;
   begin
      namespaceRequest (sender, msg, pathBuffer, pathLen, volume, relStart, ok);
      if not ok then
         return;
      end if;
      declare
         relPath : String renames pathBuffer (relStart .. pathLen);
      begin
         Ext2.resolvePath (Contexts (volume).Fs, relPath, target, lookup);
         if lookup /= Ext2.Lookup_Found then
            sendReply (sender, Lookup_Reply_Label (lookup), 0);
            return;
         elsif Open_Inodes.Exclusively_Held (inodeObjects, (volume, target)) then
            sendReply (sender, REPLY_SHARING_VIOLATION, 0);
            return;
         end if;
         for j in Handle_Slot loop
            if isFileOf (j, (volume, target)) then
               holder := j;
            end if;
         end loop;
         Ext2.unlinkPath
           (Contexts (volume).Fs, relPath, holder >= 0, unlinkedNumber, unlinked, status);
      end;
      if holder >= 0 and then unlinkedNumber = target then
         --  The name is gone: handles keep a zero-link inode (their writes
         --  must not restore the link), freed at the last close.
         declare
            current : Ext2.Inode :=
              Open_Inodes.Value (inodeObjects, Open_Inodes.Owner_Index (holder));
            recorded : Boolean := False;
         begin
            current.numHardLinks := 0;
            Open_Inodes.Replace
              (inodeObjects, Open_Inodes.Owner_Index (holder), current);
            for orphan of orphans loop
               if not recorded and then orphan.number = 0 then
                  orphan := (volume, target);
                  recorded := True;
               end if;
            end loop;
         end;
      end if;
      sendReply (sender, replyForRemove (status), 0);
   end handleUnlink;

   procedure handleMkdir (sender : ProcessID; msg : Message) is
      pathBuffer : String (1 .. Natural (MAXIMUM_PATH_BYTES));
      pathLen, relStart : Natural;
      volume : Volume_Index;
      ok : Boolean;
      created : Unsigned_32;
      lookup : Ext2.Directory_Lookup_Status;
      status : Ext2.Write_Status;
   begin
      namespaceRequest (sender, msg, pathBuffer, pathLen, volume, relStart, ok);
      if not ok then
         return;
      end if;
      Ext2.makeDirectoryPath
        (Contexts (volume).Fs, pathBuffer (relStart .. pathLen), created, lookup, status);
      if lookup /= Ext2.Lookup_Found then
         sendReply (sender, Lookup_Reply_Label (lookup), 0);
      else
         sendReply (sender, replyForWrite (status), Unsigned_64 (created));
      end if;
   end handleMkdir;

   --  Remove an empty directory. Refused while a directory handle has it
   --  open: its blocks would be freed under the enumeration.
   procedure handleRmdir (sender : ProcessID; msg : Message) is
      pathBuffer : String (1 .. Natural (MAXIMUM_PATH_BYTES));
      pathLen, relStart : Natural;
      volume : Volume_Index;
      ok : Boolean;
      target, removed : Unsigned_32;
      lookup : Ext2.Directory_Lookup_Status;
      status : Ext2.Remove_Status;
   begin
      namespaceRequest (sender, msg, pathBuffer, pathLen, volume, relStart, ok);
      if not ok then
         return;
      end if;
      declare
         relPath : String renames pathBuffer (relStart .. pathLen);
      begin
         Ext2.resolvePath (Contexts (volume).Fs, relPath, target, lookup);
         if lookup /= Ext2.Lookup_Found then
            sendReply (sender, Lookup_Reply_Label (lookup), 0);
            return;
         end if;
         for j in files'Range loop
            if files (j).active and then files (j).objectKind = DIRECTORY_OBJECT and then
              files (j).filesystemKind = EXT2_FILESYSTEM and then
              files (j).volume = volume and then files (j).inodeNum = target
            then
               sendReply (sender, REPLY_SHARING_VIOLATION, 0);
               return;
            end if;
         end loop;
         Ext2.removeDirectoryPath (Contexts (volume).Fs, relPath, removed, status);
      end;
      sendReply (sender, replyForRemove (status), 0);
   end handleRmdir;

   --  Main message loop variables
   sender : ProcessID;
   msg    : Message;
   rdAddr : Unsigned_64;
   rdSize : Unsigned_64;

   ---------------------------------------------------------------------------
   --  handleQueue: a client lends its request queue and transfer arena
   --  (FQ.OP_FS_QUEUE): words 0 = queue grant, 1 = arena grant (wire form),
   --  2 = arena bytes. One queue per process.
   ---------------------------------------------------------------------------
   procedure handleQueue (sender : ProcessID; msg : Message) is
      free : Client_Queue_Count := No_Client_Queue;
      queueRef, arenaRef : CuBit.Grant_References.Reference;
      queueGrant, arenaGrant : CuBit.Memory_Grants.Grant_Reference;
      base, arena : System.Address;
      ok, returned : Boolean;
   begin
      if msg.tag.length /= 3 or else
        not CuBit.Grant_References.Valid_Wire (msg.words (0)) or else
        not CuBit.Grant_References.Valid_Wire (msg.words (1)) or else
        msg.words (2) = 0
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      for q in clientQueues'Range loop
         if clientQueues (q).owner = sender then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;
         if free = No_Client_Queue and then clientQueues (q).owner = NO_PROCESS then
            free := q;
         end if;
      end loop;
      if free = No_Client_Queue then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      queueRef := CuBit.Grant_References.Decode (msg.words (0));
      arenaRef := CuBit.Grant_References.Decode (msg.words (1));
      queueGrant := (slot => queueRef.slot, generation => queueRef.generation);
      arenaGrant := (slot => arenaRef.slot, generation => arenaRef.generation);
      CuBit.Memory_Grants.Acquire
        (queueGrant, sender, 0, FQ.Queue_Bytes,
         CuBit.Memory_Grants.Write_Access, base, ok);
      if not ok then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;
      CuBit.Memory_Grants.Acquire
        (arenaGrant, sender, 0, msg.words (2),
         CuBit.Memory_Grants.Write_Access, arena, ok);
      if not ok then
         CuBit.Memory_Grants.Return_Acquisition (queueGrant, returned);
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;
      clientQueues (free) := (owner      => sender,
                              queueGrant => queueGrant,
                              arenaGrant => arenaGrant,
                              base       => base,
                              arena      => arena,
                              arenaBytes => msg.words (2),
                              server     => <>,
                              waiting    => False);
      sendReply (sender, REPLY_OK, 0);
   end handleQueue;

   --  Take the client's reaped index: its answers' slots are free again.
   procedure acceptReaped (q : Client_Queue_Index) is
      consumed : Unsigned_32 with Volatile, Import,
        Address => queueWord (q, FQ.Completions_At + FQ.Consumed_At);
      ignore : Boolean;
   begin
      FQueues.Accept_Reaped
        (clientQueues (q).server, FQueues.Completions.Index (consumed), ignore);
   end acceptReaped;

   --  WAIT (FQ.OP_FS_WAIT): complete now if answers wait, else when one is
   --  posted.
   procedure handleWait (sender : ProcessID) is
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner = sender then
            acceptReaped (q);
            if clientQueues (q).server.Answers.Fill > 0 or else
              clientQueues (q).waiting
            then
               sendReply (sender, REPLY_OK, 0);
            elsif saveReplyCap (Unsigned_64 (WAIT_REPLY_SLOT_BASE + q)) = 1 then
               clientQueues (q).waiting := True;
            else
               sendReply (sender, REPLY_ERR, 0);
            end if;
            return;
         end if;
      end loop;
      sendReply (sender, REPLY_ERR, 0);
   end handleWait;

   --  Handle one request entry of queue q as its message twin would be.
   procedure dispatchEntry (q : Client_Queue_Index; item : FQueues.Submission) is
      owner : constant ProcessID := clientQueues (q).owner;
      r : FQ.Request renames item.Item;
      m : Message :=
        (tag => (label => 0, length => 0, flags => 0, reserved => 0),
         authorityTag => 0,
         words => [others => 0]);
      inArena : constant Boolean :=
        FQ.In_Arena (r.Arena_Offset, r.Length, clientQueues (q).arenaBytes);
   begin
      curRoute := (queue => q, token => item.Tag);
      if inArena then
         curArena := clientQueues (q).arena + Storage_Offset (r.Arena_Offset);
         curArenaBytes := r.Length;
      end if;
      case r.Operation is
         when FQ.Queue_Open =>
            if inArena then
               m.tag := (label => OP_OPEN, length => 4, flags => 0, reserved => 0);
               m.words := [0 => 0, 1 => r.Length, 2 => Unsigned_64 (r.Options), 3 => 1];
               handleOpen (owner, m);
            else
               sendReply (owner, REPLY_ERR, Unsigned_64'Last);
            end if;
         when FQ.Queue_Read_At | FQ.Queue_Write_At =>
            if inArena then
               m.tag := (label => (if r.Operation = FQ.Queue_Read_At then OP_READ_AT
                                   else OP_WRITE_AT),
                         length => 4, flags => 0, reserved => 0);
               m.words := [0 => r.Handle, 1 => 0, 2 => r.Length, 3 => 1];
               if r.Operation = FQ.Queue_Read_At then
                  handleRead (owner, m, Explicit_Offset, r.Position);
               else
                  handleWrite (owner, m, Explicit_Offset, r.Position);
               end if;
            else
               sendReply (owner, REPLY_ERR, 0);
            end if;
         when FQ.Queue_Close =>
            m.tag := (label => OP_CLOSE, length => 1, flags => 0, reserved => 0);
            m.words (0) := r.Handle;
            handleClose (owner, m);
         when FQ.Queue_Flush =>
            m.tag := (label => OP_FLUSH_FILE, length => 1, flags => 0, reserved => 0);
            m.words (0) := r.Handle;
            handleFlush (owner, m);
         when others =>
            sendReply (owner, REPLY_ERR, 0);
      end case;
      curRoute := (others => <>);
      curArena := System.Null_Address;
      curArenaBytes := 0;
   end dispatchEntry;

   --  Take queue q's requests and handle each.
   procedure serviceQueue (q : Client_Queue_Index) is
      produced : Unsigned_32 with Volatile, Import,
        Address => queueWord (q, FQ.Submissions_At + FQ.Produced_At);
      consumed : Unsigned_32 with Volatile, Import,
        Address => queueWord (q, FQ.Submissions_At + FQ.Consumed_At);
      wake : Unsigned_32 with Volatile, Import,
        Address => queueWord (q, FQ.Submissions_At + FQ.Wake_At);
      requests : FQueues.Submissions.Ring with Import,
        Address => queueWord (q, FQ.Requests_At);
      item : FQueues.Submission;
      ok   : Boolean;
      took : Boolean := False;
   begin
      --  Awake: the client need not kick until the word is armed again.
      if wake /= 0 then
         wake := 0;
      end if;
      acceptReaped (q);
      --  The client's count, read once; one that goes back or overfills the
      --  ring is ignored.
      FQueues.Submissions.Accept_Produced
        (clientQueues (q).server.Requests, FQueues.Submissions.Index (produced), ok);
      if not ok then
         return;
      end if;
      --  The requests were written before the count was: read them after.
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      while FQueues.Can_Take (clientQueues (q).server) loop
         --  A private copy of the entry: what is checked is what is used.
         FQueues.Take (clientQueues (q).server, requests, item);
         took := True;
         dispatchEntry (q, item);
      end loop;
      if took then
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         consumed := Unsigned_32 (clientQueues (q).server.Requests.Consumed);
      end if;
   end serviceQueue;

   --  When a queue last had requests (TSC): the service polls its queues
   --  for a short window after that before it blocks, so back-to-back
   --  requests need no KICK (CuBit.Busy_Poll, as netstack).
   lastQueueActivity : Unsigned_64 := 0;

   function queuesPending return Boolean is
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner /= NO_PROCESS then
            declare
               produced : Unsigned_32 with Volatile, Import,
                 Address => queueWord (q, FQ.Submissions_At + FQ.Produced_At);
            begin
               if FQueues.Submissions.Index (produced) /=
                 clientQueues (q).server.Requests.Consumed
               then
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   end queuesPending;

   procedure serviceClientQueues is
      polling : Boolean := True;
   begin
      while polling loop
         for q in clientQueues'Range loop
            if clientQueues (q).owner /= NO_PROCESS then
               serviceQueue (q);
            end if;
         end loop;
         --  Keep looking while requests keep coming, then briefly after.
         loop
            if queuesPending then
               lastQueueActivity := CuBit.Busy_Poll.Now;
               exit;
            end if;
            if not CuBit.Busy_Poll.Within
              (lastQueueActivity, CuBit.Busy_Poll.Default_Window_Microseconds)
            then
               polling := False;
               exit;
            end if;
            CuBit.Busy_Poll.Relax;
         end loop;
      end loop;
   end serviceClientQueues;

   --  About to block: arm each queue's wake word so its client kicks, then
   --  look once more. True if requests wait: do not block.
   function armClientQueues return Boolean is
      found : Boolean := False;
   begin
      clientQueueEpoch :=
        (if clientQueueEpoch = Unsigned_32'Last then 1 else clientQueueEpoch + 1);
      for q in clientQueues'Range loop
         if clientQueues (q).owner /= NO_PROCESS then
            declare
               produced : Unsigned_32 with Volatile, Import,
                 Address => queueWord (q, FQ.Submissions_At + FQ.Produced_At);
               wake : Unsigned_32 with Volatile, Import,
                 Address => queueWord (q, FQ.Submissions_At + FQ.Wake_At);
            begin
               wake := clientQueueEpoch;
               System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
               if FQueues.Submissions.Index (produced) /=
                 clientQueues (q).server.Requests.Consumed
               then
                  found := True;
               end if;
            end;
         end if;
      end loop;
      return found;
   end armClientQueues;

begin
   debugPrint ("FS Server: Starting..." & LF);

   --  Get ramdisk address and size from kernel
   rdAddr := getInfo (SYSINFO_RAMDISK_ADDRESS);
   rdSize := getInfo (SYSINFO_RAMDISK_SIZE);
   if rdAddr = 0 or rdAddr = Unsigned_64'Last then
      debugPrint ("FS Server: No ramdisk found, disk-only mode." & LF);
      cpioOk := False;
   else
      debugPrint ("FS Server: Got ramdisk, initializing CPIO..." & LF);

      Cpio.init (cpioArchive, toAddr (rdAddr), rdSize, cpioOk);
      if not cpioOk then
         debugPrint ("FS Server: Invalid CPIO archive on ramdisk." & LF);
      else
         if Cpio.findFile (cpioArchive, "live-rw.ext2") < cpioArchive.count then
            declare
               Result : Registration_Result;
               admission : Admission_Result;
            begin
               Register
                 (Volumes, "mem:0",
                  (Endpoint => CAP_SLOT_RAMDISK, Ready_Role => 0, Transfer_Pages => 128),
                  Default_Write_Volume, Result);
               if Result = Registered then
                  ensureVolume (Volume_Index (Default_Write_Volume), admission);
                  if admission = Admitted then
                     debugPrint ("FS Server: Writable memory filesystem ready." & LF);
                  end if;
               end if;
            end;
         end if;
      end if;
   end if;

   --  Bootstrap bindings only. File operations never test these driver roles.
   declare
      Volume : Volume_Reference;
      Result : Registration_Result;
   begin
      Register (Volumes, "ata:0",
        (Endpoint => CAP_SLOT_ATA, Ready_Role => DRIVER_ATA, Transfer_Pages => 8),
        Volume, Result);
      if Result /= Registered then
         debugPrint ("FS Server: volume list initialization failed." & LF);
         return;
      end if;
      Register (Volumes, "nvme:0",
        (Endpoint => CAP_SLOT_NVME, Ready_Role => DRIVER_NVME, Transfer_Pages => 128),
        Volume, Result);
      if Result /= Registered then
         debugPrint ("FS Server: volume list initialization failed." & LF);
         return;
      end if;
   end;

   --  Register as DRIVER_FS so other services can discover us
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_FS);
   end;

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      ignore : MessageTag;
   begin
      ignore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, reserved => 0),
          authorityTag => 0,
          words    => (others => 0)));
   end;

   CuBit.Busy_Poll.Calibrate;   --  queue polling windows are timed by the TSC
   debugPrint ("FS Server: Entering message loop." & LF);

   --  Main IPC message loop.
   --  Uses blocking receive for lowest latency.  Future async work
   --  can switch to Poll_Service_Request + Poll_Completion when capSubmit-based
   --  driver I/O is implemented.
   loop
      --  Queued requests first; block only when none wait.
      serviceClientQueues;
      if not armClientQueues then
      receive (sender, msg);

      case msg.tag.label is
         when FQ.OP_FS_QUEUE =>
            handleQueue (sender, msg);
         when FQ.OP_FS_KICK =>
            null;   --  one-way: the queue is serviced at the loop's top
         when FQ.OP_FS_WAIT =>
            handleWait (sender);
         when OP_OPEN =>
            handleOpen (sender, msg);
         when OP_READ =>
            handleRead (sender, msg);
         when OP_WRITE =>
            handleWrite (sender, msg);
         when OP_READ_AT | OP_WRITE_AT =>
            handlePositioned (sender, msg);
         when OP_SEEK =>
            handleSeek (sender, msg);
         when OP_CLOSE =>
            handleClose (sender, msg);
         when OP_FLUSH_FILE =>
            handleFlush (sender, msg);
         when OP_RESIZE_FILE =>
            handleResize (sender, msg);
         when OP_OPEN_DIRECTORY =>
            handleOpenDirectory (sender, msg);
         when OP_READ_DIRECTORY_PAGE =>
            handleReadDirectoryPage (sender, msg);
         when OP_CLOSE_DIRECTORY =>
            handleCloseDirectory (sender, msg);
         when OP_OPEN_CHILD_DIRECTORY =>
            handleOpenChildDirectory (sender, msg);
         when OP_REWIND_DIRECTORY =>
            handleRewindDirectory (sender, msg);
         when OP_SET_ACL =>
            handleSetACL (sender, msg);
         when OP_REVOKE_ACL =>
            handleRevokeACL (sender, msg);
         when OP_RENAME =>
            handleRename (sender, msg);
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
      end if;
   end loop;
end main;

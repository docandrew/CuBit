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
with CuBit.Channel_Contracts;
with CuBit.Protocols;
with CuBit.Channel_Protocol;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Busy_Poll;
with CuBit.Grant_References;
with CuBit.Directory_Paths;
with CuBit.Directory_Pages;
with CuBit.File_Access;
with Cpio;
with Ext2;
with Ext2_Support;
with ISO_Records;
with ISO9660;
with Shared_Objects;
with Volume_List; use Volume_List;
with Volume_Admission; use Volume_Admission;
with Dirty_Runs;
with Queue_Wakes;
with Watch_Reserve;
with CuBit.Filesystem_Events;
with CuBit.Channel_Rings;
with CuBit.Volume_Descriptions;
with CuBit.Block_Devices;
with Copy_Slices;

procedure main is
   use ASCII;
   use type Ext2.Read_Status;
   use type Ext2.Write_Status;
   use type Ext2.Truncate_Status;
   use type Ext2.Flush_Status;
   use type Ext2.Directory_Lookup_Status;
   use type Ext2.Remove_Status;

   --  Sysinfo query for ramdisk address
   --  (uses SYSINFO_RAMDISK_ADDRESS from CuBit.Messages)

   --  Maximum open files and path length. Handle slots below
   --  EXTENDED_HANDLES also carry directory and optical-file state
   --  (extras); regular files use the rest, so a large table costs a few
   --  words per slot.
   MAX_OPEN_FILES : constant := 2_048;
   --  Handles one client may hold on one file at once (of the file's
   --  Open_Inodes.Max_Holders).
   MAX_HANDLES_PER_OWNER : constant := 8;
   EXTENDED_HANDLES : constant := 64;

   --  Filesystem format is independent of the block provider.
   type Filesystem_Kind is (CPIO_ARCHIVE, ISO_FILESYSTEM, EXT2_FILESYSTEM);

   type Inode_Identity is record
      volume : Volume_Index;
      number : Unsigned_32;
   end record;
   function identityHash (Key : Inode_Identity) return Unsigned_32 is
     (Key.number * 16#9E37_79B9# xor Unsigned_32 (Key.volume) * 16#85EB_CA6B#);
   package Open_Inodes is new Shared_Objects
     (Capacity => MAX_OPEN_FILES, Object_Key => Inode_Identity,
      Empty_Key => (Volume_Index'First, 0), Object_Value => Ext2.Inode,
      Empty_Value => Ext2.NULL_INODE, Hash => identityHash);
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
      cpioFileIdx : Natural      := 0;      --  cpio only
      offset      : Unsigned_64  := 0;
      ownerPID    : Process_ID    := No_Process;
      openRights  : Unsigned_8   := 0;
      objectKind  : Open_Object_Kind := FILE_OBJECT;
   end record;

   type FileTable is array (0 .. MAX_OPEN_FILES - 1) of FileEntry;
   files : FileTable;

   --  Directory and optical-file handles (slots below EXTENDED_HANDLES).
   type Extra_Entry is record
      directoryInode : Ext2.Inode; -- Directory enumeration snapshot only.
      opticalFile : ISO_Records.File_Record;
      directoryPath : CuBit.Directory_Paths.Path;
   end record;
   extras : array (0 .. EXTENDED_HANDLES - 1) of Extra_Entry;

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
      pid    : Process_ID := No_Process;
      active : Boolean   := False;
      policy : CuBit.File_Access.Policy;
   end record;

   aclProfiles : array (0 .. MAX_ACL_PROFILES - 1) of ACLProfile;

   --  Administrative identity comes only from the kernel's authenticated
   --  service registry. Query it for each rare policy operation so a cached
   --  raw PID cannot become authority after process death and PID reuse.
   function isAdmin (sender : Process_ID) return Boolean is
      devmgrAdmin : constant Process_ID :=
        Registered_Driver (DRIVER_DEVMGR);
      procmgrAdmin : constant Process_ID :=
        Registered_Driver (DRIVER_PROCMGR);
   begin
      return
        (devmgrAdmin /= No_Process and then
         sender = devmgrAdmin) or else
        (procmgrAdmin /= No_Process and then
         sender = procmgrAdmin);
   end isAdmin;

   --  Check if sender has access rights for the given path.  A scope prefix
   --  must end at a path-component boundary: authority for "apps/foo" must
   --  not also authorize "apps/foobar".
   function checkAccess
     (sender : Process_ID;
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
   --  Plain file handles are sought from a cursor over the slots above
   --  EXTENDED_HANDLES (found at once while the table is not nearly full);
   --  extended ones (directories, optical files) in the slots below.
   plainCursor : Natural range EXTENDED_HANDLES .. MAX_OPEN_FILES - 1 := EXTENDED_HANDLES;

   procedure allocHandle
     (handle  : out Unsigned_64;
      slot    : out Integer;
      success : out Boolean;
      extended : Boolean := False)
   is
      i : Natural;
   begin
      if extended then
         for j in 0 .. EXTENDED_HANDLES - 1 loop
            if not files (j).active and then not files (j).retired then
               slot := j;
               handle := Shift_Left (Unsigned_64 (files (j).generation), 32) or
                 Unsigned_64 (j + 1);
               success := True;
               return;
            end if;
         end loop;
      else
         for step in EXTENDED_HANDLES .. MAX_OPEN_FILES - 1 loop
            i := plainCursor;
            plainCursor := (if plainCursor = MAX_OPEN_FILES - 1 then EXTENDED_HANDLES
                            else plainCursor + 1);
            if not files (i).active and then not files (i).retired then
               slot := i;
               handle := Shift_Left (Unsigned_64 (files (i).generation), 32) or
                 Unsigned_64 (i + 1);
               success := True;
               return;
            end if;
         end loop;
      end if;

      handle := 0;
      slot := -1;
      success := False;
   end allocHandle;

   function resolveHandle
     (handle : Unsigned_64;
      sender : Process_ID;
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
   orphanCount : Natural range 0 .. MAX_OPEN_FILES := 0;

   --  Defined with the version table below.
   procedure bumpVersion (key : Inode_Identity);

   procedure reclaimOrphan (key : Inode_Identity) is
      status : Ext2.Remove_Status;
   begin
      if orphanCount = 0 or else Open_Inodes.Object_Of (inodeObjects, key) /= 0 then
         return;   --  nothing unlinked is held, or this file still is
      end if;
      for orphan of orphans loop
         if orphan.number /= 0 and then orphan = key then
            orphan := NO_ORPHAN;
            orphanCount := orphanCount - 1;
            Ext2.reclaimInode (Contexts (key.volume).Fs, key.number, status);
            --  The number may name another file next: no cached page of
            --  this one may be taken for it.
            bumpVersion (key);
            if status /= Ext2.Remove_Complete then
               debugPrint ("FS Server: unlinked inode not reclaimed: " &
                           Ext2.Remove_Status'Image (status) & LF);
            end if;
         end if;
      end loop;
   end reclaimOrphan;

   --  Take back slot's delegation (harvesting a write delegation's pages).
   procedure dropDelegation (slot : Natural);
   --  A write-back failure of a released handle, kept for its owner's next
   --  flush or close of the file (defined with writebackFailed below).
   procedure keepWriteError (slot : Natural);
   --  Change notifications for written files (defined with the watches):
   --  slot's file changed; slot is going (report and forget its path).
   procedure noteModified (slot : Integer);
   procedure forgetNoticePath (slot : Natural);
   --  Server-side copies (defined with them): slot is going; a copy that
   --  reads or writes it ends (REPLY_ERR, with the prefix it copied).
   procedure copyHandleGone (slot : Natural);

   procedure releaseHandle (slot : Integer) is
      --  [filesystem-journal agent] last-close reclaim of an unlinked file.
      heldFile : constant Boolean :=
        files (slot).active and then files (slot).objectKind = FILE_OBJECT and then
        files (slot).filesystemKind = EXT2_FILESYSTEM;
      key : constant Inode_Identity := (files (slot).volume, files (slot).inodeNum);
   begin
      --  A client must never keep using a released handle's delegation
      --  (a parked handle, say, after its owner's access was revoked).
      dropDelegation (slot);
      keepWriteError (slot);
      if files (slot).objectKind = FILE_OBJECT then
         forgetNoticePath (slot);
         copyHandleGone (slot);
      end if;
      Open_Inodes.Detach (inodeObjects, Open_Inodes.Owner_Index (slot));
      files (slot).active := False;
      files (slot).ownerPID := No_Process;
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

   procedure releaseHandlesForOwner (owner : Process_ID) is
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
   --  Saved reply capabilities for held wake requests (FQ.OP_FS_WAKE), one
   --  per queue.
   WAKE_REPLY_SLOT_BASE : constant := 40;

   --  A client's queue: three channels it opened (FQ: the queue pair, the
   --  transfer arena, the dirty arena), docs/data-plane.md.
   type Client_Queue is record
      owner      : Process_ID := No_Process;   --  set once the queue pair is open
      queueLink  : CuBit.Channels.Channel;
      transferLink : CuBit.Channels.Channel;
      dirtyLink  : CuBit.Channels.Channel;
      clientBase : System.Address := System.Null_Address;   --  its region, read-only
      serverBase : System.Address := System.Null_Address;   --  ours
      arena      : System.Address := System.Null_Address;
      arenaBytes : Unsigned_64 := 0;
      server     : FQueues.Server;
      wake       : Queue_Wakes.State := Queue_Wakes.Idle;   --  a held wake request
      --  The event ring (FQ.Event_Connector), if the client opened one, and
      --  how many of its watches are Watching (Watch_Reserve).
      eventLink  : CuBit.Channels.Channel;
      watching   : Watch_Reserve.Normal_Count := 0;
      --  The dirty arena (FQ.Dirty_*), if the client lent one.
      dirty      : System.Address := System.Null_Address;
   end record;
   clientQueues : array (Client_Queue_Index) of Client_Queue;
   --  Arenas a client lent before its queue pair: taken in when it opens.
   type Pending_Arenas is record
      owner : Process_ID := No_Process;
      transferLink, dirtyLink : CuBit.Channels.Channel;
   end record;
   --  A transfer arena of the queue's layout, of any size up to the
   --  largest (a client that lists directories lends a small one).
   function transferArena (offered : CuBit.Channel_Contracts.Contract) return Boolean is
     (CuBit.Protocols."=" (offered.Element.Identity, FQ.TRANSFER_CONTRACT.Element.Identity)
      and then CuBit.Protocols."=" (offered.Element.Version, FQ.TRANSFER_CONTRACT.Element.Version)
      and then CuBit.Channel_Contracts."=" (offered.Kind, FQ.TRANSFER_CONTRACT.Kind)
      and then offered.Pages = FQ.TRANSFER_CONTRACT.Pages
      and then offered.Buffers <= FQ.Transfer_Pages);
   pendingArenas : array (Client_Queue_Index) of Pending_Arenas;
   clientQueueEpoch : Unsigned_32 := 0;
   --  When a queue last had requests (TSC): the service polls its queues
   --  for a short window after that before it blocks, so back-to-back
   --  requests need no KICK (CuBit.Busy_Poll, as netstack).
   lastQueueActivity : Unsigned_64 := 0;

   --  While a queue entry is handled: where its answer goes, and the arena
   --  range it names (checked against the arena before it is set).
   type Reply_Route is record
      queue : Client_Queue_Count := No_Client_Queue;
      token : FQueues.Token := 0;
   end record;
   curRoute : Reply_Route;
   curArena : System.Address := System.Null_Address;
   curArenaBytes : Unsigned_64 := 0;

   --  A word of the client's region (it writes them) or of ours.
   function clientWord (q : Client_Queue_Index; offset : Natural) return System.Address is
     (clientQueues (q).clientBase + Storage_Offset (offset));
   function serverWord (q : Client_Queue_Index; offset : Natural) return System.Address is
     (clientQueues (q).serverBase + Storage_Offset (offset));

   --  The namespace generation (FQ.Server_Namespace_At), published in
   --  every client's queue page. It moves on before any change that could
   --  alter what a name resolves to, or whether a client may open it, so a
   --  client never reuses a parked handle across such a change.
   namespaceGeneration : Unsigned_32 := 1;

   procedure publishNamespace (q : Client_Queue_Index) is
      word : Unsigned_32 with Volatile, Import,
        Address => serverWord (q, FQ.Server_Namespace_At);
   begin
      word := namespaceGeneration;
   end publishNamespace;

   --  owner: only that client's access changes (its policy), so only its
   --  queue needs the new value; No_Process: every client's.
   procedure bumpNamespace (owner : Process_ID := No_Process) is
   begin
      namespaceGeneration :=
        (if namespaceGeneration = Unsigned_32'Last then 1 else namespaceGeneration + 1);
      for q in clientQueues'Range loop
         if clientQueues (q).serverBase /= System.Null_Address and then
           (owner = No_Process or else clientQueues (q).owner = owner)
         then
            publishNamespace (q);
         end if;
      end loop;
      --  Published before the change it announces takes effect.
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end bumpNamespace;

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
   --  The opener's policy lets it read the file, whatever the handle's own
   --  rights: the handle may be parked for a later read-only open of the
   --  same name (FQ.Queue_Park), which then gives it read rights alone.
   parkable : array (Handle_Slot) of Boolean := [others => False];

   --  Versions of files seen during this service's life. An entry reused
   --  for another file gives the old one a fresh version when it is seen
   --  again, so no client keeps pages of an older version.
   --  Entries are found by hashing the identity to a home entry and probing
   --  a short window after it; a miss takes a free entry of the window, or
   --  evicts one round-robin.
   MAX_INODE_VERSIONS : constant := 1024;
   VERSION_PROBES     : constant := 8;
   subtype Version_Slot is Natural range 0 .. MAX_INODE_VERSIONS - 1;
   subtype Version_Probe is Natural range 0 .. VERSION_PROBES - 1;
   type Inode_Version is record
      used    : Boolean := False;
      key     : Inode_Identity := (Volume_Index'First, 0);
      version : Unsigned_64 := 0;
   end record;
   inodeVersions : array (Version_Slot) of Inode_Version;
   versionClock : Unsigned_64 := 0;
   nextVersionProbe : Version_Probe := Version_Probe'First;

   --  Fibonacci hashing: the top bits of number * 2**32 / golden ratio.
   GOLDEN_RATIO_32 : constant := 16#9E37_79B9#;
   VERSION_HASH_SHIFT : constant := 32 - 10;
   pragma Compile_Time_Error
     (2 ** (32 - VERSION_HASH_SHIFT) /= MAX_INODE_VERSIONS,
      "the version hash must cover the table exactly");

   function versionHome (key : Inode_Identity) return Version_Slot is
     (Version_Slot (Shift_Right
        ((key.number xor Unsigned_32 (key.volume)) * GOLDEN_RATIO_32,
         VERSION_HASH_SHIFT)));

   function versionSlotOf (key : Inode_Identity) return Version_Slot is
      home  : constant Version_Slot := versionHome (key);
      fresh : Version_Slot := (home + nextVersionProbe) mod MAX_INODE_VERSIONS;
   begin
      for p in Version_Probe loop
         declare
            i : constant Version_Slot := (home + p) mod MAX_INODE_VERSIONS;
         begin
            if inodeVersions (i).used and then inodeVersions (i).key = key then
               return i;
            end if;
         end;
      end loop;
      for p in Version_Probe loop
         if not inodeVersions ((home + p) mod MAX_INODE_VERSIONS).used then
            fresh := (home + p) mod MAX_INODE_VERSIONS;
            exit;
         end if;
      end loop;
      nextVersionProbe := (if nextVersionProbe = Version_Probe'Last
                           then Version_Probe'First else nextVersionProbe + 1);
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

   function queueOf (owner : Process_ID) return Client_Queue_Count is
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner = owner then
            return q;
         end if;
      end loop;
      return No_Client_Queue;
   end queueOf;


   --  Grant (publish the file's identity, version and size, then the valid
   --  word) or revoke (clear the valid word) slot's delegation in its
   --  owner's queue page.
   writeDelegated : array (Handle_Slot) of Boolean := [others => False];
   --  A write-back of the handle's dirty pages failed: reported by its
   --  next flush or close (as Linux reports write-back errors on fsync).
   writebackFailed : array (Handle_Slot) of Boolean := [others => False];

   --  Write-back failures of released handles (errseq-like): reported by
   --  the owner's next flush or close of the same file, never dropped. A
   --  full table falls back to the owner's next flush or close of any file.
   type Write_Error is record
      used  : Boolean := False;
      owner : Process_ID := No_Process;
      key   : Inode_Identity := (Volume_Index'First, 0);
   end record;
   MAX_WRITE_ERRORS : constant := 64;
   writeErrors : array (0 .. MAX_WRITE_ERRORS - 1) of Write_Error;
   MAX_ERROR_OWNERS : constant := 16;
   overflowOwners : array (0 .. MAX_ERROR_OWNERS - 1) of Process_ID :=
     [others => No_Process];

   procedure keepWriteError (slot : Natural) is
      kept : Boolean := False;
   begin
      if not writebackFailed (slot) then
         return;
      end if;
      writebackFailed (slot) := False;
      for e of writeErrors loop
         if not kept and then not e.used then
            e := (used => True, owner => files (slot).ownerPID,
                  key => (files (slot).volume, files (slot).inodeNum));
            kept := True;
         end if;
      end loop;
      for o of overflowOwners loop
         if not kept and then (o = No_Process or else o = files (slot).ownerPID) then
            o := files (slot).ownerPID;
            kept := True;
         end if;
      end loop;
      if not kept then
         --  Nowhere left to keep it: say so rather than lose it silently.
         debugPrint ("FS: write-back failure not kept for its owner" & LF);
      end if;
   end keepWriteError;

   --  Take (and clear) a kept failure of owner for key.
   procedure takeWriteError (owner : Process_ID; key : Inode_Identity; found : out Boolean) is
   begin
      found := False;
      for e of writeErrors loop
         if e.used and then e.owner = owner and then e.key = key then
            e := (others => <>);
            found := True;
         end if;
      end loop;
      for o of overflowOwners loop
         if o = owner then
            o := No_Process;
            found := True;
         end if;
      end loop;
   end takeWriteError;

   --  An exited owner's kept failures have no one left to report to.
   procedure forgetWriteErrors (owner : Process_ID) is
   begin
      for e of writeErrors loop
         if e.used and then e.owner = owner then
            e := (others => <>);
         end if;
      end loop;
      for o of overflowOwners loop
         if o = owner then
            o := No_Process;
         end if;
      end loop;
   end forgetWriteErrors;

   procedure delegate (slot : Handle_Slot; grant : Boolean; write : Boolean := False) is
      q : constant Client_Queue_Count := queueOf (files (slot).ownerPID);
   begin
      delegated (slot) := grant and then q /= No_Client_Queue;
      writeDelegated (slot) := delegated (slot) and then write and then
        clientQueues (q).dirty /= System.Null_Address;
      if q = No_Client_Queue then
         return;
      end if;
      declare
         entryAt : constant Natural := FQ.Delegations_At + slot * FQ.Delegation_Bytes;
         valid : Unsigned_32 with Volatile, Import,
           Address => serverWord (q, entryAt + FQ.Delegation_Valid_At);
         mode : Unsigned_32 with Volatile, Import,
           Address => serverWord (q, entryAt + FQ.Delegation_Mode_At);
         inode : Unsigned_64 with Volatile, Import,
           Address => serverWord (q, entryAt + FQ.Delegation_Inode_At);
         version : Unsigned_64 with Volatile, Import,
           Address => serverWord (q, entryAt + FQ.Delegation_Version_At);
         size : Unsigned_64 with Volatile, Import,
           Address => serverWord (q, entryAt + FQ.Delegation_Size_At);
         key : constant Inode_Identity := (files (slot).volume, files (slot).inodeNum);
      begin
         valid := 0;
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         if grant then
            --  Writes buffered under a write delegation reach the file
            --  without fileChanging: the version moves on now, so pages
            --  another client cached for the file are not taken again.
            if writeDelegated (slot) then
               bumpVersion (key);
            end if;
            inode := Shift_Left (Unsigned_64 (key.volume), 32) or Unsigned_64 (key.number);
            version := inodeVersions (versionSlotOf (key)).version;
            size := Ext2.fileSize
              (Open_Inodes.Value (inodeObjects, Open_Inodes.Owner_Index (slot)));
            mode := (if writeDelegated (slot) then FQ.Write_Delegation
                     else FQ.Read_Delegation);
            System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
            valid := 1;
         end if;
      end;
   end delegate;

   ---------------------------------------------------------------------------
   --  Harvesting a write delegation's dirty pages (FQ.Dirty_*): copy each
   --  stable entry of the handle out of the client's dirty arena, write
   --  contiguous runs to the file, and free the entries. The table is the
   --  client's: every field is checked, and an entry is used only if its
   --  sequence word is even and unchanged across the copy.
   ---------------------------------------------------------------------------
   --  Planned by Dirty_Runs (proved): items, file order, contiguous runs.
   HARVEST_BUFFER_BYTES : constant := Dirty_Runs.Buffer_Bytes;
   --  Yields one harvest may spend waiting out entries the client is
   --  writing (odd sequence words), in total: an entry still odd after the
   --  budget is left for a later harvest. A client cannot hold the service.
   HARVEST_WAIT_BUDGET  : constant := 64;
   pragma Compile_Time_Error
     (MAX_OPEN_FILES > 2 ** FQ.Tag_Slot_Bits, "a dirty-entry tag must hold every slot");
   TAG_SLOT_MASK : constant Unsigned_32 := 2 ** FQ.Tag_Slot_Bits - 1;
   TAG_GENERATION_MASK : constant Unsigned_32 := 2 ** FQ.Tag_Generation_Bits - 1;
   pragma Compile_Time_Error
     (Dirty_Runs.Maximum_Items /= FQ.Dirty_Entries or else
      Dirty_Runs.Page_Bytes /= FQ.Dirty_Page_Bytes,
      "Dirty_Runs must match the dirty arena");
   subtype Dirty_Index is Dirty_Runs.Item_Index;
   harvestItems : Dirty_Runs.Item_Array;
   type Byte_Buffer is array (0 .. HARVEST_BUFFER_BYTES - 1) of Unsigned_8;
   harvestBuffer : Byte_Buffer with Alignment => FQ.Dirty_Page_Bytes;

   --  Atomically replace expected at addr with desired; True if it was so.
   function compareAndSwap
     (addr : System.Address; expected, desired : Unsigned_32) return Boolean
   is
      previous : Unsigned_32;
   begin
      System.Machine_Code.Asm
        ("lock cmpxchgl %2, (%1)",
         Outputs  => Unsigned_32'Asm_Output ("=a", previous),
         Inputs   => [System.Address'Asm_Input ("r", addr),
                      Unsigned_32'Asm_Input ("r", desired),
                      Unsigned_32'Asm_Input ("a", expected)],
         Clobber  => "memory",
         Volatile => True);
      return previous = expected;
   end compareAndSwap;

   --  Entries found in one scan of the arena, listed per handle slot
   --  (links are entry index + 1; 0 ends a list).
   type Entry_Links is array (Dirty_Index) of Natural;
   type Slot_Links is array (Handle_Slot) of Natural;
   type Slot_List is array (0 .. MAX_OPEN_FILES - 1) of Handle_Slot;
   nextOfEntry : Entry_Links := [others => 0];
   firstOfSlot : Slot_Links := [others => 0];
   scannedSlots : Slot_List := [others => 0];

   --  Harvest the entries of queue q's dirty arena that belong to slot
   --  single, or (single < 0) to any of the queue owner's write-delegated
   --  handles: one scan of the entries either way.
   --  slot's dirty-entry tag (FQ.Dirty_Slot_At) for its current handle.
   function tagOf (slot : Handle_Slot) return Unsigned_32 is
     (Shift_Left (files (slot).generation and TAG_GENERATION_MASK, FQ.Tag_Slot_Bits) or
      Unsigned_32 (slot));

   --  The live handle of owner that tag names, or -1: an entry with any
   --  other tag is stale (its handle released) or forged.
   function taggedHandle (tag : Unsigned_32; owner : Process_ID) return Integer is
      slot : constant Handle_Slot := Handle_Slot (tag and TAG_SLOT_MASK);
   begin
      if files (slot).active and then files (slot).ownerPID = owner and then
        tagOf (slot) = tag
      then
         return slot;
      end if;
      return -1;
   end taggedHandle;

   procedure harvestEntries (q : Client_Queue_Index; single : Integer) is
      owner : constant Process_ID := clientQueues (q).owner;
      base : constant System.Address := clientQueues (q).dirty;
      slotCount : Natural := 0;
      waitBudget : Natural := HARVEST_WAIT_BUDGET;

      function entryWord (i : Dirty_Index; field : Natural) return System.Address is
        (base + Storage_Offset (i * FQ.Dirty_Entry_Bytes + field));

      --  A live handle's entry this harvest takes.
      function wanted (handle : Integer) return Boolean is
        (handle >= 0 and then
         (if single >= 0 then handle = single
          else writeDelegated (handle) and then
               files (handle).filesystemKind = EXT2_FILESYSTEM));

      --  Write harvestBuffer (0 .. length - 1) at offset through slot; then
      --  free the items first .. last whose words did not move (none if
      --  last < first).
      procedure writeRun (slot : Handle_Slot; offset : Unsigned_64; length : Natural;
                          first, last : Integer) is
         currentInode : Ext2.Inode := Open_Inodes.Value
           (inodeObjects, Open_Inodes.Owner_Index (slot));
         written : Unsigned_64;
         status : Ext2.Write_Status;
      begin
         if length = 0 or else first < 0 or else last < first then
            return;
         end if;
         Ext2.writeData
           (Contexts (files (slot).volume).Fs, files (slot).inodeNum,
            currentInode, offset, harvestBuffer'Address, Unsigned_64 (length),
            written, status);
         Open_Inodes.Replace
           (inodeObjects, Open_Inodes.Owner_Index (slot), currentInode);
         noteModified (slot);
         if status /= Ext2.Write_Complete or else written /= Unsigned_64 (length) then
            writebackFailed (slot) := True;
         end if;
         for k in first .. last loop
            if compareAndSwap (entryWord (harvestItems (k).Entry_Index, FQ.Dirty_Sequence_At),
                               harvestItems (k).Sequence, 0)
            then
               null;   --  freed; a word that moved holds newer data, taken later
            end if;
         end loop;
      end writeRun;

      --  2. slot's items in file order, then 3. contiguous runs within the
      --  buffer, each copied, checked against the client's words, and written.
      procedure writeSlot (slot : Handle_Slot) is
         count : Natural := 0;
         link  : Natural := firstOfSlot (slot);
         first : Natural := 0;
         last  : Dirty_Index;
         bytes : Natural;
      begin
         while link /= 0 loop
            declare
               i : constant Dirty_Index := link - 1;
               startWord : Unsigned_16 with Volatile, Import,
                 Address => entryWord (i, FQ.Dirty_Start_At);
               stopWord : Unsigned_16 with Volatile, Import,
                 Address => entryWord (i, FQ.Dirty_Stop_At);
               seqWord : Unsigned_32 with Volatile, Import,
                 Address => entryWord (i, FQ.Dirty_Sequence_At);
               pageWord : Unsigned_32 with Volatile, Import,
                 Address => entryWord (i, FQ.Dirty_Page_At);
               sequence : constant Unsigned_32 := seqWord;
               page : constant Unsigned_32 := pageWord;
               start : constant Unsigned_16 := startWord;
               stop  : constant Unsigned_16 := stopWord;
            begin
               if sequence /= 0 and then sequence mod 2 = 0 and then
                 start < stop and then Natural (stop) <= FQ.Dirty_Page_Bytes
               then
                  harvestItems (count) :=
                    (Entry_Index => i, Sequence => sequence, Page => page,
                     Start => Natural (start), Stop => Natural (stop));
                  count := count + 1;
               end if;
               link := nextOfEntry (i);
            end;
         end loop;
         firstOfSlot (slot) := 0;
         Dirty_Runs.Sort (harvestItems, count);
         while first < count loop
            Dirty_Runs.Next_Run (harvestItems, count, first, last, bytes);
            declare
               length : Natural := 0;
               kept   : Integer := first - 1;   --  items copied intact
            begin
               for k in first .. last loop
                  declare
                     item : constant Dirty_Runs.Item := harvestItems (k);
                     n : constant Natural := Dirty_Runs.Length (item);
                     seqWord : Unsigned_32 with Volatile, Import,
                       Address => entryWord (item.Entry_Index, FQ.Dirty_Sequence_At);
                     source : Byte_Buffer with Import,
                       Address => base + Storage_Offset
                         (FQ.Dirty_Pages_At + item.Entry_Index * FQ.Dirty_Page_Bytes);
                  begin
                     harvestBuffer (length .. length + n - 1) :=
                       source (item.Start .. item.Stop - 1);
                     System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
                     --  Rewritten while copied: stop here; it is taken later.
                     exit when seqWord /= item.Sequence;
                     length := length + n;
                     kept := k;
                  end;
               end loop;
               writeRun (slot, Dirty_Runs.Offset (harvestItems (first)), length, first, kept);
            end;
            first := last + 1;
         end loop;
      end writeSlot;
   begin
      if base = System.Null_Address then
         return;
      end if;
      --  1. The wanted entries, listed per slot. Every field is the
      --  client's: copied, then checked against the service's own table.
      --  An entry being written (odd) is waited for within one bounded
      --  budget per harvest, else left for a later one. A stable entry
      --  whose tag names no live handle of this client is freed unwritten.
      for i in reverse Dirty_Index loop
         declare
            seqWord : Unsigned_32 with Volatile, Import,
              Address => entryWord (i, FQ.Dirty_Sequence_At);
            slotWord : Unsigned_32 with Volatile, Import,
              Address => entryWord (i, FQ.Dirty_Slot_At);
            sequence : Unsigned_32 := seqWord;
            ignore : Unsigned_64;
         begin
            if sequence /= 0 then
               while sequence mod 2 = 1 and then waitBudget > 0 and then
                 wanted (taggedHandle (slotWord, owner))
               loop
                  ignore := syscall (SYSCALL_YIELD);
                  waitBudget := waitBudget - 1;
                  sequence := seqWord;
               end loop;
               System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
               declare
                  tag : constant Unsigned_32 := slotWord;
                  handle : constant Integer := taggedHandle (tag, owner);
               begin
                  if sequence = 0 or else sequence mod 2 = 1 then
                     null;   --  free, or still being written: a later harvest
                  elsif handle < 0 then
                     if compareAndSwap (seqWord'Address, sequence, 0) then
                        null;   --  stale: its handle is gone
                     end if;
                  elsif wanted (handle) then
                     if firstOfSlot (handle) = 0 then
                        scannedSlots (slotCount) := handle;
                        slotCount := slotCount + 1;
                     end if;
                     nextOfEntry (i) := firstOfSlot (handle);
                     firstOfSlot (handle) := i + 1;
                  end if;
               end;
            end if;
         end;
      end loop;
      for k in 0 .. slotCount - 1 loop
         writeSlot (scannedSlots (k));
      end loop;
   end harvestEntries;

   procedure harvest (slot : Handle_Slot) is
      q : constant Client_Queue_Count := queueOf (files (slot).ownerPID);
   begin
      if q /= No_Client_Queue and then clientQueues (q).dirty /= System.Null_Address and then
        files (slot).filesystemKind = EXT2_FILESYSTEM
      then
         harvestEntries (q, slot);
      end if;
   end harvest;

   --  Every write-delegated handle of queue q's owner, in one scan.
   procedure harvestOwner (q : Client_Queue_Index) is
   begin
      harvestEntries (q, -1);
   end harvestOwner;

   --  Take back slot's write delegation: clear its valid word, then harvest.
   procedure recallWrite (slot : Handle_Slot) is
   begin
      if writeDelegated (slot) then
         delegate (slot, False);
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         harvest (slot);
      end if;
   end recallWrite;

   --  The file key is about to change through slot except: every other
   --  handle's delegation goes first, and the version moves on.
   procedure fileChanging (key : Inode_Identity; except : Integer) is
      object : constant Open_Inodes.Link := Open_Inodes.Object_Of (inodeObjects, key);
   begin
      --  The file's handles are its object's holders (no scan of the table).
      if object /= 0 then
         for i in 0 .. Open_Inodes.Holding (inodeObjects, object) - 1 loop
            declare
               j : constant Handle_Slot := Open_Inodes.Holder (inodeObjects, object, i);
            begin
               if j /= except and then delegated (j) then
                  if writeDelegated (j) then
                     recallWrite (j);
                  else
                     delegate (j, False);
                  end if;
               end if;
            end;
         end loop;
      end if;
      bumpVersion (key);
      noteModified (except);
   end fileChanging;

   procedure dropDelegation (slot : Natural) is
   begin
      if slot <= Handle_Slot'Last and then delegated (slot) then
         if writeDelegated (slot) then
            recallWrite (slot);
         else
            delegate (slot, False);
         end if;
      end if;
   end dropDelegation;

   --  After slot's own change: its delegation, if held, carries the new
   --  version and size.
   procedure refreshDelegation (slot : Handle_Slot) is
   begin
      if delegated (slot) then
         --  Kept in its mode: a write delegation stays one (its client
         --  goes on buffering, and its entries stay the service's to take).
         delegate (slot, True, write => writeDelegated (slot));
      end if;
   end refreshDelegation;

   --  A handle was opened: a writable open revokes the others; the handle
   --  is delegated if no other handle can write the file.
   procedure handleOpened (slot : Handle_Slot) is
      object : constant Open_Inodes.Link :=
        Open_Inodes.Object_Of_Owner (inodeObjects, Open_Inodes.Owner_Index (slot));
      holders : constant Natural :=
        (if object = 0 then 0 else Open_Inodes.Holding (inodeObjects, object));
      otherWriter : Boolean := False;
      sole : Boolean := True;
   begin
      --  The file's other handles are its object's holders.
      for i in 0 .. holders - 1 loop
         declare
            j : constant Handle_Slot := Open_Inodes.Holder (inodeObjects, object, i);
         begin
            if j /= slot then
               sole := False;
               --  Its buffered writes reach the file before this open answers.
               if writeDelegated (j) then
                  recallWrite (j);
               elsif (files (slot).openRights and ACL_WRITE) /= 0 and then delegated (j) then
                  delegate (j, False);
               end if;
               if (files (j).openRights and ACL_WRITE) /= 0 then
                  otherWriter := True;
               end if;
            end if;
         end;
      end loop;
      if not otherWriter then
         delegate (slot, True,
                   write => sole and then (files (slot).openRights and ACL_WRITE) /= 0);
      end if;
   end handleOpened;

   --  Answer queue q's held wake request (its saved reply capability is
   --  consumed whether or not the client still waits for it).
   procedure answerWake (q : Client_Queue_Index; label : Unsigned_32) is
      ignore : Unsigned_64;
   begin
      ignore := replyCap
        (CapabilitySlot (WAKE_REPLY_SLOT_BASE + q),
         (tag => (label => label, length => 0, flags => 0, reserved => 0),
          authorityTag => 0, words => [others => 0]));
   end answerWake;

   --  Answer the entry being handled on its queue, and answer the client's
   --  held wake request, if any.
   procedure answerEntry
     (label : Unsigned_32; word0, word1 : Unsigned_64; rights : Unsigned_32 := 0)
   is
      q : constant Client_Queue_Count := curRoute.queue;
   begin
      if q = No_Client_Queue or else clientQueues (q).server.Owed = 0 then
         return;
      end if;
      declare
         ring : FQueues.Completions.Ring with Import,
           Address => serverWord (q, FQ.Server_Answers_At);
         produced : Unsigned_32 with Volatile, Import,
           Address => serverWord (q, FQ.Server_Answered_At);
         wakeHeld : Boolean;
      begin
         --  Owed <= Space (FQueues.Valid): the answer has its slot.
         FQueues.Complete
           (clientQueues (q).server, ring, curRoute.token,
            (Status => label, Reserved => rights, Value => word0, Spare => word1));
         --  The answer is written before the count that hands it over.
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         produced := Unsigned_32 (clientQueues (q).server.Answers.Produced);
         Queue_Wakes.Posted (clientQueues (q).wake, wakeHeld);
         if wakeHeld then
            answerWake (q, REPLY_OK);
         end if;
      end;
   end answerEntry;

   ---------------------------------------------------------------------------
   --  Change notifications (docs/filesystem-protocol-v2.md step 4): a
   --  client's watches, each naming a folder of a volume by its path, and
   --  the records they produce in the client's event ring. Paths are
   --  matched canonically: relative to the volume, without empty
   --  components. Room in the ring is Watch_Reserve's: an event that would
   --  not fit becomes the watch's Rescan_Needed, never a silent loss.
   ---------------------------------------------------------------------------
   package FE renames CuBit.Filesystem_Events;
   package WR renames Watch_Reserve;
   use type WR.Watch_State, WR.Decision, FE.Event_Kind;

   type Watch_Record is record
      state   : WR.Watch_State := WR.Unused;
      subtree : Boolean := False;
      volume  : Volume_Index := Volume_Index'First;
      rel     : String (1 .. Natural (MAXIMUM_PATH_BYTES)) := [others => ' '];
      relLength : Natural range 0 .. Natural (MAXIMUM_PATH_BYTES) := 0;
      --  The ring's producer index after this watch's last Rescan_Needed
      --  or its Watch_Ended (Ended: the number is free once read past it).
      rescanMark : CuBit.Channel_Rings.Index := 0;
   end record;
   watches : array (Client_Queue_Index, FE.Watch_Number) of Watch_Record;
   --  Shared by a rename's two records.
   renameCookie : Unsigned_64 := 0;

   --  Path without empty components (no leading, trailing or doubled '/').
   function canonical (path : String) return String is
      result : String (1 .. path'Length) := [others => ' '];
      used : Natural := 0;
   begin
      for c of path loop
         if c /= '/' or else (used > 0 and then result (used) /= '/') then
            used := used + 1;
            result (used) := c;
         end if;
      end loop;
      if used > 0 and then result (used) = '/' then
         used := used - 1;
      end if;
      return result (1 .. used);
   end canonical;

   --  A volume's full name for policy checks ("@name/rel").
   function fullName (volume : Volume_Index; rel : String) return String is
     ("@" & Volume_List.Name (Volumes, volume) & "/" & rel);

   --  Records the client has not read yet.
   function eventsWaiting (q : Client_Queue_Index) return Boolean is
      ignore : Natural;
   begin
      if not clientQueues (q).eventLink.Active then
         return False;
      end if;
      ignore := CuBit.Channels.Free_Bytes (clientQueues (q).eventLink);   --  learns the index
      return clientQueues (q).eventLink.Writer.Fill > 0;
   end eventsWaiting;

   --  What the reserve may count as free.
   function ringFree (q : Client_Queue_Index) return WR.Free_Bytes is
     (Natural'Min (CuBit.Channels.Free_Bytes (clientQueues (q).eventLink), WR.Maximum_Free));

   function rescanRead (q : Client_Queue_Index; w : FE.Watch_Number) return Boolean is
     (WR.Reached (watches (q, w).rescanMark,
                  CuBit.Channel_Rings.Consumed (clientQueues (q).eventLink.Writer),
                  clientQueues (q).eventLink.Writer.Fill));

   --  Put one record in q's ring (room was checked: Watch_Reserve).
   procedure putRecord
     (q : Client_Queue_Index; item : FE.Event; name : FE.Name_Bytes; length : FE.Name_Length)
   is
      bytes : FE.Record_Bytes;
      used : FE.Record_Length;
      result : CuBit.Channels.Put_Result;
   begin
      FE.Encode (item, name, length, bytes, used);
      CuBit.Channels.Put (clientQueues (q).eventLink, bytes'Address, used, result);
      if item.Kind in FE.Rescan_Needed | FE.Watch_Ended then
         watches (q, item.Watch).rescanMark := clientQueues (q).eventLink.Writer.Produced;
      end if;
   end putRecord;

   procedure putRescan (q : Client_Queue_Index; w : FE.Watch_Number) is
   begin
      putRecord (q, (Watch => w, Kind => FE.Rescan_Needed, Flags => 0, Object => 0, Cookie => 0,
                     Stamp => Unsigned_64 (namespaceGeneration)),
                 [others => 0], 0);
   end putRescan;

   procedure putEnded (q : Client_Queue_Index; w : FE.Watch_Number) is
   begin
      putRecord (q, (Watch => w, Kind => FE.Watch_Ended, Flags => 0, Object => 0, Cookie => 0,
                     Stamp => Unsigned_64 (namespaceGeneration)),
                 [others => 0], 0);
   end putEnded;

   --  Answer q's held wake: records wait.
   procedure eventsPosted (q : Client_Queue_Index) is
      wakeHeld : Boolean;
   begin
      Queue_Wakes.Posted (clientQueues (q).wake, wakeHeld);
      if wakeHeld then
         answerWake (q, REPLY_OK);
      end if;
   end eventsPosted;

   --  Whether watch w of queue q still sees changes.
   function live (q : Client_Queue_Index; w : FE.Watch_Number) return Boolean is
     (watches (q, w).state in WR.Live_State);

   --  A live watch w of q ends (unwatched, or its folder went): its
   --  Watch_Ended goes in now, or as soon as there is room (End_Owed).
   procedure endWatch (q : Client_Queue_Index; w : FE.Watch_Number) is
      put : Boolean;
   begin
      if live (q, w) and then WR.Holds (ringFree (q), clientQueues (q).watching) then
         WR.End_Watch (watches (q, w).state, clientQueues (q).watching, ringFree (q), put);
         if put then
            putEnded (q, w);
            eventsPosted (q);
         end if;
      end if;
   end endWatch;

   --  One event for live watch w of queue q, or its Rescan_Needed.
   procedure offer
     (q : Client_Queue_Index; w : FE.Watch_Number; item : FE.Event; name : String)
   is
      bytes : FE.Name_Bytes := [others => 0];
      decision : WR.Decision;
      free : constant WR.Free_Bytes := ringFree (q);
   begin
      if name'Length > FE.Maximum_Name_Bytes then
         return;
      end if;
      for i in name'Range loop
         bytes (i - name'First + 1) := Unsigned_8 (Character'Pos (name (i)));
      end loop;
      if not live (q, w) or else
        (FE.Named (item.Kind) and then not FE.Valid_Relative (bytes, name'Length)) or else
        not WR.Holds (free, clientQueues (q).watching)
      then
         return;
      end if;
      WR.Decide (watches (q, w).state, clientQueues (q).watching, free,
                 FE.Header_Bytes + (if FE.Named (item.Kind) then name'Length else 0),
                 rescanRead (q, w), decision);
      case decision is
         when WR.Put_Event =>
            putRecord (q, item, bytes, (if FE.Named (item.Kind) then name'Length else 0));
         when WR.Put_Rescan =>
            putRescan (q, w);
         when WR.Drop =>
            null;
      end case;
      if decision /= WR.Drop then
         eventsPosted (q);
      end if;
   end offer;

   --  A change to path (relative to volume) of kind: every watch that sees
   --  it gets a record, if its owner's policy lets it read that path.
   procedure notify
     (volume : Volume_Index; path : String; kind : FE.Event_Kind; isDirectory : Boolean;
      object : Unsigned_64; cookie : Unsigned_64 := 0)
   is
      rel : constant String := canonical (path);
      parentEnd : Natural := 0;
      flags : constant Unsigned_8 := (if isDirectory then FE.Is_Directory else 0);
   begin
      if rel'Length = 0 then
         return;
      end if;
      for i in reverse rel'Range loop
         if rel (i) = '/' then
            parentEnd := i;
            exit;
         end if;
      end loop;
      for q in clientQueues'Range loop
         if clientQueues (q).owner /= No_Process and then clientQueues (q).eventLink.Active then
            for w in FE.Watch_Number loop
               declare
                  watch : Watch_Record renames watches (q, w);
                  root : constant String := watch.rel (1 .. watch.relLength);
                  item : constant FE.Event :=
                    (Watch => w, Kind => kind, Flags => flags, Object => object, Cookie => cookie,
                     Stamp => Unsigned_64 (namespaceGeneration));
               begin
                  if live (q, w) and then watch.volume = volume then
                     if kind in FE.Removed | FE.Renamed_From and then isDirectory and then
                       (root = rel or else
                        (root'Length > rel'Length and then
                         root (1 .. rel'Length) = rel and then root (rel'Length + 1) = '/'))
                     then
                        --  The watched folder itself, or a folder above it:
                        --  the watch named it by its path.
                        endWatch (q, w);
                     elsif not checkAccess (clientQueues (q).owner, fullName (volume, rel), ACL_READ) then
                        null;
                     elsif not watch.subtree then
                        if (parentEnd = 0 and then root'Length = 0) or else
                          (parentEnd > 0 and then rel (rel'First .. parentEnd - 1) = root)
                        then
                           offer (q, w, item, rel (parentEnd + 1 .. rel'Last));
                        end if;
                     elsif root'Length = 0 then
                        offer (q, w, item, rel);
                     elsif rel'Length > root'Length + 1 and then
                       rel (rel'First .. rel'First + root'Length - 1) = root and then
                       rel (rel'First + root'Length) = '/'
                     then
                        offer (q, w, item, rel (rel'First + root'Length + 1 .. rel'Last));
                     end if;
                  end if;
               end;
            end loop;
         end if;
      end loop;
   end notify;

   --  A change in volume the service cannot name: its watches rescan.
   procedure notifyUnnamed (volume : Volume_Index) is
      put : Boolean;
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner /= No_Process and then clientQueues (q).eventLink.Active then
            for w in FE.Watch_Number loop
               if live (q, w) and then watches (q, w).volume = volume and then
                 WR.Holds (ringFree (q), clientQueues (q).watching)
               then
                  WR.Force_Rescan (watches (q, w).state, clientQueues (q).watching, ringFree (q),
                                   rescanRead (q, w), put);
                  if put then
                     putRescan (q, w);
                     eventsPosted (q);
                  end if;
               end if;
            end loop;
         end if;
      end loop;
   end notifyUnnamed;

   --  Each pass: watches whose Rescan_Needed was read watch again, and owed
   --  records (Rescan_Needed, Watch_Ended) go in when there is room.
   procedure settleWatches is
      put : Boolean;
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner /= No_Process and then clientQueues (q).eventLink.Active then
            for w in FE.Watch_Number loop
               if watches (q, w).state in WR.Rescan_Posted | WR.Rescan_Owed | WR.End_Owed and then
                 WR.Holds (ringFree (q), clientQueues (q).watching)
               then
                  WR.Settle (watches (q, w).state, clientQueues (q).watching, ringFree (q),
                             rescanRead (q, w), put);
                  if put then
                     if watches (q, w).state = WR.Ended then
                        putEnded (q, w);
                     else
                        putRescan (q, w);
                     end if;
                     eventsPosted (q);
                  end if;
               end if;
            end loop;
         end if;
      end loop;
   end settleWatches;


   ---------------------------------------------------------------------------
   --  Modified events: a handle that may write keeps the path it was opened
   --  by (a bounded table: past it, the change is reported unnamed), and a
   --  written handle reports Modified once at its close, flush, park or the
   --  periodic commit.
   ---------------------------------------------------------------------------
   MAX_NOTICE_PATHS : constant := 128;
   subtype Notice_Slot is Natural range 0 .. MAX_NOTICE_PATHS;
   type Notice_Path is record
      used : Boolean := False;
      rel  : String (1 .. Natural (MAXIMUM_PATH_BYTES)) := [others => ' '];
      relLength : Natural range 0 .. Natural (MAXIMUM_PATH_BYTES) := 0;
   end record;
   noticePaths : array (1 .. MAX_NOTICE_PATHS) of Notice_Path;
   noticeOf : array (files'Range) of Notice_Slot := [others => 0];
   modifiedPending : array (files'Range) of Boolean := [others => False];
   --  A writable handle whose path could not be kept.
   pathUnknown : array (files'Range) of Boolean := [others => False];

   procedure keepNoticePath (slot : Natural; rel : String) is
      kept : constant String := canonical (rel);
   begin
      noticeOf (slot) := 0;
      pathUnknown (slot) := True;
      modifiedPending (slot) := False;
      for n in noticePaths'Range loop
         if not noticePaths (n).used then
            noticePaths (n).used := True;
            noticePaths (n).relLength := kept'Length;
            noticePaths (n).rel (1 .. kept'Length) := kept;
            noticeOf (slot) := n;
            pathUnknown (slot) := False;
            return;
         end if;
      end loop;
   end keepNoticePath;

   procedure noteModified (slot : Integer) is
   begin
      if slot in files'Range and then (noticeOf (slot) /= 0 or else pathUnknown (slot)) then
         modifiedPending (slot) := True;
      end if;
   end noteModified;

   procedure reportModified (slot : Natural) is
   begin
      if modifiedPending (slot) then
         modifiedPending (slot) := False;
         if noticeOf (slot) /= 0 then
            notify (files (slot).volume,
                    noticePaths (noticeOf (slot)).rel (1 .. noticePaths (noticeOf (slot)).relLength),
                    FE.Modified, False,
                    Shift_Left (Unsigned_64 (files (slot).volume), 32) or Unsigned_64 (files (slot).inodeNum));
         else
            notifyUnnamed (files (slot).volume);
         end if;
      end if;
   end reportModified;

   procedure forgetNoticePath (slot : Natural) is
   begin
      reportModified (slot);
      if noticeOf (slot) /= 0 then
         noticePaths (noticeOf (slot)).used := False;
      end if;
      noticeOf (slot) := 0;
      pathUnknown (slot) := False;
   end forgetNoticePath;

   --  A rename moved old to new: kept paths at or below old follow it.
   procedure renameNoticePaths (before, after : String) is
      from : constant String := canonical (before);
      to : constant String := canonical (after);
   begin
      for n of noticePaths loop
         if n.used then
            declare
               kept : constant String := n.rel (1 .. n.relLength);
            begin
               if kept = from then
                  n.relLength := to'Length;
                  n.rel (1 .. to'Length) := to;
               elsif kept'Length > from'Length and then kept (1 .. from'Length) = from and then
                 kept (from'Length + 1) = '/' and then
                 to'Length + kept'Length - from'Length <= n.rel'Length
               then
                  declare
                     moved : constant String := to & kept (from'Length + 1 .. kept'Last);
                  begin
                     n.relLength := moved'Length;
                     n.rel (1 .. moved'Length) := moved;
                  end;
               end if;
            end;
         end if;
      end loop;
   end renameNoticePaths;

   procedure acquireClientMemory
     (sender        : Process_ID;
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
     (dest   : Process_ID;
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
   procedure handleSetACL (sender : Process_ID; msg : Message) is
      targetPID     : constant Process_ID := From_Word (msg.words (0));
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
      --  Policy for the target changes: none of its parked handles may
      --  be reused past this point.
      bumpNamespace (targetPID);
      --  A policy change takes back every delegation the client holds
      --  (buffered writes are written first): from here its handles write
      --  and read only through the service, under the rights each was
      --  opened with, and a parked handle can no longer buffer writes.
      for slot in Handle_Slot loop
         if files (slot).active and then files (slot).ownerPID = targetPID then
            dropDelegation (slot);
         end if;
      end loop;

      if msg.tag.length /= 4 or else targetPID = No_Process or else
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
            Unsigned_64 (entryCount * CuBit.File_Access.Wire_Entry_Bytes),
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
            buf : CuBit.File_Access.Wire_Bytes (1 .. entryCount * CuBit.File_Access.Wire_Entry_Bytes)
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
   procedure handleRevokeACL (sender : Process_ID; msg : Message) is
      targetPID : constant Process_ID := From_Word (msg.words (0));
   begin
      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;
      bumpNamespace (targetPID);

      for i in aclProfiles'Range loop
         if aclProfiles (i).active and then
            aclProfiles (i).pid = targetPID
         then
            aclProfiles (i).active := False;
            aclProfiles (i).pid    := No_Process;
            CuBit.File_Access.Clear (aclProfiles (i).policy);
         end if;
      end loop;

      releaseHandlesForOwner (targetPID);

      sendReply (sender, REPLY_OK, 0);
   end handleRevokeACL;

   --  Return a client queue's grants and free its entry.
   procedure releaseClientQueue (owner : Process_ID);

   --  OP_RELEASE_OWNER (procmgr, once a process has exited): nothing it
   --  held survives to be found through a reused PID. Its handles are
   --  released (a write delegation's dirty pages are harvested first: the
   --  service's acquisition keeps the arena's frames), then its queue and
   --  its access profile go.
   procedure handleReleaseOwner (sender : Process_ID; msg : Message) is
      targetPID : constant Process_ID := From_Word (msg.words (0));
   begin
      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      elsif msg.tag.length /= 1 or else targetPID = No_Process then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      declare
         held : Natural := 0;
      begin
         for f of files loop
            if f.active and then f.ownerPID = targetPID then
               held := held + 1;
            end if;
         end loop;
         if held > 0 or else queueOf (targetPID) /= No_Client_Queue then
            debugPrint ("FS: released exited process" & targetPID'Image & ":" &
                        held'Image & " handles" &
                        (if queueOf (targetPID) /= No_Client_Queue then ", its queue"
                         else "") & LF);
         end if;
      end;
      releaseHandlesForOwner (targetPID);
      forgetWriteErrors (targetPID);
      releaseClientQueue (targetPID);
      for i in aclProfiles'Range loop
         if aclProfiles (i).active and then aclProfiles (i).pid = targetPID then
            aclProfiles (i).active := False;
            aclProfiles (i).pid    := No_Process;
            CuBit.File_Access.Clear (aclProfiles (i).policy);
         end if;
      end loop;
      sendReply (sender, REPLY_OK, 0);
   end handleReleaseOwner;

   FS_PAGE_SIZE : constant := 4096;

   --  Generic endpoint admission. The registry role is only a boot-order hint;
   --  grant creation is authorized by the endpoint already installed by devmgr.
   procedure ensureVolume (Volume : Volume_Index; Result : out Admission_Result) is
      Device : constant Device_Binding := Binding (Volumes, Volume);
      Context : Volume_Context renames Contexts (Volume);
      Grant_OK, Revoked : Boolean;
      Provider : Process_ID;
      Raw : Unsigned_64;
   begin
      Result := Context.Status;
      if Context.Status /= Provider_Not_Ready then
         return;
      end if;
      if Device.Ready_Role /= 0 then
         Provider := Registered_Driver (Device.Ready_Role);
         if Provider = No_Process then
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
      bumpNamespace;   --  names on this volume resolve anew
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
   procedure handleOpen (sender : Process_ID; msg : Message) is
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
      mayRead : Boolean := False;
      mayWrite : Boolean := False;
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

            --  Read too if the policy allows it (one check in the common
            --  case): the handle may then be parked (FQ.Queue_Park).
            if checkAccess (sender, pathStr, requiredRights or ACL_READ) then
               mayRead := True;
            elsif not checkAccess (sender, pathStr, requiredRights) then
               debugPrint ("FS: access denied for PID" & Process_ID'Image (sender)
                           & ": " & pathStr & LF);
               sendReply (sender, REPLY_ACCESS_DENIED, Unsigned_64'Last);
               return;
            end if;
            --  Whether the policy would let it write (access(W_OK)).
            mayWrite := (requiredRights and ACL_WRITE) /= 0 or else
              checkAccess (sender, pathStr, ACL_WRITE);
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
         elsif selection = Boot_Archive then
            --  "@boot/<name>": the bootstrap archive only.
            if cpioOk then
               cpioIdx := Cpio.findFile
                 (cpioArchive, pathStr (relStart .. pathStr'Last));
               if cpioIdx < cpioArchive.count then
                  selectedKind := CPIO_ARCHIVE;
                  inodeNum := 1;
               end if;
            end if;
         elsif selection = Optical_Volume then
            --  "@cd:0/<name>": the optical disc only.
            ISO9660.Find
              (pathStr (relStart .. pathStr'Last), opticalFile, opticalFound);
            if opticalFound then
               selectedKind := ISO_FILESYSTEM;
               inodeNum := 1;
            elsif ISO9660.Media_Failed then
               sendReply (sender, REPLY_IO_ERROR, 0);
               return;
            end if;
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
      allocHandle (handleId, handle, allocated,
                   extended => selectedKind = ISO_FILESYSTEM);
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
                  --  A new file under a number an old one may have had.
                  bumpVersion ((useVolume, inodeNum));
                  notify (useVolume, relPath, FE.Created, False,
                          Shift_Left (Unsigned_64 (useVolume), 32) or Unsigned_64 (inodeNum));
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
         --  One client may hold at most MAX_HANDLES_PER_OWNER of a file's
         --  Max_Holders handles, parked ones included: no one principal can
         --  exhaust a shared file.
         declare
            object : constant Open_Inodes.Link :=
              Open_Inodes.Object_Of (inodeObjects, (useVolume, inodeNum));
            mine : Natural := 0;
         begin
            if object /= 0 then
               for i in 0 .. Open_Inodes.Holding (inodeObjects, object) - 1 loop
                  if files (Open_Inodes.Holder (inodeObjects, object, i)).ownerPID = sender then
                     mine := mine + 1;
                  end if;
               end loop;
            end if;
            if mine >= MAX_HANDLES_PER_OWNER then
               sendReply (sender, REPLY_SHARING_VIOLATION, 0);
               return;
            end if;
         end;
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
      if handle < EXTENDED_HANDLES then
         extras (handle).opticalFile := opticalFile;
      end if;
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
      --  The handle keeps the rights asked for (a write-only handle never
      --  reads); the answer tells the client whether it may be parked.
      parkable (handle) := selectedKind = EXT2_FILESYSTEM and then mayRead;
      if selectedKind = EXT2_FILESYSTEM then
         handleOpened (handle);
         --  Watches see what it writes (Modified at close, flush, park or
         --  the commit), a truncation included.
         if Requests_Write (openFlags) then
            keepNoticePath (handle, pathBuffer (relStart .. Natural (pathLen)));
            modifiedPending (handle) := (openFlags and OPEN_TRUNCATE) /= 0;
         end if;
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
            answerEntry
              (REPLY_OK, handleId, fsize,
               rights => (if parkable (handle) then FQ.Rights_Read else 0) or
                         (if (files (handle).openRights and ACL_WRITE) /= 0
                          then FQ.Rights_Write else 0) or
                         (if mayWrite and then selectedKind = EXT2_FILESYSTEM
                          then FQ.Rights_Policy_Write else 0));
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
     (sender : Process_ID; msg : Message;
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
         if writeDelegated (handle) then
            harvest (handle);   --  the file holds what the client wrote
         end if;
         currentInode := Open_Inodes.Value
           (inodeObjects, Open_Inodes.Owner_Index (handle));
      end if;
      case files (handle).filesystemKind is
         when ISO_FILESYSTEM =>
            declare
               ok : Boolean;
            begin
               ISO9660.Read (extras (handle).opticalFile, transferOffset,
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
     (sender : Process_ID; msg : Message;
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
            --  The client's bytes are copied into service memory first, a
            --  buffer at a time: what reaches the disk and the cache is one
            --  copy the client can no longer change.
            bytesWritten := 0;
            while bytesWritten < count and then writeStatus = Ext2.Write_Complete loop
               declare
                  part : constant Unsigned_64 := Unsigned_64'Min
                    (count - bytesWritten, Unsigned_64 (HARVEST_BUFFER_BYTES));
                  source : Byte_Buffer with Import,
                    Address => grantAddr + Storage_Offset (bytesWritten);
                  done : Unsigned_64;
               begin
                  harvestBuffer (0 .. Natural (part) - 1) := source (0 .. Natural (part) - 1);
                  Ext2.writeData
                    (Contexts (files (handle).volume).Fs,
                     files (handle).inodeNum,
                     currentInode,
                     transferOffset + bytesWritten,
                     harvestBuffer'Address,
                     part,
                     done,
                     writeStatus);
                  bytesWritten := bytesWritten + Unsigned_64'Min (done, part);
                  exit when done < part;
               end;
            end loop;
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
   procedure handlePositioned (sender : Process_ID; msg : Message) is
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
   procedure handleSeek (sender : Process_ID; msg : Message) is
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
            size := Unsigned_64 (extras (handle).opticalFile.Bytes);
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
   procedure handleClose (sender : Process_ID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
   begin
      if handle < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if delegated (handle) then
         recallWrite (handle);
         delegate (handle, False);
      end if;
      declare
         failed : constant Boolean := writebackFailed (handle);
      begin
         --  A queued close is not waited for: its failure is kept for the
         --  owner's next flush of the file (releaseHandle) instead.
         if curRoute.queue = No_Client_Queue then
            writebackFailed (handle) := False;
         end if;
         releaseHandle (handle);
         sendReply (sender, (if failed then REPLY_IO_ERROR else REPLY_OK), 0);
      end;
   end handleClose;

   --  PARK (FQ.Queue_Park): the client keeps a closed handle to reuse for a
   --  later read-only open of the same name. Its rights drop to reading; it
   --  keeps its delegation (or is delegated as a new reader would be). A
   --  handle that cannot read, or is not an ext2 file, is closed instead.
   procedure handlePark (sender : Process_ID; handleWord : Unsigned_64) is
      handle : constant Integer := resolveHandle (handleWord, sender, FILE_OBJECT);
   begin
      if handle < 0 then
         sendReply (sender, REPLY_ERR, 0);
      elsif handle > Handle_Slot'Last or else not parkable (handle) then
         writebackFailed (handle) := False;
         releaseHandle (handle);
         sendReply (sender, REPLY_ERR, 0);
      else
         --  A write delegation stays: its buffered pages are harvested
         --  later (write-back, the commit, another open, release), as
         --  Linux's page cache writes back after close. The handle can
         --  no longer write through the service.
         files (handle).openRights := ACL_READ;
         if not delegated (handle) then
            handleOpened (handle);
         end if;
         reportModified (handle);
         sendReply (sender, REPLY_OK, 0);
      end if;
   end handlePark;

   procedure handleFlush (sender : Process_ID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
      status : Ext2.Flush_Status;
   begin
      if msg.tag.length /= 1 or else msg.tag.flags /= 0 or else
        msg.tag.reserved /= 0 or else handle < 0
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      --  Any handle may flush (fsync of a read-only descriptor, as Linux):
      --  a flush writes back and commits, it changes no data.
      if writeDelegated (handle) then
         harvest (handle);
      end if;
      reportModified (handle);
      case files (handle).filesystemKind is
         when CPIO_ARCHIVE | ISO_FILESYSTEM =>
            status := Ext2.Flush_Unsupported;
         when EXT2_FILESYSTEM =>
            Ext2.Flush (Contexts (files (handle).volume).Fs, status);
      end case;
      declare
         kept : Boolean;
      begin
         takeWriteError (sender, (files (handle).volume, files (handle).inodeNum), kept);
         if kept then
            status := Ext2.Flush_IO_Error;
         end if;
      end;
      if writebackFailed (handle) then
         writebackFailed (handle) := False;
         status := Ext2.Flush_IO_Error;
      end if;
      case status is
         when Ext2.Flush_Complete => sendReply (sender, REPLY_OK, 0);
         when Ext2.Flush_Unsupported =>
            sendReply (sender, REPLY_DURABILITY_UNSUPPORTED, 0);
         when Ext2.Flush_IO_Error => sendReply (sender, REPLY_IO_ERROR, 0);
         when Ext2.Flush_Recovery_Required =>
            sendReply (sender, REPLY_RECOVERY_REQUIRED, 0);
      end case;
   end handleFlush;

   procedure handleResize (sender : Process_ID; msg : Message) is
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
      if writeDelegated (handle) then
         harvest (handle);
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
   procedure handleOpenDirectory (sender : Process_ID; msg : Message) is
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

      mayCreate : Boolean := False;
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
            --  Whether the policy would let it create here (access(W_OK)).
            mayCreate := checkAccess (sender, path, ACL_WRITE or ACL_CREATE);
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
           (selection = Unqualified and then onlySeparators) or else
           (selection = Boot_Archive and then
              (for all index in relStart .. path'Last => path (index) = '/'))
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

      allocHandle (handleId, handleSlot, allocated, extended => True);
      if not allocated then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      files (handleSlot).active := True;
      files (handleSlot).filesystemKind := filesystemKind;
      files (handleSlot).volume := selectedVolume;
      files (handleSlot).inodeNum := inodeNum;
      if filesystemKind /= CPIO_ARCHIVE then
         extras (handleSlot).directoryInode := dirIno;
      end if;
      files (handleSlot).offset := 0;
      files (handleSlot).ownerPID := sender;
      files (handleSlot).openRights := ACL_READ;
      files (handleSlot).objectKind := DIRECTORY_OBJECT;
      CuBit.Directory_Paths.Set_Root
        (pathBuffer (1 .. Natural (pathLen)),
         extras (handleSlot).directoryPath, allocated);

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
         if curRoute.queue /= No_Client_Queue then
            answerEntry
              (REPLY_OK, replyMsg.words (0), replyMsg.words (1),
               rights => (if mayCreate and then filesystemKind = EXT2_FILESYSTEM
                          then FQ.Rights_Policy_Write else 0));
         else
            ignored := reply (sender, replyMsg);
         end if;
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

   procedure handleOpenChildDirectory (sender : Process_ID; msg : Message) is
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
        (extras (parent).directoryPath,
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
      allocHandle (identity, slot, ok, extended => True);
      if not ok then
         sendReply (sender, REPLY_NO_SPACE, 0);
         return;
      end if;
      files (slot).filesystemKind := files (parent).filesystemKind;
      files (slot).volume := files (parent).volume;
      files (slot).inodeNum := inodeNum;
      extras (slot).directoryInode := ino;
      files (slot).offset := 0;
      files (slot).ownerPID := sender;
      files (slot).openRights := ACL_READ;
      files (slot).objectKind := DIRECTORY_OBJECT;
      extras (slot).directoryPath := childPath;
      files (slot).active := True;
      sendReply (sender, REPLY_OK, identity);
   end handleOpenChildDirectory;

   procedure handleRewindDirectory (sender : Process_ID; msg : Message) is
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
         extras (handle).directoryInode := candidate;
      end if;
      files (handle).offset := 0;
      sendReply (sender, REPLY_OK, 0);
   end handleRewindDirectory;

   --  What an ext2 inode records, as Directory.Inspection.V1. Ext2 keeps
   --  seconds since the epoch; i_ctime is the change time.
   function inspectionOf
     (ino : Ext2.Inode; volume : Volume_Index; inodeNum : Unsigned_32)
      return Entry_Inspection
   is
      MS_PER_SECOND : constant := 1_000;
      VOLUME_SHIFT : constant := 32;
      function Ms (seconds : Unsigned_32) return Unsigned_64 is
        (Unsigned_64 (seconds) * MS_PER_SECOND);
   begin
      return
        (valid => INSPECTED_SIZE or INSPECTED_TIMES or INSPECTED_MODE or
                  INSPECTED_LINKS or INSPECTED_OWNER or INSPECTED_OBJECT,
         mode => Unsigned_32 (ino.typeAndPermissions),
         sizeBytes => Ext2.fileSize (ino),
         modifiedMs => Ms (ino.modifiedTime),
         changedMs => Ms (ino.creationTime),
         accessedMs => Ms (ino.accessedTime),
         links => Unsigned_32 (ino.numHardLinks),
         owner => Unsigned_32 (ino.uid),
         group => Unsigned_32 (ino.gid),
         reserved => 0,
         objectId => Shift_Left (Unsigned_64 (volume), VOLUME_SHIFT) or
                     Unsigned_64 (inodeNum));
   end inspectionOf;

   --  Queue_Watch: a directory handle of the client's; Options: subtree.
   procedure handleWatch (q : Client_Queue_Index; handleWord : Unsigned_64; options : Unsigned_32) is
      owner : constant Process_ID := clientQueues (q).owner;
      handle : constant Integer := resolveHandle (handleWord, owner, DIRECTORY_OBJECT);
      selection : Path_Selection;
      volume : Volume_Reference;
      relFirst : Integer;
   begin
      if handle < 0 then
         sendReply (owner, REPLY_WRONG_OBJECT_TYPE, 0);
         return;
      elsif not clientQueues (q).eventLink.Active or else
        (options and not FQ.Watch_Subtree) /= 0 or else
        files (handle).filesystemKind /= EXT2_FILESYSTEM
      then
         sendReply (owner, REPLY_ERR, 0);
         return;
      end if;
      declare
         path : constant String := CuBit.Directory_Paths.Value (extras (handle).directoryPath);
      begin
         Select_Path (Volumes, path, selection, volume, relFirst);
         if selection = Unqualified then
            volume := Default_Write_Volume;
         elsif selection /= Known_Volume then
            volume := No_Volume;
         end if;
         if volume = No_Volume or else volume /= files (handle).volume then
            sendReply (owner, REPLY_ERR, 0);
            return;
         end if;
         declare
            rel : constant String :=
              canonical (path (Integer'Max (relFirst, path'First) .. path'Last));
         begin
            if not checkAccess (owner, fullName (volume, rel), ACL_READ) then
               sendReply (owner, REPLY_ACCESS_DENIED, 0);
               return;
            end if;
            for w in FE.Watch_Number loop
               --  A number is free when unused, or ended and its
               --  Watch_Ended read (no old record is read as the new watch's).
               if watches (q, w).state = WR.Unused or else
                 (watches (q, w).state = WR.Ended and then rescanRead (q, w))
               then
                  if not WR.May_Admit (ringFree (q), clientQueues (q).watching) then
                     exit;
                  end if;
                  watches (q, w) :=
                    (state => WR.Watching, subtree => (options and FQ.Watch_Subtree) /= 0,
                     volume => volume, rel => [others => ' '], relLength => rel'Length,
                     rescanMark => 0);
                  watches (q, w).rel (1 .. rel'Length) := rel;
                  clientQueues (q).watching := clientQueues (q).watching + 1;
                  sendReply (owner, REPLY_OK, Unsigned_64 (w));
                  return;
               end if;
            end loop;
            sendReply (owner, REPLY_NO_SPACE, 0);
         end;
      end;
   end handleWatch;

   procedure handleUnwatch (q : Client_Queue_Index; number : Unsigned_64) is
      owner : constant Process_ID := clientQueues (q).owner;
   begin
      --  An ended watch (Watch_Ended posted or owed) is not live.
      if number not in 1 .. FE.Maximum_Watches or else not live (q, FE.Watch_Number (number)) then
         sendReply (owner, REPLY_ERR, 0);
         return;
      end if;
      --  Its last record is a Watch_Ended, as for a folder that went.
      endWatch (q, FE.Watch_Number (number));
      sendReply (owner, REPLY_OK, 0);
   end handleUnwatch;

   ---------------------------------------------------------------------------
   --  Queue_List_Scopes (docs/filesystem-protocol-v2.md step 6): the
   --  caller's own access profile, as CuBit.File_Access wire entries. A
   --  process learns only its own grants. Without a profile: no entries.
   ---------------------------------------------------------------------------
   procedure handleListScopes (owner : Process_ID; rangeBytes : Unsigned_64) is
      package FA renames CuBit.File_Access;
      wire : FA.Wire_Bytes (1 .. FA.Maximum_Entries * FA.Wire_Entry_Bytes);
      count : Natural := 0;
   begin
      for profile of aclProfiles loop
         if profile.active and then profile.pid = owner then
            FA.Encode (profile.policy, wire, count);
            exit;
         end if;
      end loop;
      if Unsigned_64 (count) * FA.Wire_Entry_Bytes > rangeBytes then
         sendReply (owner, REPLY_NO_SPACE, Unsigned_64 (count));
         return;
      end if;
      if count > 0 then
         declare
            target : FA.Wire_Bytes (1 .. count * FA.Wire_Entry_Bytes)
              with Import, Address => curArena;
         begin
            target := wire (1 .. count * FA.Wire_Entry_Bytes);
         end;
      end if;
      sendReply (owner, REPLY_OK, Unsigned_64 (count));
   end handleListScopes;

   ---------------------------------------------------------------------------
   --  Queue_Describe_Volume (step 7): the volume of a path the caller may
   --  read, from the service's in-memory superblock (with the blocks freed
   --  by the running transaction, which are free once it commits).
   ---------------------------------------------------------------------------
   package VD renames CuBit.Volume_Descriptions;
   use type VD.Volume_Kind;
   pragma Compile_Time_Error
     (VD.Record_Bytes /= FQ.Volume_Description_Bytes, "Volume.Description.V1 size");

   procedure handleDescribeVolume (owner : Process_ID; pathLength, rangeBytes : Unsigned_64) is
      pathBuffer : String (1 .. Natural (MAXIMUM_PATH_BYTES));
      selection : Path_Selection;
      reference : Volume_Reference;
      relStart : Integer;
      item : VD.Description;
      bytes : VD.Record_Image;

      procedure setName (name : String) is
      begin
         item.Length := Natural'Min (name'Length, VD.Maximum_Name_Bytes);
         for i in 1 .. item.Length loop
            item.Name (i) := Character'Pos (name (name'First + i - 1));
         end loop;
      end setName;
   begin
      if pathLength = 0 or else pathLength > MAXIMUM_PATH_BYTES or else
        pathLength > rangeBytes or else rangeBytes < VD.Record_Bytes
      then
         sendReply (owner, REPLY_ERR, 0);
         return;
      end if;
      declare
         source : String (1 .. Natural (pathLength)) with Import, Address => curArena;
      begin
         pathBuffer (1 .. Natural (pathLength)) := source;   --  copy, then validate
      end;
      declare
         path : String renames pathBuffer (1 .. Natural (pathLength));
      begin
         if hasTraversal (path) then
            sendReply (owner, REPLY_ERR, 0);
            return;
         elsif not checkAccess (owner, path, ACL_READ) then
            sendReply (owner, REPLY_ACCESS_DENIED, 0);
            return;
         end if;
         Select_Path (Volumes, path, selection, reference, relStart);
      end;
      if selection = Unqualified then
         reference := Default_Write_Volume;
         selection := (if reference = No_Volume then Unknown_Volume else Known_Volume);
      end if;
      case selection is
         when Known_Volume =>
            declare
               volume : constant Volume_Index := Volume_Index (reference);
               admission : Admission_Result;
            begin
               ensureVolume (volume, admission);
               if admission /= Admitted then
                  sendReply (owner, REPLY_IO_ERROR, 0);
                  return;
               end if;
               declare
                  fs : Ext2.Filesystem renames Contexts (volume).Fs;
                  free : constant Unsigned_64 := Unsigned_64 (fs.sb.freeBlocks);
                  total : constant Unsigned_64 := Unsigned_64 (fs.sb.blockCount);
               begin
                  item.Kind := (if (fs.sb.compatibleFeatures and Ext2_Support.Compat_Has_Journal) /= 0
                                then VD.Ext3 else VD.Ext2);
                  item.Flags :=
                    (if fs.writeQuarantined or else
                        CuBit.Block_Devices.Is_Read_Only (fs.device.description)
                     then VD.Read_Only else 0) +
                    (if fs.journal.Active and then item.Kind = VD.Ext3 then VD.Journaled else 0) +
                    (if CuBit.Block_Devices.Can_Persist (fs.device.description)
                     then VD.Durable_Flush else 0);
                  item.Block := VD.Block_Size'Max (VD.Smallest_Block,
                                  VD.Block_Size'Min (VD.Largest_Block, fs.blkSize));
                  item.Total_Blocks := total;
                  item.Free_Blocks := Unsigned_64'Min (free, total);
                  item.Releasing_Blocks :=
                    Unsigned_64'Min (Unsigned_64 (fs.pendingCount), total - item.Free_Blocks);
                  item.Total_Inodes := Unsigned_64 (fs.sb.inodeCount);
                  item.Free_Inodes := Unsigned_64'Min (Unsigned_64 (fs.sb.freeInodes), item.Total_Inodes);
                  setName (Volume_List.Name (Volumes, volume));
               end;
            end;
         when Boot_Archive =>
            if not cpioOk then
               sendReply (owner, REPLY_NOT_FOUND, 0);
               return;
            end if;
            item := (Kind => VD.Boot_Archive, Flags => VD.Read_Only, Block => VD.Smallest_Block,
                     Total_Blocks => (cpioArchive.size + VD.Smallest_Block - 1) / VD.Smallest_Block,
                     others => <>);
            setName (Volume_List.Boot_Archive_Name);
         when Optical_Volume =>
            declare
               blocks : Unsigned_64;
               mounted : Boolean;
            begin
               ISO9660.Describe (blocks, mounted);
               if not mounted then
                  sendReply (owner, REPLY_NOT_FOUND, 0);
                  return;
               end if;
               item := (Kind => VD.ISO_9660, Flags => VD.Read_Only,
                        Block => ISO_Records.Block_Bytes, Total_Blocks => blocks, others => <>);
               setName (Volume_List.Optical_Name);
            end;
         when Unqualified | Unknown_Volume | Invalid_Path =>
            sendReply (owner, REPLY_NOT_FOUND, 0);
            return;
      end case;
      if not VD.Valid (item) then
         sendReply (owner, REPLY_ERR, 0);
         return;
      end if;
      VD.Encode (item, bytes);
      declare
         target : VD.Record_Image with Import, Address => curArena;
      begin
         target := bytes;
      end;
      sendReply (owner, REPLY_OK, VD.Record_Bytes);
   end handleDescribeVolume;

   ---------------------------------------------------------------------------
   --  Server-side copy (docs/filesystem-protocol-v2.md step 5): Queue_Copy
   --  is answered once, when the copy ends; meanwhile the service copies one
   --  bounded slice (Copy_Slices) per copy per pass between its other work,
   --  through its block cache, and publishes the bytes done in the queue's
   --  progress words (FQ.Server_Copies_At). A copy holds no lock a client
   --  waits on: other requests, including writes to the same files, go on
   --  between slices (POSIX copy_file_range gives no more).
   ---------------------------------------------------------------------------
   subtype Copy_Index is Natural range 0 .. FQ.Maximum_Copies - 1;
   type Copy_Record is record
      active   : Boolean := False;
      token    : FQueues.Token := 0;
      source   : Natural := 0;           --  handle slots
      target   : Natural := 0;
      plan     : Copy_Slices.Copy;
      deadline : Unsigned_64 := Wait_Forever;
      cancel   : Boolean := False;
   end record;
   copies : array (Client_Queue_Index, Copy_Index) of Copy_Record;
   COPY_SLICE_BYTES : constant := HARVEST_BUFFER_BYTES;
   pragma Compile_Time_Error
     (Copy_Slices.Copy_To_End /= FQ.Copy_To_End, "Copy_To_End");

   --  Publish copy c's progress (the token last when starting, first when
   --  ending: a reader that sees its token sees that copy's count).
   procedure publishCopy (q : Client_Queue_Index; c : Copy_Index) is
      at_byte : constant Natural := FQ.Server_Copies_At + c * FQ.Copy_Entry_Bytes;
      token : Unsigned_64 with Volatile, Import,
        Address => serverWord (q, at_byte + FQ.Copy_Token_At);
      done : Unsigned_64 with Volatile, Import,
        Address => serverWord (q, at_byte + FQ.Copy_Done_At);
   begin
      if copies (q, c).active then
         done := copies (q, c).plan.Done;
         token := Unsigned_64 (copies (q, c).token);
      else
         token := 0;
         done := 0;
      end if;
   end publishCopy;

   --  Answer copy c (once): label, the bytes copied (always a prefix).
   procedure finishCopy (q : Client_Queue_Index; c : Copy_Index; label : Unsigned_32) is
      saved : constant Reply_Route := curRoute;
      done : constant Unsigned_64 := copies (q, c).plan.Done;
   begin
      curRoute := (queue => q, token => copies (q, c).token);
      copies (q, c).active := False;
      publishCopy (q, c);
      answerEntry (label, done, 0);
      curRoute := saved;
      copies (q, c) := (others => <>);
   end finishCopy;

   procedure copyHandleGone (slot : Natural) is
   begin
      for q in clientQueues'Range loop
         for c in Copy_Index loop
            if copies (q, c).active and then
              (copies (q, c).source = slot or else copies (q, c).target = slot)
            then
               finishCopy (q, c, REPLY_ERR);
            end if;
         end loop;
      end loop;
   end copyHandleGone;

   function copiesActive return Boolean is
     (for some q in clientQueues'Range =>
        (for some c in Copy_Index => copies (q, c).active));

   function sourceSize (slot : Natural) return Unsigned_64 is
     (case files (slot).filesystemKind is
         when EXT2_FILESYSTEM =>
           Ext2.fileSize (Open_Inodes.Value (inodeObjects, Open_Inodes.Owner_Index (slot))),
         when ISO_FILESYSTEM => Unsigned_64 (extras (slot).opticalFile.Bytes),
         when CPIO_ARCHIVE => cpioArchive.files (files (slot).cpioFileIdx).dataSize);

   --  Queue_Copy: Handle = source (may read), Spare_1 = target (may write,
   --  ext2), Position / Arena_Offset = source / target offsets, Length (or
   --  FQ.Copy_To_End), Spare_2 = deadline (absolute ms, or Wait_Forever).
   procedure handleCopy (q : Client_Queue_Index; r : FQ.Request) is
      owner : constant Process_ID := clientQueues (q).owner;
      source : constant Integer := resolveHandle (r.Handle, owner, FILE_OBJECT);
      target : constant Integer := resolveHandle (r.Spare_1, owner, FILE_OBJECT);
      plan : Copy_Slices.Copy;
      admitted : Boolean;
   begin
      if source < 0 or else target < 0 then
         sendReply (owner, REPLY_WRONG_OBJECT_TYPE, 0);
         return;
      elsif (files (source).openRights and ACL_READ) = 0 or else
        (files (target).openRights and ACL_WRITE) = 0
      then
         sendReply (owner, REPLY_ACCESS_DENIED, 0);
         return;
      elsif files (target).filesystemKind /= EXT2_FILESYSTEM then
         sendReply (owner, REPLY_READ_ONLY, 0);
         return;
      end if;
      Copy_Slices.Admit (r.Position, r.Arena_Offset, r.Length, plan, admitted);
      if not admitted then
         sendReply (owner, REPLY_OUT_OF_RANGE, 0);
         return;
      end if;
      --  One file into itself: only between ranges that cannot overlap.
      if files (source).filesystemKind = EXT2_FILESYSTEM and then
        files (source).volume = files (target).volume and then
        files (source).inodeNum = files (target).inodeNum and then
        (r.Length = FQ.Copy_To_End or else
         (plan.Source_At < plan.Target_At + plan.Wanted and then
          plan.Target_At < plan.Source_At + plan.Wanted))
      then
         sendReply (owner, REPLY_ERR, 0);
         return;
      end if;
      for c in Copy_Index loop
         if not copies (q, c).active then
            copies (q, c) := (active => True, token => curRoute.token, source => source,
                              target => target, plan => plan, deadline => r.Spare_2,
                              cancel => False);
            publishCopy (q, c);
            return;   --  answered when it ends
         end if;
      end loop;
      sendReply (owner, REPLY_BUSY, 0);
   end handleCopy;

   --  Queue_Cancel: Handle = the copy's token. The copy is answered
   --  REPLY_CANCELLED (with its prefix) at its next slice.
   procedure handleCancel (q : Client_Queue_Index; token : Unsigned_64) is
   begin
      for c in Copy_Index loop
         if copies (q, c).active and then Unsigned_64 (copies (q, c).token) = token then
            copies (q, c).cancel := True;
            sendReply (clientQueues (q).owner, REPLY_OK, 0);
            return;
         end if;
      end loop;
      sendReply (clientQueues (q).owner, REPLY_NOT_FOUND, 0);
   end handleCancel;

   --  One slice of copy c.
   procedure copySlice (q : Client_Queue_Index; c : Copy_Index) is
      use type Copy_Slices.Ending;
      copy : Copy_Record renames copies (q, c);
      source : constant Natural := copy.source;
      target : constant Natural := copy.target;
      slice : Unsigned_64 := 0;
      copied : Unsigned_64 := 0;
      label : Unsigned_32 := REPLY_OK;
      ending : Copy_Slices.Ending;
   begin
      if not copy.cancel then
         --  The source's buffered writes first; the target's client stops
         --  buffering (its pages are taken in) before the service writes.
         if writeDelegated (source) then
            harvest (source);
         end if;
         if writeDelegated (target) then
            recallWrite (target);
         end if;
         slice := Copy_Slices.Next (copy.plan, sourceSize (source), COPY_SLICE_BYTES);
      end if;
      if slice > 0 then
         declare
            read : Unsigned_64 := 0;
            readStatus : Ext2.Read_Status := Ext2.Read_Complete;
            writeStatus : Ext2.Write_Status := Ext2.Write_Complete;
            targetInode : Ext2.Inode;
         begin
            case files (source).filesystemKind is
               when EXT2_FILESYSTEM =>
                  Ext2.readData
                    (Contexts (files (source).volume).Fs,
                     Open_Inodes.Value (inodeObjects, Open_Inodes.Owner_Index (source)),
                     Copy_Slices.Source_Position (copy.plan), harvestBuffer'Address, slice,
                     read, readStatus);
               when ISO_FILESYSTEM =>
                  declare
                     ok : Boolean;
                  begin
                     ISO9660.Read (extras (source).opticalFile, Copy_Slices.Source_Position (copy.plan),
                                   slice, harvestBuffer'Address, read, ok);
                     readStatus := (if ok then Ext2.Read_Complete else Ext2.Read_Device_Error);
                  end;
               when CPIO_ARCHIVE =>
                  read := Cpio.readData (cpioArchive, files (source).cpioFileIdx,
                                         Copy_Slices.Source_Position (copy.plan),
                                         harvestBuffer'Address, slice);
            end case;
            read := Unsigned_64'Min (read, slice);
            if readStatus /= Ext2.Read_Complete then
               label := REPLY_IO_ERROR;
            elsif read > 0 then
               fileChanging ((files (target).volume, files (target).inodeNum), target);
               targetInode := Open_Inodes.Value (inodeObjects, Open_Inodes.Owner_Index (target));
               Ext2.writeData
                 (Contexts (files (target).volume).Fs, files (target).inodeNum, targetInode,
                  Copy_Slices.Target_Position (copy.plan), harvestBuffer'Address, read,
                  copied, writeStatus);
               copied := Unsigned_64'Min (copied, read);
               if writeStatus /= Ext2.Write_Recovery_Required then
                  Open_Inodes.Replace (inodeObjects, Open_Inodes.Owner_Index (target), targetInode);
                  refreshDelegation (target);
               end if;
               label := replyForWrite (writeStatus);
            end if;
            if label = REPLY_OK and then read < slice then
               label := REPLY_IO_ERROR;   --  the source gave less than it holds
            end if;
         end;
         Copy_Slices.Advance (copy.plan, Unsigned_64'Min (copied, copy.plan.Wanted - copy.plan.Done));
         publishCopy (q, c);
      end if;
      ending := Copy_Slices.Decide
        (Slice_Failed => label /= REPLY_OK, Cancel_Asked => copy.cancel,
         Finished => slice = 0 and then not copy.cancel,
         Deadline_Passed => copy.deadline /= Wait_Forever and then
                            syscall (SYSCALL_GETTIME) >= copy.deadline);
      case ending is
         when Copy_Slices.Going => null;
         when Copy_Slices.Complete => finishCopy (q, c, REPLY_OK);
         when Copy_Slices.Cancelled => finishCopy (q, c, REPLY_CANCELLED);
         when Copy_Slices.Deadline_Reached => finishCopy (q, c, REPLY_DEADLINE);
         when Copy_Slices.Failed => finishCopy (q, c, label);
      end case;
   end copySlice;

   --  Each pass: one slice of every copy.
   procedure runCopies is
   begin
      for q in clientQueues'Range loop
         for c in Copy_Index loop
            if copies (q, c).active then
               copySlice (q, c);
            end if;
         end loop;
      end loop;
   end runCopies;

   --  Directory resume tokens (Directory.Page.V2): the next record's byte
   --  position in the directory, and above it a check naming the directory,
   --  so a token handed back for another directory is refused. The position
   --  itself is checked by Ext2.readDirectoryPage whatever its origin.
   TOKEN_POSITION_BITS : constant := 32;
   function directoryCheck (handle : Natural) return Unsigned_32 is
      --  Fibonacci hashing: spreads the inode, volume and generation over
      --  the word (a mix-up detector, not authority: the handle is that).
      GOLDEN : constant Unsigned_32 := 16#9E37_79B1#;
      VOLUME_SHIFT : constant := 24;
      mixed : Unsigned_32 :=
        files (handle).inodeNum * GOLDEN xor
        Shift_Left (Unsigned_32 (files (handle).volume), VOLUME_SHIFT);
   begin
      if files (handle).filesystemKind = EXT2_FILESYSTEM then
         mixed := mixed xor extras (handle).directoryInode.generationNumber;
      end if;
      return (if mixed = 0 then GOLDEN else mixed);
   end directoryCheck;

   function resumeToken (handle : Natural; position : Unsigned_64) return Unsigned_64 is
     (Shift_Left (Unsigned_64 (directoryCheck (handle)), TOKEN_POSITION_BITS) or
      (position and (2 ** TOKEN_POSITION_BITS - 1)));

   --  Queue_Seek_Directory: continue the handle's listing at a resume token
   --  (0: from the start).
   procedure handleSeekDirectory (owner : Process_ID; handleWord, token : Unsigned_64) is
      handle : constant Integer := resolveHandle (handleWord, owner, DIRECTORY_OBJECT);
   begin
      if handle < 0 then
         sendReply (owner, REPLY_WRONG_OBJECT_TYPE, 0);
      elsif token = 0 then
         files (handle).offset := 0;
         sendReply (owner, REPLY_OK, 0);
      elsif Unsigned_32 (Shift_Right (token, TOKEN_POSITION_BITS)) /= directoryCheck (handle) then
         sendReply (owner, REPLY_OUT_OF_RANGE, 0);
      else
         files (handle).offset := token and (2 ** TOKEN_POSITION_BITS - 1);
         sendReply (owner, REPLY_OK, 0);
      end if;
   end handleSeekDirectory;

   --  Write one Directory.Page.V2 (CuBit.Directory_Pages) of the handle's
   --  directory at target, continuing from its cursor. Metadata: fill each
   --  entry's facts from its inode (else only kind and object). The page is
   --  built in service memory and copied out whole.
   procedure readDirectoryInto
     (handle : Natural; target : System.Address; metadata : Boolean;
      label : out Unsigned_32; listed : out Natural; ended : out Boolean)
   is
      package DP renames CuBit.Directory_Pages;
      VOLUME_SHIFT : constant := 32;
      page : DP.Page;
      writer : DP.Writer;
      nextCursor : Unsigned_64 := files (handle).offset;
      atEnd : Boolean := False;
      readStatus : Ext2.Directory_Read_Status := Ext2.Directory_Malformed;
      records : Ext2.Listed_Records;
      count : Natural := 0;
      shared : DP.Page with Import, Address => target;
   begin
      label := REPLY_OK;
      listed := 0;
      ended := False;
      DP.Start (page, writer);
      if files (handle).filesystemKind = CPIO_ARCHIVE then
         if files (handle).offset > Unsigned_64 (cpioArchive.count) then
            label := REPLY_MALFORMED_FILESYSTEM;
         else
            while nextCursor < Unsigned_64 (cpioArchive.count) loop
               declare
                  archiveIndex : constant Natural := Natural (nextCursor);
                  nameLength : constant Natural := cpioArchive.files (archiveIndex).nameLen;
                  name : DP.Name_Bytes := [others => 0];
               begin
                  if nameLength = 0 or else nameLength > DP.Maximum_Name_Bytes then
                     label := REPLY_MALFORMED_FILESYSTEM;
                     exit;
                  end if;
                  exit when not DP.Fits (writer, nameLength);
                  declare
                     archiveName : String (1 .. nameLength)
                       with Import,
                            Address => cpioArchive.base + Storage_Offset
                              (cpioArchive.files (archiveIndex).nameOff);
                  begin
                     for index in 1 .. nameLength loop
                        name (index) := Unsigned_8 (Character'Pos (archiveName (index)));
                     end loop;
                  end;
                  DP.Append
                    (page, writer,
                     (Kind => DP.Kind_File, Valid => DP.Valid_Size or DP.Valid_Object,
                      Object => nextCursor + 1,
                      Size => cpioArchive.files (archiveIndex).dataSize, others => <>),
                     name, nameLength);
                  nextCursor := nextCursor + 1;
               end;
            end loop;
            atEnd := nextCursor = Unsigned_64 (cpioArchive.count);
         end if;
      else
         case files (handle).filesystemKind is
            when EXT2_FILESYSTEM =>
               Ext2.readDirectoryPage
                 (Contexts (files (handle).volume).Fs, extras (handle).directoryInode,
                  files (handle).offset, records, count, nextCursor, readStatus);
            when CPIO_ARCHIVE | ISO_FILESYSTEM => null;
         end case;
         case readStatus is
            when Ext2.Directory_Page_Complete => null;
            when Ext2.Directory_End => atEnd := True;
            when Ext2.Directory_Malformed => label := REPLY_MALFORMED_FILESYSTEM;
            when Ext2.Directory_Device_Error => label := REPLY_IO_ERROR;
            when Ext2.Directory_Out_Of_Range => label := REPLY_OUT_OF_RANGE;
            when Ext2.Directory_Range_Unsupported => label := REPLY_FILE_RANGE_UNSUPPORTED;
         end case;
         if label = REPLY_OK then
            for index in 0 .. count - 1 loop
               declare
                  item : constant Ext2.Listed_Record := records (index);
                  facts : DP.Facts :=
                    (Kind => item.kind, Valid => DP.Valid_Object,
                     Object => Shift_Left (Unsigned_64 (files (handle).volume), VOLUME_SHIFT) or
                               Unsigned_64 (item.inode),
                     others => <>);
               begin
                  if metadata then
                     declare
                        ino : Ext2.Inode;
                        inodeStatus : Ext2.Read_Status;
                        described : Entry_Inspection;
                     begin
                        Ext2.readInode
                          (Contexts (files (handle).volume).Fs, item.inode, ino, inodeStatus);
                        if inodeStatus = Ext2.Read_Complete then
                           described := inspectionOf (ino, files (handle).volume, item.inode);
                           facts :=
                             (Kind => item.kind, Valid => described.valid,
                              Object => described.objectId, Size => described.sizeBytes,
                              Modified => described.modifiedMs, Changed => described.changedMs,
                              Accessed => described.accessedMs, Mode => described.mode,
                              Links => described.links, Owner => described.owner,
                              Group => described.group);
                        end if;
                     end;
                  end if;
                  --  Ext2 counted the same records against a page.
                  exit when not DP.Fits (writer, item.length);
                  DP.Append (page, writer, facts, item.name, item.length);
               end;
            end loop;
         end if;
      end if;
      if label = REPLY_OK then
         DP.Finish (page, writer, atEnd, resumeToken (handle, nextCursor),
                    Unsigned_64 (namespaceGeneration));
         shared := page;
         files (handle).offset := nextCursor;
         listed := writer.Count;
         ended := atEnd;
      end if;
   end readDirectoryInto;

   --  OP_READ_DIRECTORY_PAGE: one Directory.Page.V2, with metadata, into a
   --  one-page writable grant.
   procedure handleReadDirectoryPage (sender : Process_ID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, DIRECTORY_OBJECT);
      grantAddr : System.Address := System.Null_Address;
      grantOk : Boolean := False;
      returned : Boolean := False;
      label : Unsigned_32;
      listed : Natural;
      ended : Boolean;
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
        (sender, msg.words (1), msg.words (3), Unsigned_64 (DIRECTORY_PAGE_BYTES),
         CuBit.Memory_Grants.Write_Access, grantAddr, grantOk);
      if not grantOk then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;
      readDirectoryInto (handle, grantAddr, True, label, listed, ended);
      returnClientMemory (msg.words (1), msg.words (3), returned);
      if not returned then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      sendReply (sender, label, Unsigned_64 (listed));
   end handleReadDirectoryPage;

   procedure handleCloseDirectory (sender : Process_ID; msg : Message) is
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
      clientPID : Process_ID    := No_Process;
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
   --  A file's last name is gone while handles hold it: they keep a
   --  zero-link inode (their writes must not restore the link), freed at
   --  the last close (reclaimOrphan).
   procedure keepOrphan (volume : Volume_Index; target : Unsigned_32; holder : Natural) is
      current : Ext2.Inode :=
        Open_Inodes.Value (inodeObjects, Open_Inodes.Owner_Index (holder));
      recorded : Boolean := False;
   begin
      current.numHardLinks := 0;
      Open_Inodes.Replace
        (inodeObjects, Open_Inodes.Owner_Index (holder), current);
      for orphan of orphans loop
         if orphan.number = 0 then
            orphan := (volume, target);
            orphanCount := orphanCount + 1;
            recorded := True;
         end if;
         exit when recorded;
      end loop;
   end keepOrphan;

   --  A handle on (volume, target) other than a dropped parked one, or -1.
   function holderOf (volume : Volume_Index; target : Unsigned_32) return Integer is
      object : constant Open_Inodes.Link :=
        Open_Inodes.Object_Of (inodeObjects, (volume, target));
   begin
      if object /= 0 and then Open_Inodes.Holding (inodeObjects, object) > 0 then
         return Open_Inodes.Holder (inodeObjects, object, 0);
      end if;
      return -1;
   end holderOf;

   procedure handleRename (sender : Process_ID; msg : Message) is
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
            sendReply (sender, REPLY_CROSS_VOLUME, 0);
            return;
         elsif oldVolume = No_Volume then
            sendReply (sender, REPLY_READ_ONLY, 0);
            return;
         end if;
         declare
            status : Ext2.Rename_Status;
            target, replacedNumber : Unsigned_32 := 0;
            replaced : Ext2.Inode;
            holder : Integer := -1;
            volume : constant Volume_Index := Volume_Index (oldVolume);
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
            --  A replaced destination is checked as unlink checks its file:
            --  no exclusive hold, and a held one keeps its inode until the
            --  last close (the ext3 orphan list).
            Ext2.resolvePath
              (Contexts (volume).Fs, newPath (newRelStart .. newPath'Last),
               target, lookup);
            if lookup not in Ext2.Lookup_Found | Ext2.Lookup_Not_Found then
               sendReply (sender, Lookup_Reply_Label (lookup), 0);
               return;
            elsif lookup = Ext2.Lookup_Found and then target /= inodeNum then
               if Open_Inodes.Exclusively_Held (inodeObjects, (volume, target)) then
                  sendReply (sender, REPLY_SHARING_VIOLATION, 0);
                  return;
               end if;
               holder := holderOf (volume, target);
            else
               target := 0;
            end if;
            --  No source identity means no hold can conflict. Let rename's
            --  own preflight distinguish an unsupported parent layout from
            --  an absent source; failed metadata I/O was already stopped above.
            bumpNamespace;
            Ext2.renamePath
              (Contexts (volume).Fs,
               oldPath (oldRelStart .. oldPath'Last),
               newPath (newRelStart .. newPath'Last),
               holder >= 0, replacedNumber, replaced, status);
            if target /= 0 and then replacedNumber = target and then holder >= 0 then
               keepOrphan (volume, target, holder);
            end if;
            if target /= 0 and then Ext2."=" (status, Ext2.Rename_Complete) then
               --  The replaced file's number may name another file next.
               bumpVersion ((volume, target));
            end if;

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
                  when Ext2.Rename_Not_Directory => label := REPLY_WRONG_OBJECT_TYPE;
                  when Ext2.Rename_Is_Directory => label := REPLY_IS_DIRECTORY;
                  when Ext2.Rename_Not_Empty => label := REPLY_NOT_EMPTY;
                  when Ext2.Rename_Invalid_Move => label := REPLY_INVALID_MOVE;
                  when Ext2.Rename_No_Room => label := REPLY_FILE_RANGE_UNSUPPORTED;
               end case;
               if label = REPLY_OK then
                  declare
                     moved : Ext2.Inode;
                     movedStatus : Ext2.Read_Status;
                     isDirectory : Boolean := False;
                  begin
                     Ext2.readInode (Contexts (volume).Fs, inodeNum, moved, movedStatus);
                     if movedStatus = Ext2.Read_Complete then
                        isDirectory := Ext2.inodeType (moved) = Ext2.INODE_DIRECTORY;
                     end if;
                     renameCookie := renameCookie + 1;
                     notify (volume, oldPath (oldRelStart .. oldPath'Last), FE.Renamed_From,
                             isDirectory, Shift_Left (Unsigned_64 (volume), 32) or Unsigned_64 (inodeNum), renameCookie);
                     notify (volume, newPath (newRelStart .. newPath'Last), FE.Renamed_To,
                             isDirectory, Shift_Left (Unsigned_64 (volume), 32) or Unsigned_64 (inodeNum), renameCookie);
                     renameNoticePaths (oldPath (oldRelStart .. oldPath'Last),
                                        newPath (newRelStart .. newPath'Last));
                  end;
               end if;
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
     (sender : Process_ID; msg : Message; pathBuffer : out String;
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
      --  unlink, mkdir, rmdir: names change from here on.
      bumpNamespace;
      ok := True;
   end namespaceRequest;

   function replyForRemove (status : Ext2.Remove_Status) return Unsigned_32 is
     (case status is
        when Ext2.Remove_Complete => REPLY_OK,
        when Ext2.Remove_Not_Found => REPLY_NOT_FOUND,
        when Ext2.Remove_Invalid_Name => REPLY_ERR,
        when Ext2.Remove_Wrong_Type => REPLY_WRONG_OBJECT_TYPE,
        when Ext2.Remove_Not_Empty => REPLY_NOT_EMPTY,
        when Ext2.Remove_Malformed => REPLY_MALFORMED_FILESYSTEM,
        when Ext2.Remove_Unsupported => REPLY_UNSUPPORTED_OBJECT,
        when Ext2.Remove_Read_Only => REPLY_READ_ONLY,
        when Ext2.Remove_Out_Of_Range => REPLY_OUT_OF_RANGE,
        when Ext2.Remove_IO_Error => REPLY_IO_ERROR,
        when Ext2.Remove_Durability_Unsupported => REPLY_DURABILITY_UNSUPPORTED,
        when Ext2.Remove_Recovery_Required => REPLY_RECOVERY_REQUIRED);

   --  An unlink's parked handle (FQ.Queue_Unlink's Handle, 0: none),
   --  checked against the service's own table: -1 if it names no live
   --  handle of the sender. With dropping, it alone holds the file being
   --  unlinked (key, one holder, one link), under a write delegation: its
   --  buffered pages may be dropped unwritten, but only once the unlink
   --  has succeeded (handleUnlink); otherwise they are written.
   procedure checkParked
     (sender : Process_ID; handleWord : Unsigned_64; key : Inode_Identity;
      handle : out Integer; dropping : out Boolean)
   is
      object : Open_Inodes.Link;
      q : Client_Queue_Count;
   begin
      dropping := False;
      handle := (if handleWord = 0 then -1
                 else resolveHandle (handleWord, sender, FILE_OBJECT));
      if handle < 0 then
         return;
      end if;
      object := Open_Inodes.Object_Of_Owner (inodeObjects, Open_Inodes.Owner_Index (handle));
      q := queueOf (sender);
      dropping := writeDelegated (handle) and then object /= 0 and then
        Open_Inodes.Key_Of_Object (inodeObjects, object) = key and then
        Open_Inodes.Holding (inodeObjects, object) = 1 and then
        Open_Inodes.Value (inodeObjects, Open_Inodes.Owner_Index (handle)).numHardLinks = 1 and then
        q /= No_Client_Queue and then clientQueues (q).dirty /= System.Null_Address;
   end checkParked;

   --  Unlink a regular file. While handles hold it, the name and link go
   --  now and the inode at the last close (releaseHandle).
   procedure handleUnlink (sender : Process_ID; msg : Message) is
      pathBuffer : String (1 .. Natural (MAXIMUM_PATH_BYTES));
      pathLen, relStart : Natural;
      volume : Volume_Index;
      ok : Boolean;
      target, unlinkedNumber : Unsigned_32 := 0;
      lookup : Ext2.Directory_Lookup_Status;
      unlinked : Ext2.Inode;
      status : Ext2.Remove_Status;
      holder : Integer := -1;
      parked : Integer := -1;
      dropping : Boolean := False;
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
         checkParked (sender, msg.words (2), (volume, target), parked, dropping);
         if parked >= 0 and then not dropping then
            --  Closed as CLOSE would, its pages written first.
            writebackFailed (parked) := False;
            releaseHandle (parked);
            parked := -1;
         elsif dropping then
            --  The client may not buffer more; what it has waits for the
            --  outcome of the unlink.
            delegate (parked, False);
         end if;
         --  A handle that outlives this request keeps the inode (the ext3
         --  orphan list, freed at its last close). A dropped parked handle
         --  is the file's only holder (checkParked) and is released below
         --  in this same request, before anything else can run: the inode
         --  goes with its name, under the unlink's own journal handle, as
         --  an unheld file's does.
         declare
            object : constant Open_Inodes.Link :=
              Open_Inodes.Object_Of (inodeObjects, (volume, target));
         begin
            if not dropping and then object /= 0 and then
              Open_Inodes.Holding (inodeObjects, object) > 0
            then
               holder := Open_Inodes.Holder (inodeObjects, object, 0);
            end if;
         end;
         Ext2.unlinkPath
           (Contexts (volume).Fs, relPath, holder >= 0, unlinkedNumber, unlinked, status);
      end;
      if holder >= 0 and then unlinkedNumber = target then
         keepOrphan (volume, target, holder);
      end if;
      if unlinkedNumber = target and then target /= 0 then
         --  A later file with this inode number is another file: pages a
         --  client cached for this one must never be taken for it.
         bumpVersion ((volume, target));
      end if;
      if dropping then
         --  Once the name is gone, so is the file: nothing can read the
         --  pages, and once the handle is released its entries' tags name
         --  no live handle, so they are never written anywhere (a later
         --  harvest of the client frees them). A name still there keeps
         --  the file, and so its data.
         if unlinkedNumber /= target then
            harvest (parked);
         end if;
         writebackFailed (parked) := False;
         releaseHandle (parked);
      end if;
      if replyForRemove (status) = REPLY_OK then
         notify (volume, pathBuffer (relStart .. pathLen), FE.Removed, False,
                 Shift_Left (Unsigned_64 (volume), 32) or Unsigned_64 (target));
      end if;
      sendReply (sender, replyForRemove (status), 0);
   end handleUnlink;

   procedure handleMkdir (sender : Process_ID; msg : Message) is
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
         if replyForWrite (status) = REPLY_OK then
            notify (volume, pathBuffer (relStart .. pathLen), FE.Created, True,
                    Shift_Left (Unsigned_64 (volume), 32) or Unsigned_64 (created));
         end if;
         sendReply (sender, replyForWrite (status), Unsigned_64 (created));
      end if;
   end handleMkdir;

   --  Remove an empty directory. Refused while a directory handle has it
   --  open: its blocks would be freed under the enumeration.
   procedure handleRmdir (sender : Process_ID; msg : Message) is
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
         for j in 0 .. EXTENDED_HANDLES - 1 loop   --  directory handles
            if files (j).active and then files (j).objectKind = DIRECTORY_OBJECT and then
              files (j).filesystemKind = EXT2_FILESYSTEM and then
              files (j).volume = volume and then files (j).inodeNum = target
            then
               sendReply (sender, REPLY_SHARING_VIOLATION, 0);
               return;
            end if;
         end loop;
         Ext2.removeDirectoryPath (Contexts (volume).Fs, relPath, removed, status);
         if replyForRemove (status) = REPLY_OK then
            notify (volume, relPath, FE.Removed, True, Shift_Left (Unsigned_64 (volume), 32) or Unsigned_64 (target));
         end if;
      end;
      sendReply (sender, replyForRemove (status), 0);
   end handleRmdir;

   --  Main message loop variables
   sender : Process_ID;
   msg    : Message;
   rdAddr : Unsigned_64;
   rdSize : Unsigned_64;

   ---------------------------------------------------------------------------
   --  handleOpen: a client opens its queue's channels (FQ, docs/data-plane.md):
   --  the transfer arena and the optional dirty arena first, then the queue
   --  pair, which takes them in. One queue per process.
   ---------------------------------------------------------------------------
   --  A channel's number here: its queue (or pending entry) and connector.
   CONNECTORS_PER_QUEUE : constant := 8;   --  more than FQ's connectors
   function channelNumber (index : Client_Queue_Index; connector : Unsigned_16) return Unsigned_64 is
     (Unsigned_64 (index) * CONNECTORS_PER_QUEUE + Unsigned_64 (connector));

   procedure handleChannelOpen (sender : Process_ID; msg : Message) is
      use type CuBit.Channel_Contracts.Contract;
      isOpen, valid : Boolean;
      offered : CuBit.Channel_Contracts.Contract;
      openerSide : CuBit.Channels.Side;
      connector : Unsigned_16;
      answer : Message;
      ignore : Unsigned_64;
      pending : Client_Queue_Count := No_Client_Queue;
      free : Client_Queue_Count := No_Client_Queue;

      procedure refuse (why : CuBit.Channel_Protocol.Open_Refusal) is
         held : Natural := 0;
      begin
         for q in clientQueues'Range loop
            if clientQueues (q).owner /= No_Process then held := held + 1; end if;
         end loop;
         debugPrint ("FS: channel open refused " &
                     CuBit.Channel_Protocol.Open_Refusal'Image (why) &
                     " connector" & connector'Image &
                     " queues held" & held'Image & LF);
         ignore := reply (sender, CuBit.Channels.Refusal_Reply (why));
      end refuse;
   begin
      CuBit.Channels.Decode_Open (msg, isOpen, valid, offered, openerSide, connector);
      for q in clientQueues'Range loop
         if clientQueues (q).owner = sender then
            --  The event ring, once, after the queue pair; the service
            --  produces into it.
            if valid and then connector = FQ.Event_Connector and then offered = FQ.EVENT_CONTRACT
              and then CuBit.Channels."=" (openerSide, CuBit.Channels.Consuming)
              and then not clientQueues (q).eventLink.Active
            then
               CuBit.Channels.Accept_Open
                 (sender, msg, channelNumber (q, connector), clientQueues (q).eventLink, answer);
               ignore := reply (sender, answer);
            else
               refuse (CuBit.Channel_Protocol.No_Room);     --  one queue per process
            end if;
            return;
         end if;
      end loop;
      for q in pendingArenas'Range loop
         if pendingArenas (q).owner = sender then
            pending := q;
         elsif free = No_Client_Queue and then pendingArenas (q).owner = No_Process
           and then clientQueues (q).owner = No_Process
         then
            --  A pending slot becomes the client queue of the same index,
            --  so both must be free.
            free := q;
         end if;
      end loop;
      if pending = No_Client_Queue then
         pending := free;
      end if;
      if not valid or else pending = No_Client_Queue then
         refuse ((if valid then CuBit.Channel_Protocol.No_Room
                  else CuBit.Channel_Protocol.Unsupported));
         return;
      elsif not CuBit.Channels."=" (openerSide, CuBit.Channels.Producing) then
         --  Only the event ring is opened consuming, after the queue pair.
         refuse (CuBit.Channel_Protocol.Unknown_Type);
         return;
      end if;
      declare
         p : Pending_Arenas renames pendingArenas (pending);
      begin
         if connector = FQ.Transfer_Connector and then transferArena (offered)
           and then not p.transferLink.Active
         then
            CuBit.Channels.Accept_Open
              (sender, msg, channelNumber (pending, connector), p.transferLink, answer);
            if p.transferLink.Active then
               p.owner := sender;
            end if;
            ignore := reply (sender, answer);
         elsif connector = FQ.Dirty_Connector and then offered = FQ.DIRTY_CONTRACT
           and then not p.dirtyLink.Active
         then
            CuBit.Channels.Accept_Open
              (sender, msg, channelNumber (pending, connector), p.dirtyLink, answer);
            if p.dirtyLink.Active then
               p.owner := sender;
            end if;
            ignore := reply (sender, answer);
         elsif connector = FQ.Queue_Connector and then offered = FQ.QUEUE_CONTRACT
           and then p.transferLink.Active and then clientQueues (pending).owner = No_Process
         then
            declare
               c : Client_Queue renames clientQueues (pending);
            begin
               CuBit.Channels.Accept_Open
                 (sender, msg, channelNumber (pending, connector), c.queueLink, answer);
               if c.queueLink.Active then
                  c.owner := sender;
                  c.clientBase := System.Storage_Elements.To_Address
                    (System.Storage_Elements.Integer_Address (c.queueLink.Peer_Base));
                  c.serverBase := System.Storage_Elements.To_Address
                    (System.Storage_Elements.Integer_Address (c.queueLink.Own_Base));
                  c.transferLink := p.transferLink;
                  c.arena := CuBit.Channels.Buffer_Address (c.transferLink, 0);
                  c.arenaBytes := Unsigned_64 (c.transferLink.Item.Buffers) * FQ.Page_Bytes;
                  c.server := (others => <>);
                  c.wake := Queue_Wakes.Idle;
                  if p.dirtyLink.Active then
                     c.dirtyLink := p.dirtyLink;
                     c.dirty := CuBit.Channels.Buffer_Address (c.dirtyLink, 0);
                  end if;
                  p := (others => <>);
                  publishNamespace (pending);
               end if;
               ignore := reply (sender, answer);
            end;
         else
            refuse (CuBit.Channel_Protocol.Unknown_Type);
         end if;
      end;
   end handleChannelOpen;

   procedure releaseClientQueue (owner : Process_ID) is
      wakeHeld : Boolean;
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner = owner then
            --  A held wake is answered (failed), so its slot is free for the
            --  queue's next owner.
            Queue_Wakes.Ended (clientQueues (q).wake, wakeHeld);
            if wakeHeld then
               answerWake (q, REPLY_ERR);
            end if;
            CuBit.Channels.Close (clientQueues (q).queueLink);
            CuBit.Channels.Close (clientQueues (q).transferLink);
            CuBit.Channels.Close (clientQueues (q).dirtyLink);
            CuBit.Channels.Close (clientQueues (q).eventLink);
            for w in FE.Watch_Number loop
               watches (q, w) := (others => <>);
            end loop;
            --  Its copies end unanswered: the queue is gone.
            for c in Copy_Index loop
               copies (q, c) := (others => <>);
            end loop;
            clientQueues (q) := (others => <>);
         end if;
      end loop;
      for p of pendingArenas loop
         if p.owner = owner then
            CuBit.Channels.Close (p.transferLink);
            CuBit.Channels.Close (p.dirtyLink);
            p := (others => <>);
         end if;
      end loop;
   end releaseClientQueue;

   --  A grant event: a client that let go of any of its channels (or died)
   --  is done with all of them, as on its OP_CLOSE, which a full mailbox
   --  may have refused. The kernel's notice always arrives
   --  (docs/ipc-delivery.md).
   procedure clientChannelEnded (event : CuBit.Control_Events.Event) is
      function Ends (link : CuBit.Channels.Channel) return Boolean is
        (CuBit.Channels.Ended (link, event));
   begin
      for c of clientQueues loop
         if c.owner /= No_Process and then
           (Ends (c.queueLink) or else Ends (c.transferLink) or else Ends (c.dirtyLink)
            or else Ends (c.eventLink))
         then
            releaseClientQueue (c.owner);
         end if;
      end loop;
      for p of pendingArenas loop
         if p.owner /= No_Process and then (Ends (p.transferLink) or else Ends (p.dirtyLink)) then
            releaseClientQueue (p.owner);
         end if;
      end loop;
   end clientChannelEnded;

   --  Take the client's reaped index: its answers' slots are free again.
   procedure acceptReaped (q : Client_Queue_Index) is
      consumed : Unsigned_32 with Volatile, Import,
        Address => clientWord (q, FQ.Client_Reaped_At);
      ignore : Boolean;
   begin
      FQueues.Accept_Reaped
        (clientQueues (q).server, FQueues.Completions.Index (consumed), ignore);
   end acceptReaped;

   --  A wake request (FQ.OP_FS_WAKE, a call or an asynchronous submit):
   --  answered now if answers wait, else held until one is posted
   --  (Queue_Wakes). A held one it supersedes is answered first, which
   --  frees its slot for this one.
   procedure handleWake (sender : Process_ID) is
      outcome : Queue_Wakes.Arrival;
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner = sender then
            acceptReaped (q);
            Queue_Wakes.Arrive
              (clientQueues (q).wake,
               clientQueues (q).server.Answers.Fill > 0 or else eventsWaiting (q), outcome);
            if outcome.Answer_Held then
               answerWake (q, REPLY_OK);
            end if;
            if outcome.Answer_Now then
               sendReply (sender, REPLY_OK, 0);
            elsif saveReplyCap (Unsigned_64 (WAKE_REPLY_SLOT_BASE + q)) /= 1 then
               Queue_Wakes.Hold_Failed (clientQueues (q).wake);
               sendReply (sender, REPLY_ERR, 0);
            end if;
            return;
         end if;
      end loop;
      sendReply (sender, REPLY_ERR, 0);
   end handleWake;

   --  Queue_Describe: the open handle's object as one Directory.Inspection.V1
   --  record at the request's arena range. A write-delegated handle's
   --  buffered pages are written first, so size and times include them.
   procedure handleDescribe (owner : Process_ID; handleWord : Unsigned_64) is
      fileHandle : constant Integer := resolveHandle (handleWord, owner, FILE_OBJECT);
      handle : constant Integer :=
        (if fileHandle >= 0 then fileHandle
         else resolveHandle (handleWord, owner, DIRECTORY_OBJECT));
      described : Entry_Inspection :=
        (valid | mode | links | owner | group | reserved => 0,
         sizeBytes | modifiedMs | changedMs | accessedMs | objectId => 0);
      label : Unsigned_32 := REPLY_OK;
   begin
      if handle < 0 then
         sendReply (owner, REPLY_ERR, 0);
         return;
      elsif curArena = System.Null_Address or else
        curArenaBytes < DIRECTORY_INSPECTION_BYTES
      then
         sendReply (owner, REPLY_ERR, 0);
         return;
      end if;
      case files (handle).filesystemKind is
         when EXT2_FILESYSTEM =>
            if fileHandle >= 0 then
               if writeDelegated (handle) then
                  harvest (handle);
               end if;
               described := inspectionOf
                 (Open_Inodes.Value (inodeObjects, Open_Inodes.Owner_Index (handle)),
                  files (handle).volume, files (handle).inodeNum);
            else
               declare
                  ino : Ext2.Inode;
                  inodeStatus : Ext2.Read_Status;
               begin
                  Ext2.readInode
                    (Contexts (files (handle).volume).Fs, files (handle).inodeNum,
                     ino, inodeStatus);
                  if inodeStatus = Ext2.Read_Complete then
                     described := inspectionOf
                       (ino, files (handle).volume, files (handle).inodeNum);
                  else
                     label := REPLY_ERR;
                  end if;
               end;
            end if;
         when CPIO_ARCHIVE =>
            if fileHandle >= 0 then
               described.valid := INSPECTED_SIZE;
               described.sizeBytes :=
                 cpioArchive.files (files (handle).cpioFileIdx).dataSize;
            end if;
         when ISO_FILESYSTEM =>
            if fileHandle >= 0 then
               described.valid := INSPECTED_SIZE;
               described.sizeBytes := Unsigned_64 (extras (handle).opticalFile.Bytes);
            end if;
      end case;
      if label = REPLY_OK then
         declare
            type Record_Bytes is array (1 .. DIRECTORY_INSPECTION_BYTES) of Unsigned_8
              with Component_Size => 8;
            function Bytes_Of is new Ada.Unchecked_Conversion (Entry_Inspection, Record_Bytes);
            --  Byte-aligned: the arena offset is the client's.
            target : Record_Bytes with Import, Address => curArena;
         begin
            target := Bytes_Of (described);
         end;
      end if;
      sendReply (owner, label, 0);
   end handleDescribe;

   --  Handle one request entry of queue q as its message twin would be.
   procedure dispatchEntry (q : Client_Queue_Index; item : FQueues.Submission) is
      owner : constant Process_ID := clientQueues (q).owner;
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
         when FQ.Queue_Park =>
            handlePark (owner, r.Handle);
         when FQ.Queue_Flush =>
            m.tag := (label => OP_FLUSH_FILE, length => 1, flags => 0, reserved => 0);
            m.words (0) := r.Handle;
            handleFlush (owner, m);
         when FQ.Queue_Unlink | FQ.Queue_Mkdir | FQ.Queue_Rmdir =>
            if inArena then
               m.tag := (label => (case r.Operation is
                                     when FQ.Queue_Unlink => OP_UNLINK,
                                     when FQ.Queue_Mkdir  => OP_MKDIR,
                                     when others          => OP_RMDIR),
                         length => 4, flags => 0, reserved => 0);
               m.words := [0 => 0, 1 => r.Length,
                           2 => (if r.Operation = FQ.Queue_Unlink then r.Handle else 0),
                           3 => 1];
               case r.Operation is
                  when FQ.Queue_Unlink => handleUnlink (owner, m);
                  when FQ.Queue_Mkdir  => handleMkdir (owner, m);
                  when others          => handleRmdir (owner, m);
               end case;
            else
               sendReply (owner, REPLY_ERR, 0);
            end if;
            --  An unlink's parked handle is closed whatever became of
            --  the unlink (handleUnlink took it already if it got far).
            if r.Operation = FQ.Queue_Unlink and then r.Handle /= 0 then
               declare
                  parked : constant Integer := resolveHandle (r.Handle, owner, FILE_OBJECT);
               begin
                  if parked >= 0 then
                     writebackFailed (parked) := False;
                     releaseHandle (parked);
                  end if;
               end;
            end if;
         when FQ.Queue_Open_Directory =>
            if inArena then
               m.tag := (label => OP_OPEN_DIRECTORY, length => 3, flags => 0, reserved => 0);
               m.words := [0 => 0, 1 => r.Length, 2 => 1, 3 => 0];
               handleOpenDirectory (owner, m);
            else
               sendReply (owner, REPLY_ERR, 0);
            end if;
         when FQ.Queue_Close_Directory =>
            m.tag := (label => OP_CLOSE_DIRECTORY, length => 1, flags => 0, reserved => 0);
            m.words (0) := r.Handle;
            handleCloseDirectory (owner, m);
         when FQ.Queue_Read_Directory =>
            --  Consecutive Directory.Page.V2 pages until the directory ends
            --  or the range is full.
            declare
               handle : constant Integer := resolveHandle (r.Handle, owner, DIRECTORY_OBJECT);
               pages : constant Natural :=
                 (if inArena then Natural (r.Length / Unsigned_64 (DIRECTORY_PAGE_BYTES)) else 0);
               metadata : constant Boolean := (r.Options and FQ.Directory_Metadata) /= 0;
               filled : Natural := 0;
               label : Unsigned_32 := REPLY_OK;
               pageLabel : Unsigned_32;
               listed : Natural;
               ended : Boolean;
            begin
               if handle < 0 then
                  label := REPLY_WRONG_OBJECT_TYPE;
               elsif pages = 0 or else (r.Options and not FQ.Directory_Metadata) /= 0 then
                  label := REPLY_ERR;
               else
                  for p in 0 .. pages - 1 loop
                     readDirectoryInto
                       (handle, clientQueues (q).arena + Storage_Offset (r.Arena_Offset)
                          + Storage_Offset (p * DIRECTORY_PAGE_BYTES),
                        metadata, pageLabel, listed, ended);
                     if pageLabel /= REPLY_OK then
                        if filled = 0 then
                           label := pageLabel;
                        end if;
                        exit;
                     end if;
                     filled := filled + 1;
                     exit when ended;   --  that page ended the directory
                  end loop;
               end if;
               sendReply (owner, label, Unsigned_64 (filled));
            end;
         when FQ.Queue_Seek_Directory =>
            handleSeekDirectory (owner, r.Handle, r.Position);
         when FQ.Queue_Watch =>
            handleWatch (q, r.Handle, r.Options);
         when FQ.Queue_Unwatch =>
            handleUnwatch (q, r.Handle);
         when FQ.Queue_Copy =>
            handleCopy (q, r);
         when FQ.Queue_Cancel =>
            handleCancel (q, r.Handle);
         when FQ.Queue_List_Scopes =>
            if inArena then
               handleListScopes (owner, r.Length);
            else
               sendReply (owner, REPLY_ERR, 0);
            end if;
         when FQ.Queue_Describe_Volume =>
            if inArena then
               handleDescribeVolume (owner, r.Position, r.Length);
            else
               sendReply (owner, REPLY_ERR, 0);
            end if;
         when FQ.Queue_Rename =>
            --  Both paths in the arena range, split at Position; the
            --  handler copies them out before it checks either.
            if inArena and then r.Length >= 2 and then r.Position in 1 .. r.Length - 1 then
               m.tag := (label => OP_RENAME, length => 4, flags => 0, reserved => 0);
               m.words := [0 => 0, 1 => r.Position, 2 => r.Length - r.Position, 3 => 1];
               handleRename (owner, m);
            else
               sendReply (owner, REPLY_ERR, 0);
            end if;
         when FQ.Queue_Resize =>
            m.tag := (label => OP_RESIZE_FILE, length => 2, flags => 0, reserved => 0);
            m.words (0) := r.Handle;
            m.words (1) := r.Length;
            handleResize (owner, m);
         when FQ.Queue_Describe =>
            if inArena then
               handleDescribe (owner, r.Handle);
            else
               sendReply (owner, REPLY_ERR, 0);
            end if;
         when FQ.Queue_Writeback =>
            declare
               handle : constant Integer := resolveHandle (r.Handle, owner, FILE_OBJECT);
            begin
               if handle >= 0 and then writeDelegated (handle) then
                  --  The arena is shared by the client's handles: free it
                  --  of all of them.
                  harvestOwner (q);
                  sendReply (owner, REPLY_OK, 0);
               else
                  sendReply (owner, REPLY_ERR, 0);
               end if;
            end;
         when others =>
            sendReply (owner, REPLY_ERR, 0);
      end case;
      curRoute := (others => <>);
      curArena := System.Null_Address;
      curArenaBytes := 0;
      --  The poll window runs from the last request handled.
      lastQueueActivity := CuBit.Busy_Poll.Now;
   end dispatchEntry;

   --  Take queue q's requests and handle each.
   procedure serviceQueue (q : Client_Queue_Index) is
      produced : Unsigned_32 with Volatile, Import,
        Address => clientWord (q, FQ.Client_Submitted_At);
      consumed : Unsigned_32 with Volatile, Import,
        Address => serverWord (q, FQ.Server_Taken_At);
      wake : Unsigned_32 with Volatile, Import,
        Address => serverWord (q, FQ.Server_Wake_At);
      requests : constant FQueues.Submissions.Ring with Import,
        Address => clientWord (q, FQ.Client_Requests_At);
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


   function queuesPending return Boolean is
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner /= No_Process then
            declare
               produced : Unsigned_32 with Volatile, Import,
                 Address => clientWord (q, FQ.Client_Submitted_At);
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
            if clientQueues (q).owner /= No_Process then
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
         if clientQueues (q).owner /= No_Process then
            declare
               produced : Unsigned_32 with Volatile, Import,
                 Address => clientWord (q, FQ.Client_Submitted_At);
               wake : Unsigned_32 with Volatile, Import,
                 Address => serverWord (q, FQ.Server_Wake_At);
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

   --  The periodic commit (Linux's commit=5): clients' buffered writes are
   --  harvested, and each volume's journal commits its dirty blocks.
   COMMIT_INTERVAL_MS : constant := 5_000;
   nextCommit : Unsigned_64 := 0;
   received   : Boolean := False;

   procedure commitTick is
      status : Ext2.Flush_Status;
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner /= No_Process then
            harvestOwner (q);
         end if;
      end loop;
      for slot in files'Range loop
         if files (slot).active and then files (slot).objectKind = FILE_OBJECT then
            reportModified (slot);
         end if;
      end loop;
      settleWatches;
      for V in Contexts'Range loop
         if Contexts (V).Status = Admitted and then Ext2.dirtyBlocks (Contexts (V).Fs) then
            Ext2.Flush (Contexts (V).Fs, status);
            if status /= Ext2.Flush_Complete then
               debugPrint ("FS: periodic commit failed" & LF);
            end if;
         end if;
      end loop;
   end commitTick;

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
          words    => (others => 0)), CuBit.Messages.Wait_Forever);
   end;

   CuBit.Busy_Poll.Calibrate;   --  queue polling windows are timed by the TSC
   debugPrint ("FS Server: Entering message loop." & LF);

   --  Main IPC message loop.
   --  Uses blocking receive for lowest latency.  Future async work
   --  can switch to Poll_Service_Request + Poll_Completion when capSubmit-based
   --  driver I/O is implemented.
   nextCommit := syscall (SYSCALL_GETTIME) + COMMIT_INTERVAL_MS;
   loop
      --  Queued requests first; block only when none wait, and at most
      --  until the next journal commit.
      serviceClientQueues;
      settleWatches;
      runCopies;
      if not armClientQueues then
      --  Copies in progress: look for messages without waiting.
      receiveUntil ((if copiesActive then 0 else nextCommit), sender, msg, received);
      if received then
      case msg.tag.label is
         when CuBit.Channel_Protocol.OP_OPEN_PRODUCING
            | CuBit.Channel_Protocol.OP_OPEN_CONSUMING =>   --  consuming: the event ring
            handleChannelOpen (sender, msg);
         when CuBit.Channel_Protocol.OP_KICK =>
            null;   --  one-way: the queue is serviced at the loop's top
         when CuBit.Channel_Protocol.OP_CLOSE =>
            --  One-way: a client let go of its queue.
            releaseClientQueue (sender);
         when CuBit.Control_Events.Grant_Revoked_Label
            | CuBit.Control_Events.Grant_Returned_Label =>
            --  Only the kernel posts these (it refuses them from a process).
            if sender = No_Process then
               clientChannelEnded
                 (CuBit.Control_Events.Decode
                    (msg.tag.label, msg.tag.length, msg.words (0), msg.words (1), msg.words (2)));
            end if;
         when FQ.OP_FS_WAKE =>
            handleWake (sender);
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
         when OP_RELEASE_OWNER =>
            handleReleaseOwner (sender, msg);
         when OP_RENAME =>
            handleRename (sender, msg);
         when OP_UNLINK =>
            handleUnlink (sender, msg);
         when OP_MKDIR =>
            handleMkdir (sender, msg);
         when OP_RMDIR =>
            handleRmdir (sender, msg);
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
      end if;
      end if;
      if syscall (SYSCALL_GETTIME) >= nextCommit then
         commitTick;
         nextCommit := syscall (SYSCALL_GETTIME) + COMMIT_INTERVAL_MS;
      end if;
   end loop;
end main;

with Fs_Server.Delegations; use Fs_Server.Delegations;
with Fs_Server.Watches; use Fs_Server.Watches;
with Fs_Server.Copies; use Fs_Server.Copies;

package body Fs_Server.Handles is

   use ASCII;
   use type Codes.Generation;
   use type Ext2.Remove_Status;

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
               handle := Unsigned_64 (Codes.Encode (Codes.Handle_Slot (j), files (j).generation));
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
               handle := Unsigned_64 (Codes.Encode (Codes.Handle_Slot (i), files (i).generation));
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
      word : constant Codes.Handle_Word := Codes.Handle_Word (handle);
      slot : Handle_Slot;
   begin
      if not Codes.Valid_Slot_Code (word) then
         return -1;
      end if;

      slot := Handle_Slot (Codes.Slot_Of (word));
      if not files (slot).active or else
         files (slot).retired or else
         files (slot).generation /= Codes.Generation_Of (word) or else
         files (slot).ownerPID /= sender or else
         files (slot).objectKind /= expectedKind
      then
         return -1;
      end if;

      return slot;
   end resolveHandle;

   --  Versions of files seen during this service's life. An entry reused
   --  for another file gives the old one a fresh version when it is seen
   --  again, so no client keeps pages of an older version. Entries are
   --  found by hashing the identity to a home entry and probing a short
   --  window after it (Fs_Handle_Codes); a miss takes a free entry of the
   --  window, or evicts one round-robin.
   type Inode_Version is record
      used    : Boolean := False;
      key     : Inode_Identity := (Volume_Index'First, 0);
      version : Unsigned_64 := 0;
   end record;
   inodeVersions : array (Codes.Version_Slot) of Inode_Version;
   versionClock : Unsigned_64 := 0;
   nextVersionProbe : Codes.Version_Probe := Codes.Version_Probe'First;

   function versionSlotOf (key : Inode_Identity) return Codes.Version_Slot is
      home  : constant Codes.Version_Slot := Codes.Version_Home (key.volume, key.number);
      fresh : Codes.Version_Slot := Codes.Probe (home, nextVersionProbe);
   begin
      for p in Codes.Version_Probe loop
         declare
            i : constant Codes.Version_Slot := Codes.Probe (home, p);
         begin
            if inodeVersions (i).used and then inodeVersions (i).key = key then
               return i;
            end if;
         end;
      end loop;
      for p in Codes.Version_Probe loop
         if not inodeVersions (Codes.Probe (home, p)).used then
            fresh := Codes.Probe (home, p);
            exit;
         end if;
      end loop;
      nextVersionProbe := Codes.Next_Probe (nextVersionProbe);
      versionClock := versionClock + 1;
      inodeVersions (fresh) := (used => True, key => key, version => versionClock);
      return fresh;
   end versionSlotOf;

   function versionOf (key : Inode_Identity) return Unsigned_64 is
     (inodeVersions (versionSlotOf (key)).version);

   procedure bumpVersion (key : Inode_Identity) is
      i : constant Codes.Version_Slot := versionSlotOf (key);
   begin
      versionClock := versionClock + 1;
      inodeVersions (i).version := versionClock;
   end bumpVersion;

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

   procedure releaseHandle (slot : Integer) is
      --  Last-close reclaim of an unlinked file.
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
      if Codes.Retires (files (slot).generation) then
         files (slot).retired := True;
      else
         files (slot).generation := Codes.Next (files (slot).generation);
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

end Fs_Server.Handles;

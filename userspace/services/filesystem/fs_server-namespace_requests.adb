with CuBit.Filesystems; use CuBit.Filesystems;
with Fs_Path_Checks;
with Fs_Reply_Labels;
with Fs_Server.Access_Control; use Fs_Server.Access_Control;
with Fs_Server.Delegations; use Fs_Server.Delegations;
with Fs_Server.Handles; use Fs_Server.Handles;
with Fs_Server.Replies; use Fs_Server.Replies;
with Fs_Server.Storage; use Fs_Server.Storage;
with Fs_Server.Watches; use Fs_Server.Watches;

package body Fs_Server.Namespace_Requests is

   use type Ext2.Read_Status, Ext2.Directory_Lookup_Status, Ext2.Rename_Status;
   use ASCII;
   package Labels renames Fs_Reply_Labels;

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
         if Fs_Path_Checks.Has_Traversal (oldPath) or else Fs_Path_Checks.Has_Traversal (newPath) then
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
               sendReply (sender, Labels.Admission_Label (admission), 0);
               return;
            end if;
            --  Do not rename an exclusively held inode. Check identity after
            --  authority, before any mutation;
            --  path spelling and caller PID cannot bypass an exclusive hold.
            Ext2.resolvePath
              (Contexts (Volume_Index (oldVolume)).Fs,
               oldPath (oldRelStart .. oldPath'Last), inodeNum, lookup);
            if lookup not in Ext2.Lookup_Found | Ext2.Lookup_Not_Found then
               sendReply (sender, Labels.Lookup_Label (lookup), 0);
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
               sendReply (sender, Labels.Lookup_Label (lookup), 0);
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
               label := Labels.Rename_Label (status);
               if status = Ext2.Rename_Recovery_Required then
                  debugPrint ("FS Server: rename rollback failed; volume write-quarantined" & LF);
               end if;
               if label = REPLY_OK then
                  declare
                     moved : Ext2.Inode;
                     movedStatus : Ext2.Read_Status;
                     isDirectory : Boolean := False;
                     cookie : Unsigned_64;
                  begin
                     Ext2.readInode (Contexts (volume).Fs, inodeNum, moved, movedStatus);
                     if movedStatus = Ext2.Read_Complete then
                        isDirectory := Ext2.inodeType (moved) = Ext2.INODE_DIRECTORY;
                     end if;
                     cookie := nextRenameCookie;
                     notify (volume, oldPath (oldRelStart .. oldPath'Last), FE.Renamed_From,
                             isDirectory, objectOf ((volume, inodeNum)), cookie);
                     notify (volume, newPath (newRelStart .. newPath'Last), FE.Renamed_To,
                             isDirectory, objectOf ((volume, inodeNum)), cookie);
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
         if Fs_Path_Checks.Has_Traversal (path) then
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
         sendReply (sender, Labels.Admission_Label (admission), 0);
         return;
      end if;
      --  unlink, mkdir, rmdir: names change from here on.
      bumpNamespace;
      ok := True;
   end namespaceRequest;

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
            sendReply (sender, Labels.Lookup_Label (lookup), 0);
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
      if Labels.Remove_Label (status) = REPLY_OK then
         notify (volume, pathBuffer (relStart .. pathLen), FE.Removed, False,
                 objectOf ((volume, target)));
      end if;
      sendReply (sender, Labels.Remove_Label (status), 0);
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
         sendReply (sender, Labels.Lookup_Label (lookup), 0);
      else
         if Labels.Write_Label (status) = REPLY_OK then
            notify (volume, pathBuffer (relStart .. pathLen), FE.Created, True,
                    objectOf ((volume, created)));
         end if;
         sendReply (sender, Labels.Write_Label (status), Unsigned_64 (created));
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
            sendReply (sender, Labels.Lookup_Label (lookup), 0);
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
         if Labels.Remove_Label (status) = REPLY_OK then
            notify (volume, relPath, FE.Removed, True, objectOf ((volume, target)));
         end if;
      end;
      sendReply (sender, Labels.Remove_Label (status), 0);
   end handleRmdir;

end Fs_Server.Namespace_Requests;

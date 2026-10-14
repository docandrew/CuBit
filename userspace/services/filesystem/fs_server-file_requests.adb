with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Grant_References;
with Ext2_Support;
with ISO9660;
with Fs_Path_Checks;
with Fs_Reply_Labels;
with Fs_Server.Access_Control; use Fs_Server.Access_Control;
with Fs_Server.Delegations; use Fs_Server.Delegations;
with Fs_Server.Directory_Requests; use Fs_Server.Directory_Requests;
with Fs_Server.Handles; use Fs_Server.Handles;
with Fs_Server.Replies; use Fs_Server.Replies;
with Fs_Server.Storage; use Fs_Server.Storage;
with Fs_Server.Watches; use Fs_Server.Watches;

package body Fs_Server.File_Requests is

   use type Ext2.Read_Status, Ext2.Write_Status, Ext2.Truncate_Status, Ext2.Flush_Status, Ext2.Directory_Lookup_Status;
   use ASCII;
   package Labels renames Fs_Reply_Labels;
   use type Open_Inodes.Attach_Result;

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
         if Fs_Path_Checks.Has_Traversal (pathStr) then
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
         sendReply (sender, Labels.Lookup_Label (pathStatus), 0);
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
               fileStart : constant Natural := relPath'First;
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
                     sendReply (sender, Labels.Lookup_Label (pathStatus), 0);
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
                     sendReply (sender, Labels.Write_Label (createStatus), 0);
                     return;
                  end if;
                  --  A new file under a number an old one may have had.
                  bumpVersion ((useVolume, inodeNum));
                  notify (useVolume, relPath, FE.Created, False,
                          objectOf ((useVolume, inodeNum)));
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
               sendReply (sender, Labels.Truncate_Label (truncateStatus), 0);
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
            keepNoticePath (handle, pathBuffer (relStart .. Natural (pathLen)),
                            pending => (openFlags and OPEN_TRUNCATE) /= 0);
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
                          protocol  => 0, ipc => <>,
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

      sendReply (sender, Labels.Read_Label (readStatus), bytesRead);
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

      sendReply (sender, Labels.Write_Label (writeStatus), bytesWritten);
   end handleWrite;

   --  Decode transport metadata once, then use the identical authorization,
   --  grant acquisition, filesystemKind I/O and return path as cursor-based requests.
   procedure handlePositioned (sender : Process_ID; msg : Message) is
      request : Message := msg;
      loan : CuBit.Grant_References.Reference;
   begin
      if msg.tag.length /= 4 or else msg.tag.protocol /= 0 or else
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
      if msg.tag.length /= 1 or else msg.tag.protocol /= 0 or else
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
      sendReply (sender, Labels.Flush_Label (status), 0);
   end handleFlush;

   procedure handleResize (sender : Process_ID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
      updated : Ext2.Inode;
      status : Ext2.Truncate_Status;
   begin
      if msg.tag.length /= 2 or else msg.tag.protocol /= 0 or else
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
         when others =>
            sendReply (sender, Labels.Truncate_Label (status), 0);
      end case;
   end handleResize;

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

end Fs_Server.File_Requests;

with CuBit.Directory_Pages;
with Fs_Path_Checks;
with Fs_Reply_Labels;
with Fs_Server.Access_Control; use Fs_Server.Access_Control;
with Fs_Server.Handles; use Fs_Server.Handles;
with Fs_Server.Replies; use Fs_Server.Replies;
with Fs_Server.Storage; use Fs_Server.Storage;

package body Fs_Server.Directory_Requests is

   use type Ext2.Read_Status, Ext2.Directory_Lookup_Status;
   use type Codes.Directory_Check;
   package Labels renames Fs_Reply_Labels;

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
            if Fs_Path_Checks.Has_Traversal (path) then
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
                  sendReply (sender, Labels.Admission_Label (admission), 0);
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
         sendReply (sender, Labels.Lookup_Label (pathStatus), 0);
         return;
      end if;

      if filesystemKind /= CPIO_ARCHIVE then
         case filesystemKind is
            when EXT2_FILESYSTEM =>
               Ext2.readInode (Contexts (selectedVolume).Fs, inodeNum, dirIno, inodeStatus);
            when CPIO_ARCHIVE | ISO_FILESYSTEM => null;
         end case;
         if inodeStatus /= Ext2.Read_Complete then
            sendReply (sender, Labels.Directory_Inode_Label (inodeStatus), 0);
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
           (label => REPLY_OK, length => 2, protocol => 0, ipc => <>, reserved => 0);
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
            lookupReply := Labels.Read_Label (readStatus);
            return;
         elsif Ext2.inodeType (parentInode) /= Ext2.INODE_DIRECTORY then
            lookupReply := REPLY_WRONG_OBJECT_TYPE;
            return;
         end if;
         Ext2.lookupInDir
           (fs, parentInode, nameBuffer (1 .. Natural (nameLength)),
            inodeNum, lookupStatus);
         if lookupStatus = Ext2.Lookup_Found then
            Ext2.readInode (fs, inodeNum, ino, readStatus);
            lookupReply := Labels.Read_Label (readStatus);
            if readStatus /= Ext2.Read_Complete then
               inodeNum := 0;
            end if;
         else
            lookupReply := Labels.Child_Lookup_Label (lookupStatus);
         end if;
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
         sendReply (sender, Labels.Read_Label (status), 0);
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
   --  seconds since the epoch; i_ctime is the change time. An inode made
   --  while wall time was unknown keeps a zero mtime (Ext2.Metadata.Stamp):
   --  its times are reported as unknown, not as 1970-01-01.
   function inspectionOf
     (ino : Ext2.Inode; volume : Volume_Index; inodeNum : Unsigned_32)
      return Entry_Inspection
   is
      MS_PER_SECOND : constant := 1_000;
      NEVER_STAMPED : constant Unsigned_32 := 0;
      function Ms (seconds : Unsigned_32) return Unsigned_64 is
        (Unsigned_64 (seconds) * MS_PER_SECOND);
      Times : constant Unsigned_32 :=
        (if ino.modifiedTime = NEVER_STAMPED then 0 else INSPECTED_TIMES);
   begin
      return
        (valid => INSPECTED_SIZE or Times or INSPECTED_MODE or
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
         objectId => Codes.Object_Id (volume, inodeNum));
   end inspectionOf;

   --  Directory resume tokens (Fs_Handle_Codes): the check naming the
   --  handle's directory, and a token for a position in it.
   function directoryCheck (handle : Natural) return Codes.Directory_Check is
     (Codes.Check_Of
        (files (handle).inodeNum, files (handle).volume,
         (if files (handle).filesystemKind = EXT2_FILESYSTEM
          then extras (handle).directoryInode.generationNumber else 0)));

   function resumeToken (handle : Natural; position : Unsigned_64) return Unsigned_64 is
     (Unsigned_64 (Codes.Token_Of (directoryCheck (handle), position)));

   procedure handleSeekDirectory (owner : Process_ID; handleWord, token : Unsigned_64) is
      handle : constant Integer := resolveHandle (handleWord, owner, DIRECTORY_OBJECT);
   begin
      if handle < 0 then
         sendReply (owner, REPLY_WRONG_OBJECT_TYPE, 0);
      elsif token = 0 then
         files (handle).offset := 0;
         sendReply (owner, REPLY_OK, 0);
      elsif Codes.Check_In (Codes.Resume_Token (token)) /= directoryCheck (handle) then
         sendReply (owner, REPLY_OUT_OF_RANGE, 0);
      else
         files (handle).offset := Codes.Position_In (Codes.Resume_Token (token));
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
         label := Labels.Directory_Read_Label (readStatus);
         atEnd := Ext2."=" (readStatus, Ext2.Directory_End);
         if label = REPLY_OK then
            for index in 0 .. count - 1 loop
               declare
                  item : constant Ext2.Listed_Record := records (index);
                  facts : DP.Facts :=
                    (Kind => item.kind, Valid => DP.Valid_Object,
                     Object => Codes.Object_Id (files (handle).volume, item.inode),
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

end Fs_Server.Directory_Requests;

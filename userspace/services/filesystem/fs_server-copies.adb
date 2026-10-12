with CuBit.Filesystems; use CuBit.Filesystems;
with Copy_Slices;
with ISO9660;
with Fs_Reply_Labels;
with Fs_Server.Delegations; use Fs_Server.Delegations;
with Fs_Server.Handles; use Fs_Server.Handles;
with Fs_Server.Replies; use Fs_Server.Replies;

package body Fs_Server.Copies is

   use type Ext2.Read_Status, Ext2.Write_Status;
   package Labels renames Fs_Reply_Labels;

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
               label := Labels.Write_Label (writeStatus);
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

   procedure forgetCopies (q : Client_Queue_Index) is
   begin
      for c in Copy_Index loop
         copies (q, c) := (others => <>);
      end loop;
   end forgetCopies;

end Fs_Server.Copies;

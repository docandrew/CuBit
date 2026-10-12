with CuBit.File_Access;
with CuBit.Filesystems; use CuBit.Filesystems;
with Fs_Server.Client_Queues; use Fs_Server.Client_Queues;
with Fs_Server.Delegations; use Fs_Server.Delegations;
with Fs_Server.Handles; use Fs_Server.Handles;
with Fs_Server.Replies; use Fs_Server.Replies;

package body Fs_Server.Access_Control is

   use ASCII;

   ---------------------------------------------------------------------------
   --  Per-process file ACL infrastructure
   ---------------------------------------------------------------------------
   MAX_ACL_ENTRIES : constant := CuBit.File_Access.Maximum_Entries;
   MAX_ACL_PROFILES : constant := 32;

   type ACLProfile is record
      pid    : Process_ID := No_Process;
      active : Boolean   := False;
      policy : CuBit.File_Access.Policy;
   end record;

   aclProfiles : array (0 .. MAX_ACL_PROFILES - 1) of ACLProfile;

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

end Fs_Server.Access_Control;

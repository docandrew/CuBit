with CuBit.Filesystems; use CuBit.Filesystems;
with File_Locks;
with Fs_Server.Handles; use Fs_Server.Handles;
with Fs_Server.Replies; use Fs_Server.Replies;

package body Fs_Server.Locks is

   package Table_Locks is new File_Locks (Capacity => MAX_OPEN_FILES);
   use type Table_Locks.Outcome;
   use type Table_Locks.Mode;

   pragma Compile_Time_Error
     (Table_Locks.No_Deadline /= Wait_Forever, "a wait without deadline is Wait_Forever");

   table : Table_Locks.Table;
   --  Where each waiting handle's answer goes.
   waitRoutes : array (Handle_Slot) of Reply_Route;

   LOCK_MODES : constant Unsigned_32 := FQ.Lock_Shared + FQ.Lock_Exclusive;
   LOCK_OPTIONS : constant Unsigned_32 := LOCK_MODES + FQ.Lock_Wait;

   function keyOf (slot : Handle_Slot) return Table_Locks.Object_Key is
     (Table_Locks.Object_Key (objectOf ((files (slot).volume, files (slot).inodeNum))));

   --  Answer slot's wait (once) on its queue.
   procedure answerWait (slot : Handle_Slot; label : Unsigned_32) is
      saved : constant Reply_Route := curRoute;
   begin
      if waitRoutes (slot).queue /= No_Client_Queue then
         curRoute := waitRoutes (slot);
         answerEntry (label, 0, 0);
         curRoute := saved;
      end if;
      waitRoutes (slot) := (others => <>);
   end answerWait;

   --  Grant the waiters of a file in arrival order while they fit.
   procedure settle (object : Table_Locks.Object_Key) is
      granted : Table_Locks.Slot;
      found : Boolean;
   begin
      for round in Handle_Slot loop
         Table_Locks.Settle (table, object, granted, found);
         exit when not found;
         answerWait (granted, REPLY_OK);
      end loop;
   end settle;

   procedure handleLock (q : Client_Queue_Index; r : FQ.Request) is
      owner : constant Process_ID := clientQueues (q).owner;
      slot : constant Integer := resolveHandle (r.Handle, owner, FILE_OBJECT);
      modes : constant Unsigned_32 := r.Options and LOCK_MODES;
      result : Table_Locks.Outcome;
      released : Boolean;
   begin
      if slot < 0 then
         sendReply (owner, REPLY_WRONG_OBJECT_TYPE, 0);
         return;
      elsif (r.Options and not LOCK_OPTIONS) /= 0
        or else (modes /= FQ.Lock_Shared and then modes /= FQ.Lock_Exclusive)
        or else Table_Locks.Waiting (table, slot)
      then
         sendReply (owner, REPLY_ERR, 0);
         return;
      elsif files (slot).filesystemKind /= EXT2_FILESYSTEM then
         sendReply (owner, REPLY_UNSUPPORTED_OBJECT, 0);
         return;
      end if;
      Table_Locks.Request
        (table, slot, keyOf (slot),
         (if modes = FQ.Lock_Shared then Table_Locks.Shared else Table_Locks.Exclusive),
         Wait => (r.Options and FQ.Lock_Wait) /= 0, Until_At => r.Spare_2,
         Result => result, Released => released);
      case result is
         when Table_Locks.Granted =>
            sendReply (owner, REPLY_OK, 0);
         when Table_Locks.Busy =>
            sendReply (owner, REPLY_BUSY, 0);
         when Table_Locks.Queued =>
            waitRoutes (slot) := curRoute;   --  answered when the wait ends
      end case;
      if released then
         settle (keyOf (slot));
      end if;
   end handleLock;

   procedure endHandle (slot : Handle_Slot) is
      wasHeld, wasWaiting : Boolean;
   begin
      Table_Locks.Release (table, slot, wasHeld, wasWaiting);
      if wasWaiting then
         answerWait (slot, REPLY_CANCELLED);
      end if;
      if wasHeld or else wasWaiting then
         settle (Table_Locks.Object_Of (table, slot));
      end if;
   end endHandle;

   procedure handleUnlock (q : Client_Queue_Index; handleWord : Unsigned_64) is
      owner : constant Process_ID := clientQueues (q).owner;
      slot : constant Integer := resolveHandle (handleWord, owner, FILE_OBJECT);
   begin
      if slot < 0 then
         sendReply (owner, REPLY_WRONG_OBJECT_TYPE, 0);
         return;
      end if;
      endHandle (slot);
      sendReply (owner, REPLY_OK, 0);
   end handleUnlock;

   procedure lockHandleGone (slot : Natural) is
   begin
      if slot in Handle_Slot and then
        (Table_Locks.Held (table, slot) /= Table_Locks.Unlocked
         or else Table_Locks.Waiting (table, slot))
      then
         endHandle (slot);
      end if;
   end lockHandleGone;

   procedure forgetLockWaits (q : Client_Queue_Index) is
      wasWaiting : Boolean;
   begin
      if table.Waiting_Count = 0 then
         return;
      end if;
      for slot in Handle_Slot loop
         if waitRoutes (slot).queue = q then
            waitRoutes (slot) := (others => <>);
            Table_Locks.Cancel_Wait (table, slot, wasWaiting);
            if wasWaiting then
               settle (Table_Locks.Object_Of (table, slot));
            end if;
         end if;
      end loop;
   end forgetLockWaits;

   procedure expireLockWaits is
      expired : Table_Locks.Slot;
      found : Boolean;
      now : Unsigned_64;
   begin
      if table.Waiting_Count = 0 then
         return;
      end if;
      now := syscall (SYSCALL_GETTIME);
      for round in Handle_Slot loop
         Table_Locks.Expire (table, now, expired, found);
         exit when not found;
         answerWait (expired, REPLY_DEADLINE);
         settle (Table_Locks.Object_Of (table, expired));
      end loop;
   end expireLockWaits;

   function nextLockDeadline return Unsigned_64 is
     (if table.Waiting_Count = 0 then Wait_Forever else Table_Locks.Earliest_Deadline (table));

end Fs_Server.Locks;

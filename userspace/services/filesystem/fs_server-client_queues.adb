with System.Machine_Code;
with CuBit.Busy_Poll;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Protocols;
with Fs_Server.Copies; use Fs_Server.Copies;
with Fs_Server.Directory_Requests; use Fs_Server.Directory_Requests;
with Fs_Server.File_Requests; use Fs_Server.File_Requests;
with Fs_Server.Handles; use Fs_Server.Handles;
with Fs_Server.Namespace_Requests; use Fs_Server.Namespace_Requests;
with Fs_Server.Replies; use Fs_Server.Replies;
with Fs_Server.Storage; use Fs_Server.Storage;
with Fs_Server.Watches; use Fs_Server.Watches;
with Fs_Server.Delegations; use Fs_Server.Delegations;
with Fs_Server.Access_Control; use Fs_Server.Access_Control;

package body Fs_Server.Client_Queues is

   use ASCII;
   use type FQueues.Submissions.Index;

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
            forgetWatches (q);
            --  Its copies end unanswered: the queue is gone.
            forgetCopies (q);
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

end Fs_Server.Client_Queues;

with CuBit.Channels;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Channel_Rings;
with Fs_Path_Checks;
with Fs_Server.Access_Control; use Fs_Server.Access_Control;
with Fs_Server.Handles; use Fs_Server.Handles;
with Fs_Server.Replies; use Fs_Server.Replies;

package body Fs_Server.Watches is

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


   function nextRenameCookie return Unsigned_64 is
   begin
      renameCookie := renameCookie + 1;
      return renameCookie;
   end nextRenameCookie;

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
      rel : constant String := Fs_Path_Checks.Canonical (path);
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

   procedure keepNoticePath (slot : Natural; rel : String; pending : Boolean) is
      kept : constant String := Fs_Path_Checks.Canonical (rel);
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
            exit;
         end if;
      end loop;
      modifiedPending (slot) := pending;
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
                    objectOf ((files (slot).volume, files (slot).inodeNum)));
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
      from : constant String := Fs_Path_Checks.Canonical (before);
      to : constant String := Fs_Path_Checks.Canonical (after);
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
              Fs_Path_Checks.Canonical (path (Integer'Max (relFirst, path'First) .. path'Last));
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

   procedure forgetWatches (q : Client_Queue_Index) is
   begin
      for w in FE.Watch_Number loop
         watches (q, w) := (others => <>);
      end loop;
   end forgetWatches;

end Fs_Server.Watches;

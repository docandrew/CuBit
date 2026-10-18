with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Async_Requests;
with CuBit.Filesystem_Queues;
with CuBit.Filesystems;
with CuBit.Monotonic;
with CuBit.UI.App;
with CuBit.UI.Clipboard;

package body Text_Host_Native is
   package FQ renames CuBit.Filesystem_Queues;
   package Files renames CuBit.Filesystems;
   use type FS.Token;
   use type Files.Open_Options;
   use type CuBit.UI.Clipboard.Outcome;

   --  The transfer arena: the load's path, the save's two paths (a rename
   --  names both, back to back), then a data area for each.
   LOAD_PATH_AT : constant := 0;
   SAVE_PATH_AT : constant := MAXIMUM_NAME;
   DATA_AT : constant := 3 * MAXIMUM_NAME;
   PAGE_BYTES : constant := 4_096;
   SAVE_SUFFIX : constant String := ".text-save";


   function Half (H : Host) return Unsigned_64 is
     (if FS.Is_Open (H.Session) and then FS.Arena_Bytes (H.Session) > DATA_AT + 2 * PAGE_BYTES
      then (FS.Arena_Bytes (H.Session) - DATA_AT) / 2 / PAGE_BYTES * PAGE_BYTES else PAGE_BYTES);
   function Load_Data_At (H : Host) return Unsigned_64 is (DATA_AT);
   function Save_Data_At (H : Host) return Unsigned_64 is (DATA_AT + Half (H));

   function Chunk_Limit (H : Host) return Positive is (Positive (Half (H)));

   procedure Open (H : in out Host; Endpoint : CuBit.Handles.Endpoint; Ready : out Boolean) is
   begin
      FS.Open (H.Session, Endpoint, FQ.Transfer_Pages, Ready);
      H.Paste := new String (1 .. PASTE_BYTES);
   end Open;

   procedure Close (H : in out Host) is
   begin
      if FS.Is_Open (H.Session) then
         FS.Close (H.Session);
      end if;
   end Close;

   procedure Set_Surface (H : in out Host; Surface : Unsigned_64) is
   begin
      H.Surface := Surface;
   end Set_Surface;

   procedure Offer (H : in out Host; Name : String) is
   begin
      if H.Offered_Count < MAXIMUM_OFFERED and then Name'Length <= MAXIMUM_NAME then
         H.Offered_Count := H.Offered_Count + 1;
         H.Offered (H.Offered_Count).Length := Name'Length;
         H.Offered (H.Offered_Count).Text (1 .. Name'Length) := Name;
      end if;
   end Offer;

   function Last_Reply (H : Host) return Unsigned_32 is (H.Reply);
   function Waiting (H : Host) return Boolean is (FS.Is_Open (H.Session) and then FS.Outstanding (H.Session) > 0);

   procedure Arm_Wake (H : in out Host) is
      Accepted : Boolean;
   begin
      if Waiting (H) and then not FS.Wake_Armed (H.Session) then
         H.Wake_Token := (if H.Wake_Token = 0 then CuBit.UI.App.APPLICATION_TOKEN_FIRST else H.Wake_Token + 1);
         FS.Arm_Wake (H.Session, CuBit.Async_Requests.Token (H.Wake_Token), Accepted);
      end if;
   end Arm_Wake;

   procedure Complete (H : in out Host; Receipt : CuBit.Messages.CompletionEntry; Consumed : out Boolean) is
      Woken : Boolean;
   begin
      Consumed := False;
      if FS.Is_Open (H.Session) then
         FS.Complete_Wake (H.Session, Receipt, Consumed, Woken);
      end if;
   end Complete;

   --  Copies between private memory and the shared arena (copy first;
   --  what the service wrote is only data).
   procedure Put (H : Host; Offset : Unsigned_64; Bytes : String) is
      Target : String (1 .. Bytes'Length)
        with Import, Address => FS.Arena (H.Session) + Storage_Offset (Offset);
   begin
      Target := Bytes;
   end Put;

   procedure Take (H : Host; Offset : Unsigned_64; Into : out String) is
      Source : constant String (1 .. Into'Length)
        with Import, Address => FS.Arena (H.Session) + Storage_Offset (Offset);
   begin
      Into := Source;
   end Take;

   function Clock_Us return Unsigned_64 is
      Reading : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (if Reading.Available then Reading.Microseconds else 0);
   end Clock_Us;

   function Last_Load (H : Host) return Load_Times is (H.Times);

   --  Where a submitted request's token is kept.
   type Tag_Slot is (Load_Slot, Save_Slot, No_Slot);

   function Submit (H : in out Host; Request : FQ.Request; Slot : Tag_Slot) return Boolean is
      Tag : FS.Token := 0;
   begin
      if not FS.Is_Open (H.Session) or else not FS.Can_Submit (H.Session) then
         return False;
      end if;
      FS.Submit (H.Session, Request, Tag);
      case Slot is
         when Load_Slot => H.Load_Tag := Tag;
         when Save_Slot => H.Save_Tag := Tag;
         when No_Slot => null;
      end case;
      return True;
   end Submit;

   procedure Fire (H : in out Host; Request : FQ.Request) is
      Ignored : Boolean;
   begin
      Ignored := Submit (H, Request, No_Slot);
   end Fire;

   --  Reap every answer that waits and keep the load's and the save's.
   procedure Drain (H : in out Host) is
      Item : FS.Q.Completion;
      Got : Boolean;
   begin
      if not FS.Is_Open (H.Session) then
         return;
      end if;
      loop
         FS.Reap (H.Session, Item, Got);
         exit when not Got;
         if H.Load_Tag /= 0 and then Item.Tag = H.Load_Tag then
            if H.Load = Load_Opening then
               H.Times.Open_Reaped := Clock_Us;
            elsif H.Times.Read_Reaped = 0 then
               H.Times.Read_Reaped := Clock_Us;
            end if;
            H.Load_Answer := (True, Item.Answer.Status, Item.Answer.Value);
            H.Load_Tag := 0;
         elsif H.Save_Tag /= 0 and then Item.Tag = H.Save_Tag then
            H.Save_Answer := (True, Item.Answer.Status, Item.Answer.Value);
            H.Save_Tag := 0;
         end if;
      end loop;
   end Drain;

   function Status_Of (Reply : Unsigned_32) return Text_Hosts.File_Status is
     (if Reply = Files.REPLY_NOT_FOUND then Text_Hosts.Not_Found
      elsif Reply = Files.REPLY_ACCESS_DENIED then Text_Hosts.Not_Granted
      elsif Reply = Files.REPLY_READ_ONLY then Text_Hosts.Read_Only_Place
      else Text_Hosts.Failed);

   ---------------------------------------------------------------------------
   --  Loading.

   procedure Start_Load (H : in out Host; Name : String; Size : out Byte_Count;
                                    Status : out Text_Hosts.File_Status) is
   begin
      --  The size is not asked first (that would be a round trip before the
      --  first byte): the document grows within its capacity as chunks come.
      Size := 0;
      if H.Load /= No_Load or else Name'Length = 0 or else Name'Length > MAXIMUM_NAME then
         Status := Text_Hosts.Failed;
         return;
      end if;
      Put (H, LOAD_PATH_AT, Name);
      H.Load_Answer := (others => <>);
      H.Times := (Open_Sent => Clock_Us, others => 0);
      if Submit (H, (Operation => FQ.Queue_Open, Options => Unsigned_32 (Files.OPEN_READ_ONLY),
                     Length => Unsigned_64 (Name'Length), Arena_Offset => LOAD_PATH_AT, others => <>), Load_Slot)
      then
         H.Load := Load_Opening;
         H.Load_Position := 0;
         Status := Text_Hosts.Pending;
      else
         Status := Text_Hosts.Failed;
      end if;
   end Start_Load;

   procedure Next_Read (H : in out Host) is
   begin
      H.Load_Asked := Half (H);
      if Submit (H, (Operation => FQ.Queue_Read_At, Handle => H.Load_Handle, Position => H.Load_Position,
                     Length => H.Load_Asked, Arena_Offset => Load_Data_At (H), others => <>), Load_Slot)
      then
         if H.Times.Read_Sent = 0 then
            H.Times.Read_Sent := Clock_Us;
         end if;
         H.Load := Load_Reading;
      else
         --  The queue is full: try again at the next poll.
         H.Load_Tag := 0;
         H.Load := Load_Reading;
         H.Load_Asked := 0;
      end if;
   end Next_Read;

   procedure Poll_Load (H : in out Host; Into : out String; Got : out Natural;
                                   Status : out Text_Hosts.File_Status) is
   begin
      Got := 0;
      Status := Text_Hosts.Pending;
      Drain (H);
      case H.Load is
         when No_Load =>
            Status := Text_Hosts.Failed;
         when Load_Opening =>
            if H.Load_Answer.Held then
               H.Load_Answer.Held := False;
               if H.Load_Answer.Status /= Files.REPLY_OK then
                  H.Reply := H.Load_Answer.Status;
                  H.Load := No_Load;
                  Status := Status_Of (H.Load_Answer.Status);
               else
                  H.Load_Handle := H.Load_Answer.Value;
                  Next_Read (H);
               end if;
            end if;
         when Load_Reading =>
            if H.Load_Tag = 0 and then not H.Load_Answer.Held and then H.Load_Asked = 0 then
               Next_Read (H);
            elsif H.Load_Answer.Held then
               H.Load_Answer.Held := False;
               if H.Load_Answer.Status /= Files.REPLY_OK then
                  H.Reply := H.Load_Answer.Status;
                  Fire (H, (Operation => FQ.Queue_Close, Handle => H.Load_Handle, others => <>));
                  H.Load := No_Load;
                  Status := Text_Hosts.Failed;
               else
                  Got := Natural (Unsigned_64'Min (Unsigned_64'Min (H.Load_Answer.Value, H.Load_Asked),
                                                   Unsigned_64 (Into'Length)));
                  Take (H, Load_Data_At (H), Into (Into'First .. Into'First + Got - 1));
                  H.Load_Position := H.Load_Position + Unsigned_64 (Got);
                  if Unsigned_64 (Got) < H.Load_Asked then
                     Fire (H, (Operation => FQ.Queue_Close, Handle => H.Load_Handle, others => <>));
                     H.Load := No_Load;
                     Status := Text_Hosts.Done;
                  else
                     Next_Read (H);
                  end if;
               end if;
            end if;
         when Load_Closing =>
            H.Load := No_Load;
            Status := Text_Hosts.Done;
      end case;
   end Poll_Load;

   ---------------------------------------------------------------------------
   --  Saving: the sibling, written, flushed, closed, renamed over.

   procedure Fail_Save (H : in out Host; Reply : Unsigned_32) is
   begin
      H.Reply := Reply;
      if H.Save_Handle /= 0 then
         Fire (H, (Operation => FQ.Queue_Close, Handle => H.Save_Handle, others => <>));
         H.Save_Handle := 0;
      end if;
      --  The partial sibling goes; the original was never touched.
      Put (H, SAVE_PATH_AT, H.Temporary.Text (1 .. H.Temporary.Length));
      Fire (H, (Operation => FQ.Queue_Unlink, Length => Unsigned_64 (H.Temporary.Length),
                Arena_Offset => SAVE_PATH_AT, others => <>));
      H.Save := Save_Failed;
   end Fail_Save;

   --  Move the save along with what its answer says.
   procedure Advance_Save (H : in out Host) is
   begin
      Drain (H);
      if not H.Save_Answer.Held then
         return;
      end if;
      H.Save_Answer.Held := False;
      if H.Save_Answer.Status /= Files.REPLY_OK then
         Fail_Save (H, H.Save_Answer.Status);
         return;
      end if;
      case H.Save is
         when Save_Opening =>
            H.Save_Handle := H.Save_Answer.Value;
            H.Save := Save_Ready;
         when Save_Writing =>
            H.Save := Save_Ready;
         when Save_Flushing =>
            if Submit (H, (Operation => FQ.Queue_Close, Handle => H.Save_Handle, others => <>), Save_Slot) then
               H.Save := Save_Closing;
            else
               Fail_Save (H, Files.REPLY_NO_SPACE);
            end if;
         when Save_Closing =>
            H.Save_Handle := 0;
            Put (H, SAVE_PATH_AT, H.Temporary.Text (1 .. H.Temporary.Length) & H.Target.Text (1 .. H.Target.Length));
            if Submit (H, (Operation => FQ.Queue_Rename, Position => Unsigned_64 (H.Temporary.Length),
                           Length => Unsigned_64 (H.Temporary.Length + H.Target.Length),
                           Arena_Offset => SAVE_PATH_AT, others => <>), Save_Slot)
            then
               H.Save := Save_Renaming;
            else
               Fail_Save (H, Files.REPLY_NO_SPACE);
            end if;
         when Save_Renaming =>
            H.Save := Save_Done;
         when others => null;
      end case;
   end Advance_Save;

   procedure Start_Save (H : in out Host; Name : String; Status : out Text_Hosts.File_Status) is
      Slash : Natural := Name'First - 1;
   begin
      if H.Save not in No_Save | Save_Done | Save_Failed or else Name'Length = 0
        or else Name'Length + SAVE_SUFFIX'Length + 1 > MAXIMUM_NAME
      then
         Status := Text_Hosts.Failed;
         return;
      end if;
      for I in Name'Range loop
         if Name (I) = '/' then
            Slash := I;
         end if;
      end loop;
      declare
         Sibling : constant String :=
           Name (Name'First .. Slash) & "." & Name (Slash + 1 .. Name'Last) & SAVE_SUFFIX;
      begin
         H.Target.Length := Name'Length;
         H.Target.Text (1 .. Name'Length) := Name;
         H.Temporary.Length := Sibling'Length;
         H.Temporary.Text (1 .. Sibling'Length) := Sibling;
         Put (H, SAVE_PATH_AT, Sibling);
         H.Save_Answer := (others => <>);
         H.Finish_Requested := False;
         H.Save_Position := 0;
         H.Save_Handle := 0;
         if Submit (H, (Operation => FQ.Queue_Open,
                        Options => Unsigned_32 (Files.OPEN_WRITE_ONLY or Files.OPEN_CREATE or Files.OPEN_TRUNCATE),
                        Length => Unsigned_64 (Sibling'Length), Arena_Offset => SAVE_PATH_AT, others => <>),
                    Save_Slot)
         then
            H.Save := Save_Opening;
            Status := Text_Hosts.Done;
         else
            Status := Text_Hosts.Failed;
         end if;
      end;
   end Start_Save;

   procedure Save_Chunk (H : in out Host; Bytes : String; Status : out Text_Hosts.File_Status) is
   begin
      Advance_Save (H);
      if H.Save = Save_Failed then
         Status := Status_Of (H.Reply);
         return;
      elsif H.Save /= Save_Ready or else Unsigned_64 (Bytes'Length) > Half (H) then
         Status := Text_Hosts.Busy;
         return;
      end if;
      Put (H, Save_Data_At (H), Bytes);
      if Submit (H, (Operation => FQ.Queue_Write_At, Handle => H.Save_Handle, Position => H.Save_Position,
                     Length => Unsigned_64 (Bytes'Length), Arena_Offset => Save_Data_At (H), others => <>),
                 Save_Slot)
      then
         H.Save_Position := H.Save_Position + Unsigned_64 (Bytes'Length);
         H.Save := Save_Writing;
         Status := Text_Hosts.Done;
      else
         Status := Text_Hosts.Busy;
      end if;
   end Save_Chunk;

   procedure Finish_Save (H : in out Host; Status : out Text_Hosts.File_Status) is
   begin
      H.Finish_Requested := True;
      Status := (if H.Save = Save_Failed then Status_Of (H.Reply) else Text_Hosts.Pending);
   end Finish_Save;

   procedure Poll_Save (H : in out Host; Status : out Text_Hosts.File_Status) is
   begin
      Advance_Save (H);
      if H.Finish_Requested and then H.Save = Save_Ready then
         --  Every write answered: make it durable, then close and rename.
         if Submit (H, (Operation => FQ.Queue_Flush, Handle => H.Save_Handle, others => <>), Save_Slot) then
            H.Save := Save_Flushing;
         end if;
      end if;
      case H.Save is
         when Save_Done =>
            H.Save := No_Save;
            H.Finish_Requested := False;
            Status := Text_Hosts.Done;
         when Save_Failed =>
            H.Save := No_Save;
            H.Finish_Requested := False;
            Status := Status_Of (H.Reply);
         when others =>
            Status := Text_Hosts.Pending;
      end case;
   end Poll_Save;

   ---------------------------------------------------------------------------
   --  The clipboard service.

   procedure Copy (H : in out Host; Text : String) is
      Result : CuBit.UI.Clipboard.Outcome;
   begin
      CuBit.UI.Clipboard.Copy_Text (H.Surface, Text, CuBit.UI.Clipboard.Ordinary, Result);
   end Copy;

   procedure Request_Paste (H : in out Host) is
      Last : Natural;
      Kind : CuBit.UI.Clipboard.Sensitivity;
      Result : CuBit.UI.Clipboard.Outcome;
   begin
      --  Inside the key's handling: the desktop's activation grant holds.
      CuBit.UI.Clipboard.Paste_Text (H.Surface, CuBit.UI.Clipboard.Ordinary, H.Paste.all, Last, Kind, Result);
      H.Paste_Ready := True;
      H.Paste_Truncated := False;
      H.Paste_Length := (if Result = CuBit.UI.Clipboard.Done then Last - H.Paste'First + 1 else 0);
   end Request_Paste;

   procedure Poll_Paste
     (H : in out Host; Into : out String; Length : out Natural; Ready, Truncated : out Boolean)
   is
   begin
      Ready := H.Paste_Ready;
      H.Paste_Ready := False;
      Length := Natural'Min (H.Paste_Length, Into'Length);
      Truncated := H.Paste_Length > Into'Length or else H.Paste_Truncated;
      Into (Into'First .. Into'First + Length - 1) := H.Paste (1 .. Length);
   end Poll_Paste;

   function Granted_Count (H : Host) return Natural is (H.Offered_Count);
   function Granted_Name (H : Host; Index : Positive) return String is
     (H.Offered (Index).Text (1 .. H.Offered (Index).Length));
   --  Names are already scoped ("@nvme:0/Documents/notes.txt").
   function Display_Name (H : Host; Name : String) return String is (Name);
end Text_Host_Native;

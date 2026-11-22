pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_IDs.Text;
with CuBit.Log_Protocol; use CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Log_Records.Texts;
with CuBit.Settings;
with Log_Fanout;
with Log_Persistence;
with Log_Budgets;
with Stream_Writers;
with Publisher_Rings;
with CuBit.Channel_Protocol;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Process_Events;
with Kernel_Device_Memory;
with Kernel_Log_Publication;
with Kernel_Log_Reader;
with Startup;
with CuBit.Refusal_Notes;
with CuBit.Log_Sources;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with System;
with Source_Identities;
with Source_Registrars;
with CuBit.Monotonic;
with CuBit.Console_Lines;
with CuBit.Handles.Startup;
with CuBit.Handles.Test_Consoles;
with CuBit.Test_Console_Authority;
with Test_Console_ABI;

procedure Main is
   function Image (Process : Process_ID) return String
     renames CuBit.Process_IDs.Text.Image;
   package Logs renames CuBit.Log_Records;
   --  A request refused for want of the badge its operation needs, logged
   --  once per process and operation (CAP-003).
   procedure Report_Refusal (From : Process_ID; What : String) is
   begin
      CuBit.Refusal_Notes.Warn_Once
        ("logstore", Image (From) & " " & What,
         "process " & Image (From) & " " & What &
         ": its endpoint lacks the badge for it (the logstore, log-observer or " &
         "log-control entry its manifest requests)");
   end Report_Refusal;
   use type Logs.Severity;
   --  The setting naming the least severe record kept, as a CCL Severity
   --  value ("Severity.Information"). Without it everything is kept.
   MINIMUM_SETTING : constant String := Startup.Settings.Logs_Minimum_Level.Setting_Key;
   --  Kept: records at Minimum and above. Others are acknowledged and dropped.
   Minimum : Logs.Severity := Logs.Trace;
   --  Until Config answers (or log-control sets it), the setting is retried.
   Minimum_Settled : Boolean := False;
   Store : Log_Fanout.Broker;
   Writers : Stream_Writers.Table;
   --  Publishers' channels: drained every pass.
   Publishers : Publisher_Rings.Table;
   --  Who each source is, from the launchers' registrar channels (LOG-003).
   Identities : Source_Identities.Table;
   Registrars : Source_Registrars.Table;
   --  A registrar channel still held notices, or some arrived while arming.
   Waiting_Notices : Boolean := False;
   --  A publisher ring still held records after a pass, or records arrived
   --  while arming: do not sleep.
   Waiting_Records : Boolean := False;
   Send_Reply : Boolean;
   --  A reader's ring was full: drain again soon rather than in a second.
   Backlog : Boolean := False;
   IDLE_WAKE_MS : constant := 1_000;
   --  During boot, wake more often so the kernel's device-memory records
   --  (Kernel_Device_Memory) reach the Desktop bootstrap screen.
   STARTUP_WAKE_MS   : constant := 200;
   STARTUP_WINDOW_MS : constant := 15_000;
   Started_Ms : Unsigned_64 := 0;
   BACKLOG_WAKE_MS : constant := 20;
   Budgets : Log_Budgets.Limiter;
   From : Process_ID;
   Request, Response : Message;
   Received, Known, Acquired : Boolean;
   Now_Ms, Handle, Ignore : Unsigned_64;
   Result : Status;
   Op : Operation := Get_Minimum;
   Ended_Event : CuBit.Control_Events.Event;
   Have_Event : Boolean;
   --  procmgr's Stop (shutdown): this pass drains and persists what the
   --  publishers wrote, then logstore ends.
   Stop_Requested : Boolean := False;
   use type CuBit.Control_Events.Event_Kind, CuBit.Control_Events.Control_Kind;
   Decoded : Logs.Decoded;

   --  Test builds only (Test_Console_ABI): the serial console logstore
   --  mirrors console text onto, as "<source>: <text>", so the headless
   --  tests keep their markers. Granted by name only when logstore's test
   --  manifest declares it and a test plan approves it; the shipped
   --  manifest never does, and then there is no mirror.
   Console : constant CuBit.Handles.Test_Console :=
     CuBit.Handles.Startup.Test_Console_Named (CuBit.Test_Console_Authority.Binding);
   Mirroring : constant Boolean := CuBit.Handles.Valid (Console);

   procedure Mirror_Text (Name, Text : String) is
      Line : constant String := Name & ": " & Text & ASCII.LF;
      Answer : CuBit.Handles.Test_Consoles.Outcome;
      pragma Unreferenced (Answer);
   begin
      if Line'Length <= Test_Console_ABI.Maximum_Bytes then
         Answer := CuBit.Handles.Test_Consoles.Write (Console, Line);
      else
         Answer := CuBit.Handles.Test_Consoles.Write
           (Console, Line (Line'First .. Line'First + Test_Console_ABI.Maximum_Bytes - 2) & ASCII.LF);
      end if;
   end Mirror_Text;

   --  A publisher's console text (Publisher_Rings), under its name.
   procedure Mirror_Record (Owner : Process_ID; Value : Logs.Log_Record) is
      Name : constant String := Source_Identities.Name_Of (Identities, CuBit.Process_IDs.To_Word (Owner));
   begin
      Mirror_Text ((if Name'Length > 0 then Name else Image (Owner)), Logs.Texts.Text (Value));
   end Mirror_Record;

   --  logstore's own debugPrint text (ABI-002): never through a ring (it
   --  would be its own publisher). Each line is mirrored at once, and kept
   --  as a Debug console record, published into the store at the start of
   --  the next pass (never in the middle of a store operation); at most
   --  Own_Lines_Pending wait, the rest are counted and reported.
   Own_Lines_Pending : constant := 16;
   Own_Framer : CuBit.Console_Lines.Framer;
   Own_Lines : array (1 .. Own_Lines_Pending) of Logs.Log_Record;
   Own_Count : Natural range 0 .. Own_Lines_Pending := 0;
   Own_Lost : Unsigned_64 := 0;

   procedure Own_Text (Text : String) is
      Complete : Boolean;
      Made : Logs.Decoded;
   begin
      for C of Text loop
         CuBit.Console_Lines.Feed (Own_Framer, C, Complete);
         if Complete then
            declare
               Framed : CuBit.Console_Lines.Line_Buffer;
               Used : CuBit.Console_Lines.Line_Length;
            begin
               CuBit.Console_Lines.Line (Own_Framer, Framed, Used);
               CuBit.Console_Lines.Plain (Framed (1 .. Used));
               CuBit.Console_Lines.Clear (Own_Framer);
               declare
                  Line : String renames Framed (1 .. Used);
               begin
               if Line'Length > 0 then
                  if Mirroring then
                     Mirror_Text ("logstore", Line);
                  end if;
                  Made := Logs.Make (Line, Logs.Debug);
                  if Made.Success then
                     Made := Logs.With_Field
                       (Made.Value, CuBit.Console_Lines.Console_Field, Logs.Truth, 1);
                  end if;
                  if Made.Success and then Own_Count < Own_Lines_Pending then
                     Own_Count := Own_Count + 1;
                     Own_Lines (Own_Count) := Made.Value;
                  elsif Own_Lost < Unsigned_64'Last then
                     Own_Lost := Own_Lost + 1;
                  end if;
               end if;
               end;
            end;
         end if;
      end loop;
   end Own_Text;


   --  List_Sources (LOG-003): who the sources logstore knows are, newest
   --  first after Skip, written as CuBit.Log_Sources entries into the page
   --  Reference names, which Reader lends. Written and Total for the reply.
   procedure List_Sources
     (Reader : Process_ID; Reference_Word, Skip : Unsigned_64;
      Written, Total : out Unsigned_64; Result : out Status)
   is
      package LS renames CuBit.Log_Sources;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped : System.Address;
      Acquired, Returned : Boolean;
      Known : constant Natural := Source_Identities.Count (Identities);
      Bytes : LS.Entry_Buffer;
   begin
      Written := 0;
      Total := Unsigned_64 (Known);
      Result := Invalid_Request;
      if not CuBit.Grant_References.Valid_Wire (Reference_Word) then
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (Reference_Word);
      CuBit.Memory_Grants.Acquire
        (Reference, Reader, 0, LS.Page_Bytes, CuBit.Memory_Grants.Write_Access, Mapped, Acquired);
      if not Acquired then
         return;
      end if;
      declare
         Page : array (0 .. LS.Page_Bytes - 1) of Unsigned_8 with Import, Address => Mapped;
      begin
         Page := [others => 0];
         for Position in LS.Page_Position loop
            exit when Skip >= Unsigned_64 (Known) - Written;
            LS.Encode
              (Source_Identities.Newest (Identities, Natural (Skip + Written) + 1), Bytes);
            for I in Bytes'Range loop
               Page (LS.Entry_Offset (Position) + I) := Bytes (I);
            end loop;
            Written := Written + 1;
         end loop;
      end;
      CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
      Result := OK;
   end List_Sources;

   --  logstore's own record about what it keeps, published straight into
   --  the store and never itself subject to the minimum.
   procedure Note (Text : String; Level : Logs.Severity := Logs.Information) is
      Made : constant Logs.Decoded := Logs.Make (Text, Level);
   begin
      if Made.Success then
         Log_Fanout.Publish
           (Store, (Source => Own_Process_Word, Node => This_Node, Publication_Tag => 0,
                    Monotonic_Ms => Now_Ms, Data => Made.Value));
      end if;
   end Note;

   --  The kernel's device-memory refusals and physical map (CAP-001): kept
   --  as logstore's records, and (test builds) mirrored once, as console
   --  text (they used to be echoed with debugPrint; now that debugPrint
   --  stores too, that would keep each twice).
   procedure Note_Console (Text : String; Level : Logs.Severity) is
   begin
      if Mirroring then
         Mirror_Text ("logstore", Text);
      end if;
      Note (Text, Level);
   end Note_Console;
   package Kernel_Records is new Kernel_Device_Memory (Note_Console);

   --  The kernel log ring (LOG-004): the kernel's lines, and the last words
   --  of processes that ended abnormally, at the kernel's own times. They
   --  are kept like any publisher's (the minimum applies).
   procedure Publish_Kernel_Record
     (Source, Time_Ms : Unsigned_64; Level : Logs.Severity; Text : String)
   is
      Made : constant Logs.Decoded := Logs.Make (Text, Level);
   begin
      if Made.Success and then Level >= Minimum then
         Log_Fanout.Publish
           (Store, (Source => Source, Node => This_Node, Publication_Tag => 0,
                    Monotonic_Ms => Time_Ms, Data => Made.Value));
      end if;
   end Publish_Kernel_Record;

   --  A process the kernel named that no launcher told logstore about (or
   --  one already forgotten): known by the kernel's name for it, as an
   --  exited source, so (logs.recent "<name>") still finds its last words.
   procedure Learn_Kernel_Source (Source : Unsigned_64; Name : String) is
      package LS renames CuBit.Log_Sources;
   begin
      if Source /= LS.No_Source and then Name'Length > 0
        and then Name'Last < Positive'Last
        and then not Source_Identities.Knows (Identities, Source)
      then
         declare
            Who : constant LS.Source_Identity := LS.Make (Source, "", Name);
         begin
            if LS.Named (Who) then
               Source_Identities.Started (Identities, Who, Now_Ms);
               Source_Identities.Exited (Identities, Source, Now_Ms);
            end if;
         end;
      end if;
   end Learn_Kernel_Source;

   procedure Publish_Kernel_Fields
     (Source, Time_Ms : Unsigned_64; Value : Logs.Log_Record) is
   begin
      if Logs.Level (Value) >= Minimum then
         Log_Fanout.Publish
           (Store, (Source => Source, Node => This_Node, Publication_Tag => 0,
                    Monotonic_Ms => Time_Ms, Data => Value));
      end if;
   end Publish_Kernel_Fields;

   function Kernel_Source_Name (Source : Unsigned_64; Fallback : String) return String is
      Known : constant String := Source_Identities.Identity_Of (Identities, Source);
   begin
      return (if Known'Length > 0 then Known else Fallback);
   end Kernel_Source_Name;

   package Kernel_Log is new Kernel_Log_Reader
     (Publish_Kernel_Record, Publish_Kernel_Fields, Learn_Kernel_Source, Kernel_Source_Name);
   Kernel_More : Boolean := False;

   --  storage.system-volume: where the persistent log goes ("" or absent:
   --  nowhere, as on a live boot). Read until Config answers.
   Volume_Settled : Boolean := False;
   procedure Read_Volume_Setting is
      use all type CuBit.Settings.Outcome;
      Stored : constant Startup.Settings.Storage_System_Volume.Reading :=
        Startup.Settings.Storage_System_Volume.Read;
   begin
      if Stored.Kind = Present then
         Volume_Settled := True;
         declare
            Raw : constant String := CuBit.Settings.Image (Stored.Item);
            Value : constant String :=
              (if Raw'Length >= 2 and then Raw (Raw'First) = '"' and then Raw (Raw'Last) = '"'
               then Raw (Raw'First + 1 .. Raw'Last - 1) else Raw);
         begin
            Log_Persistence.Configure (Value);
            Note ("logstore: persisting to @" & Value & "/logstore.log once it attaches");
         end;
      elsif Stored.Kind in Absent | Denied then
         Volume_Settled := True;
      end if;
   end Read_Volume_Setting;

   function Decimal (Value : Unsigned_64) return String is
      Raw : constant String := Unsigned_64'Image (Value);
   begin
      return Raw (Raw'First + 1 .. Raw'Last);
   end Decimal;

   --  logs.retention (Logstore_Retention_Settings): the retained history's
   --  ceilings. Until Config answers, Log_Fanout.Until_Configured; read
   --  until it does. Each outcome is logstore's own record, so what it
   --  retains is never a guess.
   Retention_Settled : Boolean := False;
   procedure Read_Retention_Setting is
      use all type CuBit.Settings.Outcome;
      package Setting renames Startup.Settings.Logs_Retention;
      Stored : constant Setting.Reading := Setting.Read;
      function Described (Limits : Log_Fanout.Ceilings) return String is
        (Decimal (Unsigned_64 (Limits.Per_Source)) & " records per source (its first" & Natural'Image (Limits.Boot) &
         " always kept)," & Natural'Image (Limits.Total) & " in all");
   begin
      case Stored.Kind is
         when Present =>
            Retention_Settled := True;
            if Log_Fanout.Raises (Stored.Item, Log_Fanout.Limits (Store)) then
               Log_Fanout.Configure (Store, Stored.Item);
               Note ("logstore: retaining up to " & Described (Stored.Item) & " (" & Setting.Setting_Key & ")");
            else
               Note ("logstore: " & Setting.Setting_Key & " would lower the ceilings already in force; retaining up to " &
                     Described (Log_Fanout.Limits (Store)), Logs.Warning);
            end if;
         when Invalid =>
            Retention_Settled := True;
            Note ("logstore: " & Setting.Setting_Key & " is not a consistent Log_Retention (boot below per_source, " &
                  "per_source within total); retaining up to " & Described (Log_Fanout.Limits (Store)), Logs.Warning);
         when Absent | Denied =>
            Retention_Settled := True;
            Note ("logstore: " & Setting.Setting_Key & " is " &
                  (if Stored.Kind = Absent then "not in the plan" else "not readable") &
                  "; retaining up to the startup ceilings, " & Described (Log_Fanout.Limits (Store)), Logs.Warning);
         when Unavailable =>
            null;
      end case;
   end Read_Retention_Setting;

   --  A history that gave way to a new source (Log_Fanout.Source_Slots):
   --  logstore's own record says whose and how much.
   procedure Report_Reclaimed is
      Gone : CuBit.Log_Protocol.Source_Tally;
      Found : Boolean;
      Missed : Unsigned_64;
   begin
      Log_Fanout.Take_Reclaimed (Store, Gone, Found, Missed);
      if Found then
         Note ("logstore: gave up the history of " &
               Kernel_Source_Name (Gone.Source, "source " & Decimal (Gone.Source)) &
               " for a new source:" & Unsigned_64'Image (Gone.Retained) & " records retained," &
               Unsigned_64'Image (Gone.Published) & " published" &
               (if Missed > 0 then ";" & Unsigned_64'Image (Missed) & " more histories given up before this"
                else ""), Logs.Warning);
      end if;
   end Report_Reclaimed;

   procedure Read_Minimum_Setting is
      use all type CuBit.Settings.Outcome;
      Stored : constant Startup.Settings.Logs_Minimum_Level.Reading := Startup.Settings.Logs_Minimum_Level.Read;
      Value : constant String := (if Stored.Kind = Present then CuBit.Settings.Image (Stored.Item) else "");
   begin
      if Stored.Kind = Present then
         Minimum_Settled := True;
         if Logs.Texts.Is_Severity_Literal (Value) then
            Minimum := Logs.Texts.Severity_Of (Value);
            Note ("logstore: keeping " & Logs.Texts.Severity_Literal (Minimum) & " and above (" & MINIMUM_SETTING & ")");
         else
            Note ("logstore: " & MINIMUM_SETTING & " is not a Severity (for example Severity.Information); " &
                  "keeping everything", Logs.Warning);
         end if;
      elsif Stored.Kind in Absent | Denied then
         --  Nothing to wait for: no setting, or no grant to read it.
         Minimum_Settled := True;
      end if;
   end Read_Minimum_Setting;
   --  logstore's own lines into its store (Own_Text), those at the minimum
   --  and above, and what could not wait.
   procedure Publish_Own_Lines is
   begin
      for Index in 1 .. Own_Count loop
         if Logs.Level (Own_Lines (Index)) >= Minimum then
            Log_Fanout.Publish
              (Store, (Source => Own_Process_Word, Node => This_Node, Publication_Tag => 0,
                       Monotonic_Ms => Now_Ms, Data => Own_Lines (Index)));
         end if;
      end loop;
      Own_Count := 0;
      if Own_Lost > 0 then
         Note ("logstore: " & Unsigned_64'Image (Own_Lost) & " of its own debug lines lost (more than" &
               Natural'Image (Own_Lines_Pending) & " in one pass)", Logs.Warning);
         Own_Lost := 0;
      end if;
   end Publish_Own_Lines;
begin
   CuBit.Messages.Set_Console_Sink (Own_Text'Unrestricted_Access);
   if Mirroring then
      Publisher_Rings.Set_Mirror (Mirror_Record'Unrestricted_Access);
   end if;
   Now_Ms := CuBit.Monotonic.Milliseconds;
   Started_Ms := Now_Ms;
   Decoded := Logs.Make ("logstore: typed diagnostics ready");
   if Decoded.Success then
      Log_Fanout.Publish
        (Store, (Source => Own_Process_Word, Node => This_Node, Publication_Tag => 0,
                 Monotonic_Ms => Now_Ms, Data => Decoded.Value));
   end if;
   Ignore := registerDriver (DRIVER_LOGSTORE, Startup.Registration);
   if Ignore = Unsigned_64'Last then
      debugPrint ("logstore: registration failed" & ASCII.LF);
      return;
   end if;
   debugPrint ("logstore: authorized typed diagnostics ready" & ASCII.LF);
   --  The kernel's records go under this source: (logs.recent "kernel").
   Source_Identities.Started
     (Identities,
      CuBit.Log_Sources.Make (Kernel_Log_Publication.Kernel_Source, "com.cubit.kernel", "kernel"),
      Now_Ms);
   Kernel_Log.Set_Started (Started_Ms);
   Read_Minimum_Setting;
   Read_Retention_Setting;
   loop
      --  Deadline is for idle subscription reclamation, not input polling.
      --  Ask publishers for a Kick while asleep; records that arrived
      --  while arming mean there is no sleeping this time.
      if not Waiting_Records then
         Publisher_Rings.Arm (Publishers, Waiting_Records);
         Source_Registrars.Arm (Registrars, Waiting_Notices);
         Waiting_Records := Waiting_Records or else Waiting_Notices;
      end if;
      receiveUntil
        ((if Waiting_Records then Now_Ms
          else Now_Ms + Unsigned_64'Min
                 ((if Backlog then BACKLOG_WAKE_MS
                   elsif Now_Ms >= Started_Ms and then Now_Ms - Started_Ms < STARTUP_WINDOW_MS
                   then STARTUP_WAKE_MS
                   else IDLE_WAKE_MS),
                  Unsigned_64'Last - Now_Ms)),
         From, Request, Received);
      Publisher_Rings.Disarm (Publishers);
      Source_Registrars.Disarm (Registrars);
      --  The receive takes kernel notices too: a Stop is one.
      if Received and then Is_Kernel_Stop (Request.tag.label, Request.tag.length, From, Request.words (0)) then
         Stop_Requested := True;
      end if;
      Now_Ms := CuBit.Monotonic.Milliseconds;
      if not Minimum_Settled then
         Read_Minimum_Setting;
      end if;
      if not Volume_Settled then
         Read_Volume_Setting;
      end if;
      if not Retention_Settled then
         Read_Retention_Setting;
      end if;
      Log_Fanout.Advance_Time (Store, Now_Ms);
      Log_Budgets.Advance_Time (Budgets, Now_Ms);
      Publish_Own_Lines;
      Kernel_Records.Poll;
      Kernel_Log.Poll (Now_Ms, Kernel_More);
      --  Publishers that closed or died: their last records first.
      loop
         CuBit.Process_Events.Next (Ended_Event, Have_Event);
         exit when not Have_Event;
         if Ended_Event.Kind = CuBit.Control_Events.Control
           and then Ended_Event.Control = CuBit.Control_Events.Stop
         then
            Stop_Requested := True;
         end if;
         Publisher_Rings.Ended (Publishers, Store, Budgets, Ended_Event, Minimum, Now_Ms);
         Source_Registrars.Ended (Registrars, Identities, Ended_Event, Now_Ms);
         Stream_Writers.Ended (Writers, Ended_Event);
      end loop;
      if Received and then Request.tag.label = CuBit.Channel_Protocol.OP_OPEN_PRODUCING then
         --  A launcher opening its registrar channel (LOG-003), or a
         --  publisher opening its channel.
         if From /= No_Process and then CuBit.Log_Sources.Is_Registrar (Request.authorityTag) then
            Source_Registrars.Open
              (Registrars, Identities, From, Request.authorityTag, Request, Now_Ms, Response);
         elsif From /= No_Process and then May_Publish (Request.authorityTag) then
            Publisher_Rings.Open (Publishers, Store, Budgets, From, Request.authorityTag, Request,
                                  Minimum, Now_Ms, Response);
         else
            Report_Refusal (From, "opening a publisher channel");
            Response := CuBit.Channels.Refusal_Reply (CuBit.Channel_Protocol.Unsupported);
         end if;
         Ignore := reply (From, Response);
      elsif Received and then Request.tag.label = CuBit.Channel_Protocol.OP_OPEN_CONSUMING then
         --  A reader opening its stream (Subscribe binds it).
         if From /= No_Process and then May_Invoke (Request.authorityTag, Subscribe) then
            Stream_Writers.Open (Writers, From, Request.authorityTag, Request, Response);
         else
            Report_Refusal (From, "opening a reader stream");
            Response := CuBit.Channels.Refusal_Reply (CuBit.Channel_Protocol.Unsupported);
         end if;
         Ignore := reply (From, Response);
      elsif Received and then Request.tag.label = CuBit.Channel_Protocol.OP_CLOSE then
         if CuBit.Channels.Number_Of (Request) > Stream_Writers.First_Number then
            Stream_Writers.Close (Writers, From, Request);
         elsif Source_Registrars.Owns (CuBit.Channels.Number_Of (Request)) then
            Source_Registrars.Close (Registrars, Identities, From, Request, Now_Ms);
         else
            Publisher_Rings.Close (Publishers, Store, Budgets, From, Request, Minimum, Now_Ms);
         end if;
      elsif Received and then Request.tag.label = CuBit.Log_Sources.Detach_Storage then
         --  Shutdown, before storage stops (procmgr only, under its
         --  registrar tag): what the publishers wrote, then the file.
         Response := NULL_MESSAGE;
         Response.tag := (label => Status'Enum_Rep (Denied), length => 1,
                          protocol => 0, ipc => <>, reserved => 0);
         if From /= No_Process and then Request.tag.length = 0
           and then Request.authorityTag =
                    CuBit.Log_Sources.Registrar_Tag (CuBit.Log_Sources.Process_Manager_Issuance)
         then
            declare
               Was_Open : Boolean;
            begin
               Publish_Own_Lines;
               Publisher_Rings.Drain (Publishers, Store, Budgets, Minimum, Now_Ms, Waiting_Records);
               Log_Persistence.Detach (Store, Now_Ms, Was_Open);
               Response.tag.label := Status'Enum_Rep (OK);
               Response.words (0) := (if Was_Open then 1 else 0);
               Note_Console ("logstore: storage stopping: " &
                             (if Was_Open then "persisted and closed the log file" else "no log file open") &
                             "; keeping the final lines in memory", Logs.Information);
            end;
         else
            Report_Refusal (From, "detaching storage");
         end if;
         Ignore := reply (From, Response);
      elsif Received and then Request.tag.label = CuBit.Channel_Protocol.OP_KICK then
         --  Records are waiting: drained below, every pass.
         null;
      elsif Received then
         Response := NULL_MESSAGE;
         Response.tag := (label => Status'Enum_Rep (Denied), length => 4,
                          protocol => 0, ipc => <>, reserved => 0);
         Known := False;
         for Candidate in Operation loop
            if Request.tag.label = Operation'Enum_Rep (Candidate) then
               Op := Candidate;
               Known := True;
            end if;
         end loop;
         Result := Denied;
         Send_Reply := True;
         if Known and then not May_Invoke (Request.authorityTag, Op) then
            Report_Refusal (From, Operation'Image (Op));
         end if;
         if From /= No_Process and then Known and then
           May_Invoke (Request.authorityTag, Op)
         then
            Result := Invalid_Request;
            if Request.tag.length = 4 and then Request.tag.protocol = 0
              and then Request.tag.reserved = 0
            then
               case Op is
                  when Subscribe =>
                     --  Words: minimum, source, the reader's stream channel,
                     --  whether the replay ends with tallies.
                     if Request.words (0) <= Logs.Severity'Pos (Logs.Severity'Last) and then
                       Request.words (3) in Replay_Report'Enum_Rep (Records_Only) |
                                            Replay_Report'Enum_Rep (With_Tallies)
                     then
                        Log_Fanout.Subscribe
                          (Store, From, Request.authorityTag, Handle, Result,
                           Logs.Severity'Val (Request.words (0)), Request.words (1),
                           (if Request.words (3) = Replay_Report'Enum_Rep (With_Tallies)
                            then With_Tallies else Records_Only));
                        if Result = OK then
                           --  The reader's stream channel, filled by Drain.
                           Stream_Writers.Bind
                             (Writers, Request.words (2), Handle, From, Request.authorityTag,
                              Acquired);
                           if Acquired then
                              --  The handle, and the source filter it applies.
                              Response.words (0) := Handle;
                              Response.words (1) := Request.words (1);
                              --  Replay is in the ring when the reply arrives.
                              Stream_Writers.Drain (Writers, Store, Backlog);
                           else
                              Log_Fanout.Close (Store, From, Request.authorityTag, Handle, Result);
                              Result := Invalid_Request;
                           end if;
                        end if;
                     end if;
                  when Set_Minimum =>
                     if Request.words (0) <= Logs.Severity'Pos (Logs.Severity'Last) and then
                       Request.words (1 .. 3) = [0, 0, 0]
                     then
                        Response.words (0) := Logs.Severity'Pos (Minimum);
                        Minimum := Logs.Severity'Val (Request.words (0));
                        Minimum_Settled := True;
                        Result := OK;
                        --  Who changed it, kept whatever the new minimum.
                        Note ("logstore: keeping " & Logs.Texts.Severity_Literal (Minimum) &
                              " and above (set by pid" & Image (From) & ")",
                              Logs.Warning);
                     end if;
                  when Get_Minimum =>
                     if Request.words = [0, 0, 0, 0] then
                        Response.words (0) := Logs.Severity'Pos (Minimum);
                        Result := OK;
                     end if;
                  when List_Sources =>
                     --  Words: the lent page, the sources to skip.
                     if Request.words (2 .. 3) = [0, 0] then
                        List_Sources (From, Request.words (0), Request.words (1),
                                      Response.words (0), Response.words (1), Result);
                     end if;
                  when Close =>
                     if Request.words (1 .. 3) = [0, 0, 0] then
                        Log_Fanout.Close (Store, From, Request.authorityTag,
                          Request.words (0), Result);
                        if Result = OK then
                           Stream_Writers.Unbind (Writers, Request.words (0));
                        end if;
                     end if;
               end case;
            end if;
         end if;
         if Send_Reply then
            Response.tag.label := Status'Enum_Rep (Result);
            Ignore := reply (From, Response);
         end if;
      end if;
      --  Who started and exited: before readers ask, and kept while their
      --  records are; before the records, so a process's first records
      --  (mirrored at once in test builds) already carry its name.
      Source_Registrars.Drain (Registrars, Identities, Now_Ms, Waiting_Notices);
      --  Published records, new events, freed ring space, ended
      --  subscriptions: every pass.
      Publisher_Rings.Drain (Publishers, Store, Budgets, Minimum, Now_Ms, Waiting_Records);
      --  The kernel log still held records after this pass's share.
      Waiting_Records := Waiting_Records or else Kernel_More or else Waiting_Notices;
      Report_Reclaimed;
      Source_Identities.Prune (Identities, Store, Now_Ms);
      Stream_Writers.Drain (Writers, Store, Backlog);
      Log_Persistence.Pump (Store, Now_Ms);
      Publisher_Rings.Release (Publishers);
      exit when Stop_Requested;
   end loop;
   Publish_Own_Lines;
   Note ("logstore: Stop: drained and persisted; exiting");
end Main;

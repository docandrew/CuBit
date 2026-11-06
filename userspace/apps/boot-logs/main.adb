with CuBit.UI_Metrics;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with Startup;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Log_Sources;
with CuBit.Desktop_Protocol;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Clipboard;
with Clipboard_Chords;
with Clipboard_Text;
with CuBit.UI.State;
with CuBit.UI.Controls;
with CuBit.UI.Input;
procedure Main is
   package P renames CuBit.Log_Protocol;
   use type P.Status;
   use type CuBit.Log_Records.Severity;
   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   Reader : CuBit.Logging.Reader := CuBit.Logging.Bound_To (Startup.Log_Observer);
   Status : P.Status := P.Unavailable;
   Reported_Status : P.Status := P.Unavailable;
   Records : array (1 .. 512) of CuBit.Log_Records.Log_Record;
   --  Each record's source (the process logstore stamped it with).
   Record_Sources : array (Records'Range) of Unsigned_64 := [others => 0];
   Count, Page : Natural := 0;
   Lost_Records : Unsigned_64 := 0;
   Viewer_Dropped : Unsigned_64 := 0;
   Rows : Positive := 25;
   --  Records that explain a boot, pinned above the rotating pages so one
   --  photograph of any page shows them: every Warning or worse, and the
   --  key startup lines (the renderer's result and why it stopped).
   --  Left (and right) text margin of every line.
   MARGIN : constant := 12;
   Summary_Capacity : constant := 6;
   Summary : array (1 .. Summary_Capacity) of CuBit.Log_Records.Log_Record;
   Summary_Count : Natural range 0 .. Summary_Capacity := 0;
   Key_Prefixes : constant array (1 .. 4) of access constant String :=
     [new String'("DESKTOP-VULKAN:"), new String'("intel-gpu: backing denied"),
      new String'("desktop: software rendering"), new String'("procmgr: render")];
   Line_Height : constant := CuBit.UI_Metrics.Line_Height + CuBit.UI_Metrics.Space_S;

   --  A source filter (bring-up logging convention: one photograph shows
   --  one driver's stages). A record's source is the text before its first
   --  ": " (devmgr, intel-gpu, procmgr). Tab cycles through every source
   --  seen; B shows the hardware bring-up sources together.
   MAX_SOURCES : constant := 32;
   SOURCE_TEXT : constant := 24;
   subtype Source_Count is Natural range 0 .. MAX_SOURCES;
   type Source_Name is record
      Length : Natural range 0 .. SOURCE_TEXT := 0;
      Text : String (1 .. SOURCE_TEXT) := [others => ' '];
   end record;
   Sources : array (1 .. MAX_SOURCES) of Source_Name;
   Source_Total : Source_Count := 0;
   type Filter_Kind is (All_Sources, One_Source, Bring_Up);
   Filter : Filter_Kind := All_Sources;
   Filter_Source : Source_Count := 0;
   Bring_Up_Sources : constant array (1 .. 4) of access constant String :=
     [new String'("devmgr"), new String'("intel-gpu"), new String'("procmgr"),
      new String'("xhci")];
   --  Set-1 scancodes (the desktop protocol's key payload).
   KEY_TAB : constant := 16#0F#;
   KEY_B   : constant := 16#30#;

   --  Who each source is (LOG-003): what logstore's launchers recorded,
   --  so a source that has exited keeps its name. logstore lists the live
   --  sources and the exited ones it still keeps records of; this viewer
   --  keeps as many as it keeps records.
   package LS renames CuBit.Log_Sources;
   use type LS.Entry_Kind;
   type Known_Source is record
      Who : LS.Source_Identity;
      Exited : Boolean := False;
   end record;
   Known : array (Records'Range) of Known_Source;
   Known_Count : Natural range 0 .. Records'Length := 0;
   --  How often the sources are asked for again, for exits.
   NAMES_MS : constant := 2_000;
   Names_Due : Unsigned_64 := 0;

   function Known_Index (Source : Unsigned_64) return Natural is
   begin
      for I in 1 .. Known_Count loop
         if Known (I).Who.Source = Source then
            return I;
         end if;
      end loop;
      return 0;
   end Known_Index;

   --  Its label (acpi for acpi.svc), or "" while logstore has not said.
   function Label_Of (Source : Unsigned_64) return String is
      I : constant Natural := Known_Index (Source);
   begin
      return (if I = 0 then "" else LS.Label (Known (I).Who));
   end Label_Of;

   function Exited (Source : Unsigned_64) return Boolean is
      I : constant Natural := Known_Index (Source);
   begin
      return I /= 0 and then Known (I).Exited;
   end Exited;

   --  Ask logstore who the sources are (every page); say once on serial
   --  when one has exited.
   procedure Refresh_Sources is
      Page_Of_Sources : CuBit.Logging.Source_Page;
      Written : LS.Page_Count;
      Total, Skip : Natural := 0;
      Result : P.Status;
      Index : Natural;
   begin
      loop
         CuBit.Logging.List_Sources (Reader, Skip, Page_Of_Sources, Written, Total, Result);
         exit when Result /= P.OK or else Written = 0;
         for Position in 1 .. Written loop
            declare
               Listed : LS.Source_Entry renames Page_Of_Sources (Position);
            begin
               Index := Known_Index (Listed.Who.Source);
               if Index = 0 and then Known_Count < Known'Length then
                  Known_Count := Known_Count + 1;
                  Index := Known_Count;
                  Known (Index) := (Who => Listed.Who, Exited => False);
               end if;
               if Index /= 0 then
                  Known (Index).Who := Listed.Who;
                  if Listed.Kind = LS.Exited and then not Known (Index).Exited then
                     Known (Index).Exited := True;
                     debugPrint ("boot-logs: " & LS.Label (Listed.Who) & " exited (" &
                                 LS.Identity_Of (Listed.Who) & ", " & LS.Name_Of (Listed.Who) & ")" &
                                 ASCII.LF);
                  end if;
               end if;
            end;
         end loop;
         Skip := Skip + Written;
         exit when Skip >= Total;
      end loop;
   end Refresh_Sources;

   function Source_Of (Item : CuBit.Log_Records.Log_Record) return String is
      Text : constant String := CuBit.Log_Records.Text (Item);
   begin
      for I in Text'First .. Text'Last - 1 loop
         exit when I - Text'First >= SOURCE_TEXT or else Text (I) = ' ';
         if Text (I) = ':' and then Text (I + 1) = ' ' then
            return Text (Text'First .. I - 1);
         end if;
      end loop;
      return "";
   end Source_Of;

   --  A record's source name: its text's own prefix, else its source's label.
   function Name_Of_Source (Index : Positive) return String is
      Prefix : constant String := Source_Of (Records (Index));
      Label : constant String := Label_Of (Record_Sources (Index));
   begin
      return (if Prefix'Length > 0 then Prefix
              elsif Label'Length <= SOURCE_TEXT then Label
              else Label (Label'First .. Label'First + SOURCE_TEXT - 1));
   end Name_Of_Source;

   --  A record as shown: its source's label first when its text does not
   --  name it, and marked when the source has exited.
   function Shown_Text (Index : Positive) return String is
      Text : constant String := CuBit.Log_Records.Text (Records (Index));
      Label : constant String := Label_Of (Record_Sources (Index));
      Named : constant String :=
        (if Source_Of (Records (Index))'Length = 0 and then Label'Length > 0
         then Label & ": " & Text else Text);
   begin
      return (if Exited (Record_Sources (Index)) then "[exited] " & Named else Named);
   end Shown_Text;

   procedure Remember_Source (Index : Positive) is
      Name : constant String := Name_Of_Source (Index);
   begin
      if Name'Length = 0 or else Source_Total = MAX_SOURCES then
         return;
      end if;
      for I in 1 .. Source_Total loop
         if Sources (I).Text (1 .. Sources (I).Length) = Name then
            return;
         end if;
      end loop;
      Source_Total := Source_Total + 1;
      Sources (Source_Total).Length := Name'Length;
      Sources (Source_Total).Text (1 .. Name'Length) := Name;
   end Remember_Source;

   function Shown (Index : Positive) return Boolean is
      Name : constant String := Name_Of_Source (Index);
   begin
      case Filter is
         when All_Sources => return True;
         when One_Source =>
            return Filter_Source in 1 .. Source_Total
              and then Name = Sources (Filter_Source).Text (1 .. Sources (Filter_Source).Length);
         when Bring_Up =>
            return (for some S of Bring_Up_Sources => Name = S.all);
      end case;
   end Shown;

   function Filter_Text return String is
     (case Filter is
        when All_Sources => "all sources",
        when One_Source => Sources (Filter_Source).Text (1 .. Sources (Filter_Source).Length),
        when Bring_Up => "bring-up (devmgr, intel-gpu, procmgr, xhci)");
   function Pinned (Item : CuBit.Log_Records.Log_Record) return Boolean is
      Text : constant String := CuBit.Log_Records.Text (Item);
   begin
      if CuBit.Log_Records.Level (Item) >= CuBit.Log_Records.Warning then
         return True;
      end if;
      for Prefix of Key_Prefixes loop
         if Text'Length >= Prefix'Length and then
           Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix.all
         then
            return True;
         end if;
      end loop;
      return False;
   end Pinned;
   --  Keep the latest pinned records; the oldest gives way.
   procedure Pin (Item : CuBit.Log_Records.Log_Record) is
   begin
      if Summary_Count < Summary_Capacity then
         Summary_Count := Summary_Count + 1;
      else
         for J in 1 .. Summary_Capacity - 1 loop
            Summary (J) := Summary (J + 1);
         end loop;
      end if;
      Summary (Summary_Count) := Item;
   end Pin;
   Due, Turn_Page : Unsigned_64 := 0;
   Opened : Boolean;
   --  The records the filter shows, in order.
   Visible : array (1 .. 512) of Positive;
   Visible_Count : Natural := 0;
   procedure Select_Visible is
   begin
      Visible_Count := 0;
      for I in 1 .. Count loop
         if Shown (I) then
            Visible_Count := Visible_Count + 1;
            Visible (Visible_Count) := I;
         end if;
      end loop;
      Page := Natural'Min (Page, (if Visible_Count = 0 then 0 else (Visible_Count - 1) / Rows));
   end Select_Visible;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      C : constant Canvas := CuBit.UI.App.Canvas (Win, Damage);
      Colors : constant Theme := Current_Theme;
      First : Natural;
      --  Rows below the pinned summary (and its divider line).
      Top : constant Natural := 40 + (if Summary_Count = 0 then 0
                                      else (Summary_Count + 1) * Line_Height);
      --  Every line elided to keep MARGIN clear of the right edge too.
      Lines : constant Rect := (MARGIN, 0, Natural'Max (CuBit.UI.App.Width (Win), 2 * MARGIN) - 2 * MARGIN,
                                CuBit.UI.App.Height (Win));
      procedure Line (Y : Natural; Text : String; Ink : Color) is
      begin
         Draw_UI_Text_Fitted (C, Lines, MARGIN, Y, Text, Ink, Colors.panel);
      end Line;
   begin
      Fill_Rect (C, CuBit.UI.App.Full_Rect (Win), Colors.panel);
      Select_Visible;
      First := Page * Rows + 1;
      Line (10, "Boot diagnostics: " & Filter_Text & " (Tab: next source, B: bring-up); pages rotate every 8 s",
            Colors.text);
      Line (CuBit.UI.App.Height (Win) - 30, "Page" & Natural'Image (Page + 1) &
        "  Records" & Natural'Image (Visible_Count) & " of" & Natural'Image (Count) & "  Service lost" & Unsigned_64'Image (Lost_Records) &
        "  Viewer dropped" & Unsigned_64'Image (Viewer_Dropped), Colors.text);
      if Status = P.Denied then
         Line (36, "Log read authority denied", Colors.danger);
      elsif Status not in P.OK | P.Empty | P.Gap then
         Line (36, "Logstore read failed: " & P.Status'Image (Status) &
           " (retrying)", Colors.danger);
      elsif Count = 0 then
         Line (36, "Waiting for driver startup records via logstore...", Colors.text);
      else
         if Filter = All_Sources then
            for I in 1 .. Summary_Count loop
               Line (40 + (I - 1) * Line_Height,
                 CuBit.Log_Records.Text (Summary (I)),
                 (if CuBit.Log_Records.Level (Summary (I)) >= CuBit.Log_Records.Warning
                  then Colors.danger else Colors.text));
            end loop;
         end if;
         for I in 0 .. Rows - 1 loop
            if First + I <= Visible_Count then
               Line ((if Filter = All_Sources then Top else 40) + I * Line_Height,
                 Shown_Text (Visible (First + I)),
                 (if CuBit.Log_Records.Level (Records (Visible (First + I))) >= CuBit.Log_Records.Warning
                  then Colors.danger else Colors.text));
            end if;
         end loop;
      end if;
   end Render;
   --  What Ctrl+C copies (docs/clipboard.md): the summary and the page shown,
   --  one record per line. Records are short; this is ample.
   COPY_CAPACITY : constant := 16_384;
   Copy_Form : Clipboard_Text.Builder (COPY_CAPACITY);
   procedure Copy_Shown (Win : CuBit.UI.App.Window) is
      Result : CuBit.UI.Clipboard.Outcome;
      First : Positive;
      procedure Line (Text : String) is
      begin
         if Copy_Form.Length > 0 then
            Clipboard_Text.Append (Copy_Form, [1 => ASCII.LF]);
         end if;
         Clipboard_Text.Append (Copy_Form, Text);
      end Line;
   begin
      Clipboard_Text.Clear (Copy_Form);
      Select_Visible;
      First := Page * Rows + 1;
      if Filter = All_Sources then
         for I in 1 .. Summary_Count loop
            Line (CuBit.Log_Records.Text (Summary (I)));
         end loop;
      end if;
      for I in First .. Natural'Min (Visible_Count, First + Rows - 1) loop
         Line (Shown_Text (Visible (I)));
      end loop;
      CuBit.UI.Clipboard.Copy_Built (CuBit.UI.App.Surface_ID (Win), "", Copy_Form, Copy_Form, Result);
      debugPrint ("boot-logs: copy " & CuBit.UI.Clipboard.Image (Result) & ASCII.LF);
   end Copy_Shown;

   procedure Handle_Event (Win : in out CuBit.UI.App.Window;
     Event : CuBit.UI.App.Input_Event; Dirty : in out Rect; Running : in out Boolean) is
      use type Clipboard_Chords.Command;
   begin
      if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN and then
        CuBit.UI.Clipboard.Command_Of (Event.payload0, Event.payload1) = Clipboard_Chords.Copy_Command
      then
         Copy_Shown (Win);
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN and then Event.payload0 = KEY_TAB then
         --  All, then each source in the order seen, then all again.
         if Filter = One_Source and then Filter_Source < Source_Total then
            Filter_Source := Filter_Source + 1;
         elsif Filter /= One_Source and then Source_Total > 0 then
            Filter := One_Source;
            Filter_Source := 1;
         else
            Filter := All_Sources;
         end if;
         Page := 0;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN and then Event.payload0 = KEY_B then
         Filter := (if Filter = Bring_Up then All_Sources else Bring_Up);
         Page := 0;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Rows := Positive'Max (1, (CuBit.UI.App.Height (Win) - 80 -
           (Summary_Capacity + 1) * Line_Height) / Line_Height);
         Select_Visible;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      end if;
   end Handle_Event;
   function Deadline return Unsigned_64 is (Due);
   procedure Tick (Win : in out CuBit.UI.App.Window; Dirty : in out Rect; Running : in out Boolean) is
      Event : P.Event;
      Lost : Unsigned_64;
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      --  Sources were asked for this time already.
      Asked : Boolean := False;
   begin
      Due := Now + 250;
      if Status not in P.OK | P.Empty | P.Gap then
         CuBit.Logging.Subscribe (Reader, Status);
      end if;
      if Status in P.OK | P.Empty | P.Gap then
         for I in 1 .. 8 loop
            CuBit.Logging.Read_Next (Reader, Event, Lost, Status);
            if Status = P.Gap then
               Lost_Records := Lost_Records + Lost;
               Dirty := CuBit.UI.App.Full_Rect (Win);
            end if;
            exit when Status /= P.OK;
            if Pinned (Event.Data) then
               Pin (Event.Data);
            end if;
            if Count < Records'Length then
               Count := Count + 1;
            else
               -- Preserve the latest diagnosis and final capture summary.
               for J in Records'First .. Records'Last - 1 loop
                  Records (J) := Records (J + 1);
                  Record_Sources (J) := Record_Sources (J + 1);
               end loop;
               Viewer_Dropped := Viewer_Dropped + 1;
            end if;
            Records (Count) := Event.Data;
            Record_Sources (Count) := Event.Source;
            --  A new source: ask logstore who it is before showing it.
            if Known_Index (Event.Source) = 0 and then not Asked then
               Refresh_Sources;
               Asked := True;
            end if;
            --  As shown, but without the exit mark: the exit is its own line.
            declare
               Label : constant String := Label_Of (Event.Source);
               Text : constant String := CuBit.Log_Records.Text (Event.Data);
            begin
               debugPrint ("boot-logs: " &
                           (if Source_Of (Event.Data)'Length = 0 and then Label'Length > 0
                            then Label & ": " else "") & Text & ASCII.LF);
            end;
            Remember_Source (Count);
            Dirty := CuBit.UI.App.Full_Rect (Win);
         end loop;
      end if;
      --  Exits since the last look (and names logstore learned late).
      if Now >= Names_Due then
         Names_Due := Now + NAMES_MS;
         if Count > 0 then
            Refresh_Sources;
            Dirty := CuBit.UI.App.Full_Rect (Win);
         end if;
      end if;
      if Now >= Turn_Page then
         Select_Visible;
         Page := (if Visible_Count = 0 then 0 else (Page + 1) mod ((Visible_Count + Rows - 1) / Rows));
         Turn_Page := Now + 8000;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      end if;
      if Status /= Reported_Status then
         debugPrint ("boot-logs: reader status=" & P.Status'Image (Status) & ASCII.LF);
         Reported_Status := Status;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      end if;
   end Tick;
   --  Run never blocks in a call to Desktop (Activity_Wait): its input
   --  wait is submitted, and completes when input, or the deadline, arrives.
   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event,
      Next_Deadline => Deadline, On_Deadline => Tick, Activity_Wait => True);
begin
   CuBit.UI.App.Open (Win, 950, 610,
     CuBit.Desktop_Protocol.Feature_Bits
       ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
         CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
         CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
         others => False]),
     Opened, title => "CuBit boot diagnostics", protected_frames => True);
   if Opened then
      Due := syscall (SYSCALL_GETTIME) + 1;
      debugPrint ("boot-logs: window ready" & ASCII.LF);
      Run (Win);
      CuBit.UI.App.Close (Win);
   end if;
end Main;

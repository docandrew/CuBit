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
with Boot_Logs_Repaint;
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
   --  A page is anchored to the records it shows, not to positions: Anchor
   --  is the number (since the viewer started) of its first record. Once
   --  the viewer is full, each new record drops the oldest and shifts every
   --  position; the page shown stays put (only the counts line changes)
   --  until its own records are dropped or it turns.
   Anchor : Unsigned_64 := 0;
   --  The page's first row, as a position in Visible (Select_Visible).
   Shown_First : Positive := 1;
   function Number_Of (Index : Positive) return Unsigned_64 is
     (Viewer_Dropped + Unsigned_64 (Index));
   procedure Select_Visible is
   begin
      Visible_Count := 0;
      for I in 1 .. Count loop
         if Shown (I) then
            Visible_Count := Visible_Count + 1;
            Visible (Visible_Count) := I;
         end if;
      end loop;
      Shown_First := 1;
      while Shown_First <= Visible_Count and then Number_Of (Visible (Shown_First)) < Anchor loop
         Shown_First := Shown_First + 1;
      end loop;
      if Shown_First > Visible_Count then
         --  Every record at or after the anchor is gone: the last page.
         Shown_First := (if Visible_Count = 0 then 1 else ((Visible_Count - 1) / Rows) * Rows + 1);
      end if;
      Page := (Shown_First - 1) / Rows;
   end Select_Visible;

   --  Where the counts line is drawn: its top, and the band repainted for
   --  it (to the window's bottom edge).
   COUNTS_FROM_BOTTOM : constant := 30;
   function Counts_Text return String is
     ("Page" & Natural'Image (Page + 1) &
      "  Records" & Natural'Image (Visible_Count) & " of" & Natural'Image (Count) &
      "  Service lost" & Unsigned_64'Image (Lost_Records) &
      "  Viewer dropped" & Unsigned_64'Image (Viewer_Dropped));
   function Counts_Rect (Win : CuBit.UI.App.Window) return Rect is
     (0, Natural'Max (CuBit.UI.App.Height (Win), COUNTS_FROM_BOTTOM) - COUNTS_FROM_BOTTOM,
      CuBit.UI.App.Width (Win), Natural'Min (CuBit.UI.App.Height (Win), COUNTS_FROM_BOTTOM));

   --  Every line of the window above the counts line, in drawing order.
   procedure Body_Lines
     (Visit : not null access procedure (Y : Natural; Text : String; Danger : Boolean))
   is
      First : Natural;
      --  Rows below the pinned summary (and its divider line).
      Top : constant Natural := 40 + (if Summary_Count = 0 then 0
                                      else (Summary_Count + 1) * Line_Height);
   begin
      Select_Visible;
      First := Shown_First;
      Visit (10, "Boot diagnostics: " & Filter_Text & " (Tab: next source, B: bring-up); pages rotate every 8 s",
             False);
      if Status = P.Denied then
         Visit (36, "Log read authority denied", True);
      elsif Status not in P.OK | P.Empty | P.Gap then
         Visit (36, "Logstore read failed: " & P.Status'Image (Status) & " (retrying)", True);
      elsif Count = 0 then
         Visit (36, "Waiting for driver startup records via logstore...", False);
      else
         if Filter = All_Sources then
            for I in 1 .. Summary_Count loop
               Visit (40 + (I - 1) * Line_Height,
                 CuBit.Log_Records.Text (Summary (I)),
                 CuBit.Log_Records.Level (Summary (I)) >= CuBit.Log_Records.Warning);
            end loop;
         end if;
         for I in 0 .. Rows - 1 loop
            if First + I <= Visible_Count then
               Visit ((if Filter = All_Sources then Top else 40) + I * Line_Height,
                 Shown_Text (Visible (First + I)),
                 CuBit.Log_Records.Level (Records (Visible (First + I))) >= CuBit.Log_Records.Warning);
            end if;
         end loop;
      end if;
   end Body_Lines;

   --  What the window shows now, and what it showed when last painted.
   BODY_SNAPSHOT_BYTES : constant := 64 * 1024;
   COUNTS_SNAPSHOT_BYTES : constant := 256;
   Body_Now, Painted_Body : Boot_Logs_Repaint.Snapshot (BODY_SNAPSHOT_BYTES);
   Counts_Now, Painted_Counts : Boot_Logs_Repaint.Snapshot (COUNTS_SNAPSHOT_BYTES);
   procedure Add_Body_Line (Y : Natural; Text : String; Danger : Boolean) is
   begin
      Boot_Logs_Repaint.Add_Line (Body_Now, Y, Text, Danger);
   end Add_Body_Line;
   procedure Take_Snapshot is
   begin
      Boot_Logs_Repaint.Clear (Body_Now);
      Body_Lines (Add_Body_Line'Access);
      Boot_Logs_Repaint.Clear (Counts_Now);
      Boot_Logs_Repaint.Add_Line (Counts_Now, 0, Counts_Text, False);
   end Take_Snapshot;
   function Covers (Outer, Inner : Rect) return Boolean is
     (Outer.x <= Inner.x and then Outer.y <= Inner.y and then
      Outer.x + Outer.w >= Inner.x + Inner.w and then Outer.y + Outer.h >= Inner.y + Inner.h);

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      C : constant Canvas := CuBit.UI.App.Canvas (Win, Damage);
      Colors : constant Theme := Current_Theme;
      --  Every line elided to keep MARGIN clear of the right edge too.
      Lines : constant Rect := (MARGIN, 0, Natural'Max (CuBit.UI.App.Width (Win), 2 * MARGIN) - 2 * MARGIN,
                                CuBit.UI.App.Height (Win));
      procedure Line (Y : Natural; Text : String; Danger : Boolean) is
      begin
         Draw_UI_Text_Fitted (C, Lines, MARGIN, Y, Text,
                              (if Danger then Colors.danger else Colors.text), Colors.panel);
      end Line;
   begin
      Fill_Rect (C, CuBit.UI.App.Full_Rect (Win), Colors.panel);
      Body_Lines (Line'Access);
      Line (Counts_Rect (Win).y, Counts_Text, False);
      --  Remember what is on screen now, for the next change (Tick).
      Take_Snapshot;
      if Covers (Damage, CuBit.UI.App.Full_Rect (Win)) then
         Painted_Body := Body_Now;
      end if;
      if Covers (Damage, Counts_Rect (Win)) then
         Painted_Counts := Counts_Now;
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
      First := Shown_First;
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
         Anchor := 0;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN and then Event.payload0 = KEY_B then
         Filter := (if Filter = Bring_Up then All_Sources else Bring_Up);
         Page := 0;
         Anchor := 0;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Rows := Positive'Max (1, (CuBit.UI.App.Height (Win) - 80 -
           (Summary_Capacity + 1) * Line_Height) / Line_Height);
         Select_Visible;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      end if;
   end Handle_Event;
   function Deadline return Unsigned_64 is (Due);
   --  How many changes Tick repainted whole, as the counts line, or not.
   DECIDED_REPORT_MS : constant := 10_000;
   type Need_Counts is array (Boot_Logs_Repaint.Need) of Natural;
   Decided : Need_Counts := [others => 0];
   Decided_Report_Due : Unsigned_64 := 0;
   Decision : Boot_Logs_Repaint.Need;
   use type Boot_Logs_Repaint.Need;
   --  The first line a whole repaint of the period changed (serial evidence).
   type Example_Text is record
      Length : Natural range 0 .. 96 := 0;
      Text : String (1 .. 96) := [others => ' '];
   end record;
   Whole_Example : Example_Text;
   procedure Tick (Win : in out CuBit.UI.App.Window; Dirty : in out Rect; Running : in out Boolean) is
      Event : P.Event;
      Lost : Unsigned_64;
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      --  Sources were asked for this time already.
      Asked : Boolean := False;
      --  Something shown may have changed: compare before repainting.
      Changed : Boolean := False;
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
               Changed := True;
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
            Changed := True;
         end loop;
      end if;
      --  Exits since the last look (and names logstore learned late).
      if Now >= Names_Due then
         Names_Due := Now + NAMES_MS;
         if Count > 0 then
            Refresh_Sources;
            Changed := True;
         end if;
      end if;
      if Now >= Turn_Page then
         Select_Visible;
         --  The next page, after the last the first.
         Anchor := (if Shown_First + Rows <= Visible_Count then Number_Of (Visible (Shown_First + Rows))
                    else 0);
         Turn_Page := Now + 8000;
         Changed := True;
      end if;
      if Status /= Reported_Status then
         debugPrint ("boot-logs: reader status=" & P.Status'Image (Status) & ASCII.LF);
         Reported_Status := Status;
         Changed := True;
      end if;
      if Changed then
         Take_Snapshot;
         Decision := Boot_Logs_Repaint.Decide (Painted_Body, Body_Now, Painted_Counts, Counts_Now);
         if Decision = Boot_Logs_Repaint.Whole and then Whole_Example.Length = 0 then
            declare
               Line : constant String := Boot_Logs_Repaint.First_Change (Painted_Body, Body_Now);
               Kept : constant Natural := Natural'Min (Line'Length, Whole_Example.Text'Length);
            begin
               Whole_Example.Length := Kept;
               Whole_Example.Text (1 .. Kept) := Line (Line'First .. Line'First + Kept - 1);
            end;
         end if;
         Decided (Decision) := Decided (Decision) + 1;
         case Decision is
            when Boot_Logs_Repaint.Whole => Dirty := CuBit.UI.App.Full_Rect (Win);
            when Boot_Logs_Repaint.Counts_Only => Dirty := Union_Rect (Dirty, Counts_Rect (Win));
            when Boot_Logs_Repaint.Nothing => null;
         end case;
      end if;
      --  Serial only (not logstore, so it feeds no viewer): the repaints of
      --  the last period, each of which was a whole-window repaint before.
      if Now >= Decided_Report_Due then
         Decided_Report_Due := Now + DECIDED_REPORT_MS;
         if Decided /= Need_Counts'[others => 0] then
            debugPrint ("boot-logs: changes whole=" & Natural'Image (Decided (Boot_Logs_Repaint.Whole)) &
                        " counts_only=" & Natural'Image (Decided (Boot_Logs_Repaint.Counts_Only)) &
                        " nothing=" & Natural'Image (Decided (Boot_Logs_Repaint.Nothing)) &
                        " in" & Natural'Image (DECIDED_REPORT_MS) & " ms" &
                        (if Whole_Example.Length = 0 then ""
                         else "; first whole change: " & Whole_Example.Text (1 .. Whole_Example.Length)) &
                        ASCII.LF);
            Decided := [others => 0];
            Whole_Example.Length := 0;
         end if;
      end if;
   end Tick;
   --  Run never blocks in a call to Desktop (Activity_Wait): its input
   --  wait is submitted, and completes when input, or the deadline, arrives.
   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event,
      Next_Deadline => Deadline, On_Deadline => Tick, Activity_Wait => True);
begin
   declare
      Open_Outcome : CuBit.UI.App.Open_Result;
   begin
      CuBit.UI.App.Open (Win, 950, 610,
        CuBit.Desktop_Protocol.Feature_Bits
          ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
            CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
            CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
            others => False]),
        Open_Outcome, title => "CuBit boot diagnostics", protected_frames => True);
      Opened := Open_Outcome in CuBit.UI.App.Opened;
      if not Opened then
         --  Never silent: without its window Boot Logs relays nothing.
         debugPrint ("boot-logs: no window (" & CuBit.UI.App.Image (Open_Outcome) & "); exiting" & ASCII.LF);
      end if;
   end;
   if Opened then
      Due := syscall (SYSCALL_GETTIME) + 1;
      debugPrint ("boot-logs: window ready" & ASCII.LF);
      Run (Win);
      CuBit.UI.App.Close (Win);
   end if;
end Main;

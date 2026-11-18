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
with CuBit.UI.Text_Views;
with Ada.Unchecked_Deallocation;
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
   Count : Natural := 0;
   Lost_Records : Unsigned_64 := 0;
   Viewer_Dropped : Unsigned_64 := 0;
   --  Records that explain a boot, pinned above the records so one
   --  photograph shows them: every Warning or worse, and the key startup
   --  lines (the renderer's result and why it stopped).
   --  Left (and right) text margin of every line.
   MARGIN : constant := 12;
   Summary_Capacity : constant := 6;
   Summary : array (1 .. Summary_Capacity) of CuBit.Log_Records.Log_Record;
   Summary_Count : Natural range 0 .. Summary_Capacity := 0;
   --  Counts the summary's changes (Pin), for repainting it only then.
   Summary_Version : Natural := 0;
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
      Summary_Version := Summary_Version + 1;
   end Pin;
   Due : Unsigned_64 := 0;
   Opened : Boolean;

   --  The window (UI-043): a header line, the pinned summary (all sources
   --  only), the records the filter shows in a Text_Views view (scrolled,
   --  selectable, following the newest record while scrolled to the end),
   --  and the counts line. A change repaints only the rows it changes
   --  (Text_Views.Changed), and a scroll copies the rows that only moved
   --  from the frame before (Text_Views.Draw_Moved).
   package TV renames CuBit.UI.Text_Views;
   type Text_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, Text_Access);
   --  The view's text now, and as the last frame drew it (often the same).
   View_Text, Drawn_Text : Text_Access := new String'("");
   View, Drawn_View : TV.View;
   --  What the last frame drew outside the view.
   Drawn_Header, Drawn_Counts : Text_Access := new String'("");
   Drawn_Summary : Natural := 0;
   Drawn_Summary_Version : Natural := 0;
   Drawn_Filter : Filter_Kind := All_Sources;
   Drawn_Status : P.Status := P.Unavailable;
   Visible_Count : Natural := 0;
   Frames, Rendered_Rows, Copied_Rows : Natural := 0;

   HEADER_Y : constant := 10;
   SUMMARY_Y : constant := 40;
   COUNTS_FROM_BOTTOM : constant := 30;
   function Header_Text return String is
     ("Boot diagnostics: " & Filter_Text & " (Tab: next source, B: bring-up; " &
      "Up/Down, Page Up/Down, Home/End scroll; drag selects, Ctrl+C copies)");
   function Status_Text return String is
     (if Status = P.Denied then "Log read authority denied"
      elsif Status not in P.OK | P.Empty | P.Gap then
        "Logstore read failed: " & P.Status'Image (Status) & " (retrying)"
      elsif Count = 0 then "Waiting for driver startup records via logstore..."
      else "");
   --  Rows above the view: the summary while all sources show, else none
   --  (a status line takes the summary's first row when there is one).
   function Summary_Rows return Natural is
     (if Status_Text'Length > 0 then 1
      elsif Filter = All_Sources and then Summary_Count > 0 then Summary_Count + 1
      else 0);
   function Counts_Text return String is
     ("Records" & Natural'Image (Visible_Count) & " of" & Natural'Image (Count) &
      "  Service lost" & Unsigned_64'Image (Lost_Records) &
      "  Viewer dropped" & Unsigned_64'Image (Viewer_Dropped) &
      (if View.Follow_End then "  (following)" else ""));
   function Counts_Rect (Win : CuBit.UI.App.Window) return Rect is
     (0, Natural'Max (CuBit.UI.App.Height (Win), COUNTS_FROM_BOTTOM) - COUNTS_FROM_BOTTOM,
      CuBit.UI.App.Width (Win), Natural'Min (CuBit.UI.App.Height (Win), COUNTS_FROM_BOTTOM));
   function Header_Rect (Win : CuBit.UI.App.Window) return Rect is
     (0, 0, CuBit.UI.App.Width (Win), SUMMARY_Y);
   function Summary_Rect (Win : CuBit.UI.App.Window; Rows : Natural) return Rect is
     (0, SUMMARY_Y, CuBit.UI.App.Width (Win), Rows * Line_Height);
   --  The view, between the summary and the counts line.
   function View_Box (Win : CuBit.UI.App.Window) return Rect is
      Top : constant Natural := SUMMARY_Y + Summary_Rows * Line_Height;
      Bottom : constant Natural := Counts_Rect (Win).y;
      Width : constant Natural := CuBit.UI.App.Width (Win);
   begin
      return (MARGIN, Top, (if Width > 2 * MARGIN then Width - 2 * MARGIN else 0),
              (if Bottom > Top + MARGIN then Bottom - Top - MARGIN / 2 else 0));
   end View_Box;

   --  The view's text: each record the filter shows, one per line.
   procedure Rebuild is
      Length : Natural := 0;
      Next : Text_Access;
      At_Char : Positive := 1;
   begin
      Visible_Count := 0;
      for I in 1 .. Count loop
         if Shown (I) then
            Visible_Count := Visible_Count + 1;
            Length := Length + Shown_Text (I)'Length + (if Visible_Count > 1 then 1 else 0);
         end if;
      end loop;
      Next := new String (1 .. Length);
      Visible_Count := 0;
      for I in 1 .. Count loop
         if Shown (I) then
            Visible_Count := Visible_Count + 1;
            if Visible_Count > 1 then
               Next (At_Char) := ASCII.LF;
               At_Char := At_Char + 1;
            end if;
            declare
               Line : constant String := Shown_Text (I);
            begin
               Next (At_Char .. At_Char + Line'Length - 1) := Line;
               At_Char := At_Char + Line'Length;
            end;
         end if;
      end loop;
      if View_Text /= Drawn_Text then
         Free (View_Text);
      end if;
      View_Text := Next;
   end Rebuild;

   --  What changed since the last frame, for Dirty.
   function Changes (Win : CuBit.UI.App.Window) return Rect is
      Box : constant Rect := View_Box (Win);
      Result : Rect := TV.Changed (Box, Drawn_View, Drawn_Text.all, View, View_Text.all);
   begin
      if Header_Text /= Drawn_Header.all or else Filter /= Drawn_Filter then
         Result := Union_Rect (Result, Header_Rect (Win));
      end if;
      if Summary_Rows /= Drawn_Summary then
         --  The view moved: everything below the header.
         return CuBit.UI.App.Full_Rect (Win);
      end if;
      if Summary_Rows > 0 and then
        (Status /= Drawn_Status or else Summary_Version /= Drawn_Summary_Version)
      then
         Result := Union_Rect (Result, Summary_Rect (Win, Summary_Rows));
      end if;
      if Counts_Text /= Drawn_Counts.all then
         Result := Union_Rect (Result, Counts_Rect (Win));
      end if;
      return Result;
   end Changes;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      C : constant Canvas := CuBit.UI.App.Canvas (Win, Damage);
      Colors : constant Theme := Current_Theme;
      --  Every line elided to keep MARGIN clear of the right edge too.
      Lines : constant Rect := (MARGIN, 0, Natural'Max (CuBit.UI.App.Width (Win), 2 * MARGIN) - 2 * MARGIN,
                                CuBit.UI.App.Height (Win));
      Box : constant Rect := View_Box (Win);
      procedure Line (Y : Natural; Text : String; Danger : Boolean) is
      begin
         Draw_UI_Text_Fitted (C, Lines, MARGIN, Y, Text,
                              (if Danger then Colors.danger else Colors.text), Colors.panel);
      end Line;
   begin
      Fill_Rect (C, CuBit.UI.App.Full_Rect (Win), Colors.panel);
      Line (HEADER_Y, Header_Text, False);
      if Status_Text'Length > 0 then
         Line (SUMMARY_Y, Status_Text, Status = P.Denied or else Status not in P.OK | P.Empty | P.Gap);
      elsif Filter = All_Sources then
         for I in 1 .. Summary_Count loop
            Line (SUMMARY_Y + (I - 1) * Line_Height, CuBit.Log_Records.Text (Summary (I)),
                  CuBit.Log_Records.Level (Summary (I)) >= CuBit.Log_Records.Warning);
         end loop;
      end if;
      TV.Sync (View, View_Text.all, Box);
      TV.Draw_Moved (C, CuBit.UI.App.Previous_Canvas (Win), Box, Colors,
                     Drawn_View, Drawn_Text.all, View, View_Text.all, Focused => False);
      Line (Counts_Rect (Win).y, Counts_Text, False);
      Rendered_Rows := Rendered_Rows + TV.Visible_Lines (Box) - TV.Last_Copied_Rows;
      Copied_Rows := Copied_Rows + TV.Last_Copied_Rows;
      Frames := Frames + 1;
      --  What this frame shows, for the next change.
      if Drawn_Text /= View_Text then
         Free (Drawn_Text);
         Drawn_Text := View_Text;
      end if;
      Drawn_View := View;
      Free (Drawn_Header);
      Drawn_Header := new String'(Header_Text);
      Free (Drawn_Counts);
      Drawn_Counts := new String'(Counts_Text);
      Drawn_Summary := Summary_Rows;
      Drawn_Summary_Version := Summary_Version;
      Drawn_Filter := Filter;
      Drawn_Status := Status;
   end Render;

   --  What Ctrl+C copies (docs/clipboard.md): the selection, or without one
   --  the records shown, one per line.
   procedure Copy_Shown (Win : CuBit.UI.App.Window) is
      Result : CuBit.UI.Clipboard.Outcome;
      Box : constant Rect := View_Box (Win);
   begin
      if TV.Selection_First (View) /= TV.Selection_Last (View) then
         CuBit.UI.Clipboard.Copy_Text (CuBit.UI.App.Surface_ID (Win), TV.Selection (View, View_Text.all),
                                       CuBit.UI.Clipboard.Ordinary, Result);
      else
         declare
            Everything : TV.View := View;
            Text : constant String := View_Text.all;
            First : Positive := Text'First;
            Last : Natural := Text'Last;
            Line : Positive := 1;
         begin
            --  The shown lines: from the first line's start to the end of
            --  the last one.
            Everything.Anchor := 1;
            for I in Text'Range loop
               if Text (I) = ASCII.LF then
                  Line := Line + 1;
                  if Line = View.First_Line then
                     First := I + 1;
                  elsif Line = View.First_Line + TV.Visible_Lines (Box) then
                     Last := I - 1;
                     exit;
                  end if;
               end if;
            end loop;
            CuBit.UI.Clipboard.Copy_Text (CuBit.UI.App.Surface_ID (Win),
                                          (if First <= Last then Text (First .. Last) else ""),
                                          CuBit.UI.Clipboard.Ordinary, Result);
         end;
      end if;
      debugPrint ("boot-logs: copy " & CuBit.UI.Clipboard.Image (Result) & ASCII.LF);
   end Copy_Shown;

   --  A new filter shows other records: back to the newest.
   procedure Refilter (Win : CuBit.UI.App.Window; Dirty : in out Rect) is
   begin
      View := (others => <>);
      Rebuild;
      TV.Sync (View, View_Text.all, View_Box (Win));
      Dirty := CuBit.UI.App.Full_Rect (Win);
   end Refilter;

   procedure Handle_Event (Win : in out CuBit.UI.App.Window;
     Event : CuBit.UI.App.Input_Event; Dirty : in out Rect; Running : in out Boolean) is
      use type Clipboard_Chords.Command;
      Box : constant Rect := View_Box (Win);
      Handled : Boolean;
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
         Refilter (Win, Dirty);
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN and then Event.payload0 = KEY_B then
         Filter := (if Filter = Bring_Up then All_Sources else Bring_Up);
         Refilter (Win, Dirty);
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then
         TV.Key (View, View_Text.all, Box, Natural (Event.payload0 and 16#FFFF#),
                 Shift => (Event.payload1 and CuBit.UI.App.KEYMOD_SHIFT) /= 0,
                 Ctrl => (Event.payload1 and CuBit.UI.App.KEYMOD_CTRL) /= 0, Handled => Handled);
         if Handled then
            Dirty := Union_Rect (Dirty, Changes (Win));
         end if;
      elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_WHEEL then
         TV.Wheel (View, View_Text.all, Box, CuBit.UI.Input.Pointer_Wheel_Delta (Event));
         Dirty := Union_Rect (Dirty, Changes (Win));
      elsif Event.kind in CuBit.UI.Input.INPUT_POINTER_DOWN | CuBit.UI.Input.INPUT_POINTER_MOVE |
                          CuBit.UI.Input.INPUT_POINTER_UP
      then
         declare
            Down : constant Boolean := (CuBit.UI.Input.Pointer_Buttons (Event) and 1) /= 0;
         begin
            if Event.kind = CuBit.UI.Input.INPUT_POINTER_MOVE and then not View.Selecting then
               return;
            end if;
            TV.Pointer (View, View_Text.all, Box, CuBit.UI.Input.Pointer_X (Event), CuBit.UI.Input.Pointer_Y (Event),
                        Down => Down,
                        Pressed => Event.kind = CuBit.UI.Input.INPUT_POINTER_DOWN,
                        Released => Event.kind = CuBit.UI.Input.INPUT_POINTER_UP,
                        Shift => (CuBit.UI.Input.Pointer_Modifiers (Event) and CuBit.UI.App.KEYMOD_SHIFT) /= 0);
            Dirty := Union_Rect (Dirty, Changes (Win));
         end;
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         TV.Sync (View, View_Text.all, Box);
         Dirty := CuBit.UI.App.Full_Rect (Win);
      end if;
   end Handle_Event;
   function Deadline return Unsigned_64 is (Due);

   --  Serial evidence (not logstore, so it feeds no viewer): per period,
   --  the frames painted and the view's rows drawn and copied.
   REPAINT_REPORT_MS : constant := 10_000;
   Report_Due : Unsigned_64 := 0;

   procedure Tick (Win : in out CuBit.UI.App.Window; Dirty : in out Rect; Running : in out Boolean) is
      Event : P.Event;
      Lost : Unsigned_64;
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      --  Sources were asked for this time already.
      Asked : Boolean := False;
      --  Something shown may have changed: rebuild and compare.
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
               --  The oldest record gives way. While the view is scrolled
               --  back, it keeps showing the same records: it moves up a
               --  line (and the selection with it) when the dropped record
               --  was one it lists.
               if Shown (Records'First) then
                  declare
                     Gone : constant Positive := Shown_Text (Records'First)'Length + 1;
                  begin
                     if not View.Follow_End and then View.First_Line > 1 then
                        View.First_Line := View.First_Line - 1;
                     end if;
                     View.Cursor := Positive'Max (1, View.Cursor - Integer'Min (Gone, View.Cursor - 1));
                     View.Anchor := Positive'Max (1, View.Anchor - Integer'Min (Gone, View.Anchor - 1));
                  end;
               end if;
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
      if Status /= Reported_Status then
         debugPrint ("boot-logs: reader status=" & P.Status'Image (Status) & ASCII.LF);
         Reported_Status := Status;
         Changed := True;
      end if;
      if Changed then
         Rebuild;
         TV.Sync (View, View_Text.all, View_Box (Win));
         Dirty := Union_Rect (Dirty, Changes (Win));
      end if;
      if Now >= Report_Due then
         Report_Due := Now + REPAINT_REPORT_MS;
         if Frames > 0 then
            debugPrint ("boot-logs: frames=" & Natural'Image (Frames) &
                        " rows_drawn=" & Natural'Image (Rendered_Rows) &
                        " rows_copied=" & Natural'Image (Copied_Rows) &
                        " in" & Natural'Image (REPAINT_REPORT_MS) & " ms" & ASCII.LF);
            Frames := 0;
            Rendered_Rows := 0;
            Copied_Rows := 0;
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

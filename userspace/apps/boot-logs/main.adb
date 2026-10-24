with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with Startup;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
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
   Line_Height : constant := 21;

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
   Bring_Up_Sources : constant array (1 .. 3) of access constant String :=
     [new String'("devmgr"), new String'("intel-gpu"), new String'("procmgr")];
   --  Set-1 scancodes (the desktop protocol's key payload).
   KEY_TAB : constant := 16#0F#;
   KEY_B   : constant := 16#30#;

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

   procedure Remember_Source (Item : CuBit.Log_Records.Log_Record) is
      Name : constant String := Source_Of (Item);
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

   function Shown (Item : CuBit.Log_Records.Log_Record) return Boolean is
      Name : constant String := Source_Of (Item);
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
        when Bring_Up => "bring-up (devmgr, intel-gpu, procmgr)");
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
         if Shown (Records (I)) then
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
                 CuBit.Log_Records.Text (Records (Visible (First + I))),
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
         Line (CuBit.Log_Records.Text (Records (Visible (I))));
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
            debugPrint ("boot-logs: " & CuBit.Log_Records.Text (Event.Data) & ASCII.LF);
            Remember_Source (Event.Data);
            if Pinned (Event.Data) then
               Pin (Event.Data);
            end if;
            if Count < Records'Length then
               Count := Count + 1; Records (Count) := Event.Data;
            else
               -- Preserve the latest diagnosis and final capture summary.
               for J in Records'First .. Records'Last - 1 loop
                  Records (J) := Records (J + 1);
               end loop;
               Records (Records'Last) := Event.Data;
               Viewer_Dropped := Viewer_Dropped + 1;
            end if;
            Dirty := CuBit.UI.App.Full_Rect (Win);
         end loop;
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

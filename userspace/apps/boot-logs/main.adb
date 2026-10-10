with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Desktop_Protocol;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
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
   Reader : CuBit.Logging.Reader;
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
   Summary_Capacity : constant := 6;
   Summary : array (1 .. Summary_Capacity) of CuBit.Log_Records.Log_Record;
   Summary_Count : Natural range 0 .. Summary_Capacity := 0;
   Key_Prefixes : constant array (1 .. 4) of access constant String :=
     [new String'("DESKTOP-VULKAN:"), new String'("intel-gpu: backing denied"),
      new String'("desktop: software rendering"), new String'("procmgr: render")];
   Line_Height : constant := 21;
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
   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      C : constant Canvas := CuBit.UI.App.Canvas (Win, Damage);
      Colors : constant Theme := Current_Theme;
      First : constant Natural := Page * Rows + 1;
      --  Rows below the pinned summary (and its divider line).
      Top : constant Natural := 40 + (if Summary_Count = 0 then 0
                                      else (Summary_Count + 1) * Line_Height);
   begin
      Fill_Rect (C, CuBit.UI.App.Full_Rect (Win), Colors.panel);
      Draw_UI_Text (C, 12, 10, "Boot diagnostics - key records first; pages rotate every 8 seconds", Colors.text, Colors.panel);
      Draw_UI_Text (C, 12, CuBit.UI.App.Height (Win) - 30, "Page" & Natural'Image (Page + 1) &
        "  Records" & Natural'Image (Count) & "  Service lost" & Unsigned_64'Image (Lost_Records) &
        "  Viewer dropped" & Unsigned_64'Image (Viewer_Dropped),
        Colors.text, Colors.panel);
      if Status = P.Denied then
         Draw_UI_Text (C, 12, 36, "Log read authority denied", Colors.danger, Colors.panel);
      elsif Status not in P.OK | P.Empty | P.Gap then
         Draw_UI_Text (C, 12, 36, "Logstore read failed: " & P.Status'Image (Status) &
           " (retrying)", Colors.danger, Colors.panel);
      elsif Count = 0 then
         Draw_UI_Text (C, 12, 36, "Waiting for driver startup records via logstore...", Colors.text, Colors.panel);
      else
         for I in 1 .. Summary_Count loop
            Draw_UI_Text (C, 12, 40 + (I - 1) * Line_Height,
              CuBit.Log_Records.Text (Summary (I)),
              (if CuBit.Log_Records.Level (Summary (I)) >= CuBit.Log_Records.Warning
               then Colors.danger else Colors.text), Colors.panel);
         end loop;
         for I in 0 .. Rows - 1 loop
            if First + I <= Count then
               Draw_UI_Text (C, 12, Top + I * Line_Height,
                 CuBit.Log_Records.Text (Records (First + I)), Colors.text, Colors.panel);
            end if;
         end loop;
      end if;
   end Render;
   procedure Handle_Event (Win : in out CuBit.UI.App.Window;
     Event : CuBit.UI.App.Input_Event; Dirty : in out Rect; Running : in out Boolean) is
   begin
      if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Rows := Positive'Max (1, (CuBit.UI.App.Height (Win) - 80 -
           (Summary_Capacity + 1) * Line_Height) / Line_Height);
         Page := Natural'Min (Page, (if Count = 0 then 0 else (Count - 1) / Rows));
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
         Page := (if Count = 0 then 0 else (Page + 1) mod ((Count + Rows - 1) / Rows));
         Turn_Page := Now + 8000;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      end if;
      if Status /= Reported_Status then
         debugPrint ("boot-logs: reader status=" & P.Status'Image (Status) & ASCII.LF);
         Reported_Status := Status;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      end if;
   end Tick;
   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event,
      Next_Deadline => Deadline, On_Deadline => Tick);
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

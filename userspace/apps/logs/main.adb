with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Process_Observer;
with CuBit.Desktop_Protocol;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.State;
with CuBit.UI.Controls;
with CuBit.UI.Input;
with Log_View;

--  Logs (docs/logs-app.md): every service's records, live, through the
--  log-observer role; sources named through procmgr's process-observer
--  role. The view (Log_View) holds the records, filters and drawing.
procedure Main is
   package P renames CuBit.Log_Protocol;
   package PO renames CuBit.Process_Observer;
   use type P.Status;
   use type CuBit.Messages.MessageTag;
   use type Log_View.Key_Name;

   KEY_BACKSPACE : constant Unsigned_64 := 16#0E#;
   KEY_TAB : constant Unsigned_64 := 16#0F#;
   KEY_ENTER : constant Unsigned_64 := 16#1C#;
   KEY_SPACE : constant Unsigned_64 := 16#39#;
   KEY_HOME : constant Unsigned_64 := 16#47#;
   KEY_UP : constant Unsigned_64 := 16#48#;
   KEY_PAGE_UP : constant Unsigned_64 := 16#49#;
   KEY_LEFT : constant Unsigned_64 := 16#4B#;
   KEY_RIGHT : constant Unsigned_64 := 16#4D#;
   KEY_END : constant Unsigned_64 := 16#4F#;
   KEY_DOWN : constant Unsigned_64 := 16#50#;
   KEY_PAGE_DOWN : constant Unsigned_64 := 16#51#;
   KEY_DELETE : constant Unsigned_64 := 16#53#;
   KEY_F3 : constant Unsigned_64 := 16#3D#;
   --  How often the stream ring is read, and the most records taken each
   --  time (local memory: no IPC per record).
   POLL_MS : constant := 100;
   BATCH : constant := 1_024;
   --  How often unnamed sources, and what logstore keeps, are looked up again.
   NAMES_MS : constant := 2_000;
   --  While records stream in, the window is redrawn at most this often;
   --  records are still taken every POLL_MS.
   STREAM_FRAME_MS : constant := 250;
   WINDOW_WIDTH : constant := 880;
   WINDOW_HEIGHT : constant := 600;

   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   Reader : CuBit.Logging.Reader;
   Status : P.Status := P.Unavailable;
   --  About 1.4 MiB of records, on the 16 MiB main stack (no heap here).
   View : Log_View.View_State;
   Due, Names_Due, Last_Frame : Unsigned_64 := 0;
   --  The view revision last drawn.
   Drawn : Unsigned_64 := 0;
   Opened : Boolean;
   --  A source seen since the names were last looked up.
   Unnamed : Boolean := False;
   Page : array (0 .. PO.Page_Bytes - 1) of Unsigned_8 := [others => 0] with Alignment => PO.Page_Bytes;

   --  Names for every running process: its manifest identity, else its name.
   procedure Refresh_Names is
      Loan : CuBit.Memory_Grants.Grant_Reference;
      Lent, Revoked : Boolean;
      Request : Message := NULL_MESSAGE;
      Reply_Tag : MessageTag;
   begin
      CuBit.Memory_Grants.Create_Via_Capability (CapabilitySlot (PO.Observer_Slot), Page'Address, 1, True, Loan, Lent);
      if not Lent then return; end if;
      Request.tag := (label => PO.List_Label, length => 1, flags => 0, reserved => 0);
      Request.words (0) := CuBit.Grant_References.Encode (Loan);
      Reply_Tag := capCall (CapabilitySlot (PO.Observer_Slot), Request, CuBit.Messages.Wait_Forever);
      CuBit.Memory_Grants.Revoke (Loan, Revoked);
      if Reply_Tag.label /= Unsigned_32 (PO.Status'Enum_Rep (PO.OK)) then return; end if;
      for Index in 0 .. Natural (Unsigned_64'Min (Request.words (1), PO.Page_Records)) - 1 loop
         declare
            Base : constant Natural := Index * PO.Record_Bytes;
            Pid : constant Unsigned_64 :=
              Unsigned_64 (Page (Base)) or Shift_Left (Unsigned_64 (Page (Base + 1)), 8) or
              Shift_Left (Unsigned_64 (Page (Base + 2)), 16) or Shift_Left (Unsigned_64 (Page (Base + 3)), 24);
            Identity_Length : constant Natural :=
              Natural'Min (Natural (Page (Base + PO.Identity_Length_Offset)), PO.Identity_Bytes);
            Name_Length : constant Natural := Natural'Min (Natural (Page (Base + PO.Name_Length_Offset)), PO.Name_Bytes);
            Started : Unsigned_64 := 0;
            Name : String (1 .. PO.Identity_Bytes);
            Length : Natural := 0;
         begin
            for I in reverse 0 .. 7 loop
               Started := Shift_Left (Started, 8) or Unsigned_64 (Page (Base + PO.Started_Offset + I));
            end loop;
            if Identity_Length > 0 then
               for I in 1 .. Identity_Length loop
                  Name (I) := Character'Val (Page (Base + PO.Identity_Offset + I - 1));
               end loop;
               Length := Identity_Length;
            else
               for I in 1 .. Name_Length loop
                  Name (I) := Character'Val (Page (Base + PO.Name_Offset + I - 1));
               end loop;
               Length := Name_Length;
            end if;
            if Length > 0 then
               Log_View.Name_Source (View, Pid, Name (1 .. Length), Started);
            end if;
         end;
      end loop;
   end Refresh_Names;

   --  What logstore keeps, through the observer's endpoint.
   procedure Refresh_Kept is
      Level : CuBit.Log_Records.Severity;
      Result : P.Status;
   begin
      CuBit.Logging.Get_Minimum (Level, Result);
      if Result = P.OK then
         Log_View.Set_Kept (View, Level);
      end if;
   end Refresh_Kept;

   --  A change the person chose in the keep box, through log-control.
   procedure Apply_Keep_Request is
      Level, Previous : CuBit.Log_Records.Severity;
      Requested : Boolean;
      Result : P.Status;
   begin
      Log_View.Take_Keep_Request (View, Level, Requested);
      if not Requested then
         return;
      end if;
      CuBit.Logging.Set_Minimum (Level, Previous, Result);
      case Result is
         when P.OK => null;
         when P.Denied | P.Unavailable =>
            Log_View.Set_Keep_Refused
              (View, "Not changed: this program needs log-control (request-service log-control read-write log-control)");
         when others =>
            Log_View.Set_Keep_Refused (View, "Not changed: logstore refused the request");
      end case;
      Refresh_Kept;
   end Apply_Keep_Request;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
   begin
      Log_View.Render (View, CuBit.UI.App.Canvas (Win, Damage), CuBit.UI.App.Full_Rect (Win), UI, Controls);
   end Render;

   procedure Handle_Event (Win : in out CuBit.UI.App.Window;
     Event : CuBit.UI.App.Input_Event; Dirty : in out Rect; Running : in out Boolean)
   is
      Redraw : Boolean := False;
      procedure Send (Item : Log_View.Event) is
      begin
         Log_View.Handle (View, Item, Controls, Redraw);
      end Send;
      Shift : constant Boolean := (Event.payload1 and CuBit.UI.App.KEYMOD_SHIFT) /= 0;
      Control : constant Boolean := (Event.payload1 and CuBit.UI.App.KEYMOD_CTRL) /= 0;
      function Key_Of (Code : Unsigned_64) return Log_View.Key_Name is
        (if Code = CuBit.UI.App.KEY_ESC then Log_View.Escape
         elsif Code = KEY_ENTER then Log_View.Enter
         elsif Code = KEY_BACKSPACE then Log_View.Backspace
         elsif Code = KEY_TAB then Log_View.Tab
         elsif Code = KEY_SPACE then Log_View.Space
         elsif Code = KEY_LEFT then Log_View.Left
         elsif Code = KEY_RIGHT then Log_View.Right
         elsif Code = KEY_DELETE then Log_View.Delete
         elsif Code = KEY_UP then Log_View.Up
         elsif Code = KEY_DOWN then Log_View.Down
         elsif Code = KEY_PAGE_UP then Log_View.Page_Up
         elsif Code = KEY_PAGE_DOWN then Log_View.Page_Down
         elsif Code = KEY_HOME then Log_View.Home
         elsif Code = KEY_END then Log_View.End_Key
         elsif Code = KEY_F3 then Log_View.F3
         else Log_View.No_Key);
   begin
      if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Send ((Kind => Log_View.Resize, X => CuBit.UI.App.Width (Win), Y => CuBit.UI.App.Height (Win), others => <>));
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then
         if Key_Of (Event.payload0) /= Log_View.No_Key then
            Send ((Kind => Log_View.Key_Event, Key => Key_Of (Event.payload0), Shift => Shift,
                   Control => Control, others => <>));
         end if;
      elsif Event.kind = CuBit.UI.Input.INPUT_TEXT then
         if (Event.payload0 and 16#FFFF_FFFF#) in 32 .. 126 then
            Send ((Kind => Log_View.Text_Event,
                   Character_Value => Character'Val (Event.payload0 and 16#FF#), others => <>));
         end if;
      elsif Event.kind in CuBit.UI.Input.INPUT_POINTER_DOWN | CuBit.UI.Input.INPUT_POINTER_UP |
                          CuBit.UI.Input.INPUT_POINTER_MOVE
      then
         --  After App.Run's retained dispatch; the view handles what it means.
         Send ((Kind => Log_View.Pointer_Event,
                Action => (if Event.kind = CuBit.UI.Input.INPUT_POINTER_DOWN then CuBit.UI.Controls.Pointer_Press
                           elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_UP then CuBit.UI.Controls.Pointer_Release
                           else CuBit.UI.Controls.Pointer_Move),
                X => CuBit.UI.Input.Pointer_X (Event), Y => CuBit.UI.Input.Pointer_Y (Event), others => <>));
      elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_WHEEL then
         Send ((Kind => Log_View.Wheel, Steps => CuBit.UI.Input.Pointer_Wheel_Delta (Event), others => <>));
      end if;
      Apply_Keep_Request;
      if Redraw then Dirty := CuBit.UI.App.Full_Rect (Win); end if;
   end Handle_Event;

   function Deadline return Unsigned_64 is (Due);

   procedure Tick (Win : in out CuBit.UI.App.Window; Dirty : in out Rect; Running : in out Boolean) is
      pragma Unreferenced (Running);
      Event : P.Event;
      Lost : Unsigned_64;
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   begin
      Due := Now + POLL_MS;
      Log_View.Set_Time (View, Now);
      if Status not in P.OK | P.Empty | P.Gap then
         CuBit.Logging.Subscribe (Reader, Status);
         Log_View.Set_Connection
           (View, (if Status in P.OK | P.Empty | P.Gap then Log_View.Connected
                       elsif Status = P.Denied then Log_View.Denied else Log_View.Unavailable));
         Dirty := CuBit.UI.App.Full_Rect (Win);
      end if;
      if Status in P.OK | P.Empty | P.Gap then
         for I in 1 .. BATCH loop
            CuBit.Logging.Read_Next (Reader, Event, Lost, Status);
            if Status = P.Gap then
               Log_View.Add_Gap (View, Lost);
            elsif Status = P.OK then
               Log_View.Add (View, Event.Monotonic_Ms, Event.Source, Event.Data, Event.Node);
               Unnamed := True;
            end if;
            exit when Status not in P.OK | P.Gap;
         end loop;
         if Status not in P.OK | P.Empty | P.Gap then
            Log_View.Set_Connection (View, Log_View.Unavailable);
         end if;
      end if;
      if Now >= Names_Due then
         if Unnamed then
            Refresh_Names;
            Unnamed := False;
         end if;
         --  Another log-control holder may have changed what logstore keeps.
         Refresh_Kept;
         Names_Due := Now + NAMES_MS;
      end if;
      --  Only what changed, and not more often than STREAM_FRAME_MS.
      if Log_View.Revision (View) /= Drawn and then Now >= Last_Frame + STREAM_FRAME_MS then
         Dirty := CuBit.UI.Union_Rect (Dirty, Log_View.Content_Area (CuBit.UI.App.Full_Rect (Win)));
         Drawn := Log_View.Revision (View);
         Last_Frame := Now;
      end if;
   end Tick;

   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event,
      Next_Deadline => Deadline, On_Deadline => Tick);
begin
   CuBit.Logging.Announce ("logs: started", Opened);
   Log_View.Initialize (View);
   CuBit.UI.App.Open (Win, WINDOW_WIDTH, WINDOW_HEIGHT,
     CuBit.Desktop_Protocol.Feature_Bits
       ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
         CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
         CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
         others => False]),
     Opened, title => "Logs", protected_frames => True);
   if Opened then
      Log_View.Handle
        (View, (Kind => Log_View.Resize, X => CuBit.UI.App.Width (Win), Y => CuBit.UI.App.Height (Win),
                    others => <>), Controls, Opened);
      Due := syscall (SYSCALL_GETTIME) + 1;
      debugPrint ("logs: window ready" & ASCII.LF);
      Run (Win);
      CuBit.UI.App.Close (Win);
   end if;
end Main;

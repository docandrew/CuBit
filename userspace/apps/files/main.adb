------------------------------------------------------------------------------
--  CuBit Files (docs/files-app.md)
--  The platform glue only: the view (Files_View) holds the panes, listings,
--  operations and drawing, the same units the hosted harness runs
--  (tests/files-app). This translates the desktop's input into view events,
--  runs the view's pump between frames, and waits once, in CuBit.UI.App.Run
--  (Activity_Wait), for input, the filesystem's wake (OP_FS_WAKE) or the
--  view's next deadline. Nothing here blocks on the filesystem.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol;
with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Monotonic;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Icons;
with CuBit.UI.Input;
with CuBit.UI.State;
with Files_Limits;
with Files_Queue;
with Files_View;

procedure Main is
   use Files_View;
   use type CuBit.UI.Controls.Pointer_Action;

   --  Set-1 scan codes the desktop delivers (payload0 of a key event).
   KEY_ESCAPE : constant Unsigned_64 := CuBit.UI.App.KEY_ESC;
   KEY_BACKSPACE : constant Unsigned_64 := 16#0E#;
   KEY_TAB : constant Unsigned_64 := 16#0F#;
   KEY_W : constant Unsigned_64 := 16#11#;
   KEY_R : constant Unsigned_64 := 16#13#;
   KEY_T : constant Unsigned_64 := 16#14#;
   KEY_U : constant Unsigned_64 := 16#16#;
   KEY_I : constant Unsigned_64 := 16#17#;
   KEY_ENTER : constant Unsigned_64 := 16#1C#;
   KEY_A : constant Unsigned_64 := 16#1E#;
   KEY_D : constant Unsigned_64 := 16#20#;
   KEY_B : constant Unsigned_64 := 16#30#;
   KEY_N : constant Unsigned_64 := 16#31#;
   KEY_SPACE : constant Unsigned_64 := 16#39#;
   KEY_F1 : constant Unsigned_64 := 16#3B#;
   KEY_F10 : constant Unsigned_64 := 16#44#;
   KEY_HOME : constant Unsigned_64 := 16#47#;
   KEY_UP : constant Unsigned_64 := 16#48#;
   KEY_PAGE_UP : constant Unsigned_64 := 16#49#;
   KEY_LEFT : constant Unsigned_64 := 16#4B#;
   KEY_RIGHT : constant Unsigned_64 := 16#4D#;
   KEY_END : constant Unsigned_64 := 16#4F#;
   KEY_DOWN : constant Unsigned_64 := 16#50#;
   KEY_PAGE_DOWN : constant Unsigned_64 := 16#51#;
   KEY_INSERT : constant Unsigned_64 := 16#52#;
   KEY_DELETE : constant Unsigned_64 := 16#53#;
   KEY_F11 : constant Unsigned_64 := 16#57#;
   KEY_F12 : constant Unsigned_64 := 16#58#;
   --  Pointer events carry the held buttons in payload1, never modifiers:
   --  the primary button shares bit 0 with KEYMOD_SHIFT. Modifiers come from
   --  key events (payload1's low word) and INPUT_RESYNC (its high word).
   KEY_MODIFIERS_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;
   RESYNC_MODIFIERS_SHIFT : constant := 32;
   PRIMARY_BUTTON : constant Unsigned_64 := 1;
   SECONDARY_BUTTON : constant Unsigned_64 := 2;
   MIDDLE_BUTTON : constant Unsigned_64 := 4;
   --  Text events: the code point in payload0's low word.
   CODE_POINT_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;

   --  Fits a 1024x768 screen above the taskbar.
   INITIAL_WIDTH : constant := 860;
   INITIAL_HEIGHT : constant := 540;
   MINIMUM_WIDTH : constant := 480;
   MINIMUM_HEIGHT : constant := 280;
   --  One pane's explicit capacity (docs/files-app.md, "Limits"): entries
   --  and their names' bytes (24 per name on average).
   PANE_ENTRIES : constant := 262_144;
   AVERAGE_NAME_BYTES : constant := 24;
   PANE_NAME_BYTES : constant := PANE_ENTRIES * AVERAGE_NAME_BYTES;
   --  Sorting and filtering work per pump, between input checks.
   PUMP_BUDGET : constant Files_Limits.Work_Budget := 20_000;
   MICROSECONDS_PER_MILLISECOND : constant := 1_000;
   --  The places the manifest grants (filesystem-scope).
   VOLUME_PLACE : constant String := "@nvme:0/";
   WORKSPACE_PLACE : constant String := "@mem:0/";

   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   View : View_State;
   Logger : CuBit.Logging.Publisher;
   Opened : Boolean;
   --  More pump work waits: Run comes straight back (deadline now).
   Busy : Boolean := True;
   Buttons : Unsigned_64 := 0;
   --  The desktop's modifier state as of the latest key or resync event.
   Modifiers : Unsigned_64 := 0;
   Last_Pump_Us : Unsigned_64 := 0;
   First_Frame : Boolean := True;

   --  What the markers below last reported, per pane.
   type Path_Text is record
      Text : String (1 .. 512) := [others => ' '];
      Length : Natural := 0;
   end record;
   type Settled_Table is array (Side) of Boolean;
   type Path_Table is array (Side) of Path_Text;
   Reported_Settled : Settled_Table := [others => False];
   Reported_Path : Path_Table;
   Reported_Active : Side := Left_Pane;
   Reported_Status : Path_Text;

   function Now_Us return Unsigned_64 is
      Reading : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (if Reading.Available then Reading.Microseconds
              else syscall (SYSCALL_GETTIME) * MICROSECONDS_PER_MILLISECOND);
   end Now_Us;

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Unsigned_64'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Same (Saved : Path_Text; Text : String) return Boolean is
     (Saved.Length = Natural'Min (Text'Length, Saved.Text'Length)
      and then Saved.Text (1 .. Saved.Length) = Text (Text'First .. Text'First + Saved.Length - 1));

   procedure Save (Saved : out Path_Text; Text : String) is
   begin
      Saved.Length := Natural'Min (Text'Length, Saved.Text'Length);
      Saved.Text := [others => ' '];
      Saved.Text (1 .. Saved.Length) := Text (Text'First .. Text'First + Saved.Length - 1);
   end Save;

   --  One timing record into logstore: no IPC, never waits (shed when full).
   procedure Log_Timing (Text : String; Name : String; Microseconds : Unsigned_64; Count : Unsigned_64 := 0) is
      use CuBit.Log_Records;
      Made : constant Decoded := Make (Text);
      Accepted : Boolean;
   begin
      if not Made.Success then return; end if;
      declare
         Timed : constant Decoded := With_Field (Made.Value, Name, Duration_Microseconds, Microseconds);
      begin
         if not Timed.Success then return; end if;
         if Count = 0 then
            CuBit.Logging.Emit (Logger, Timed.Value, Accepted);
         else
            declare
               Counted : constant Decoded := With_Field (Timed.Value, "entries", Unsigned_Integer, Count);
            begin
               if Counted.Success then
                  CuBit.Logging.Emit (Logger, Counted.Value, Accepted);
               end if;
            end;
         end if;
      end;
   end Log_Timing;

   --  Visibility: the serial markers (tests/headless) and the timing
   --  records follow what the view shows; nothing in the view depends on
   --  them.
   procedure Report is
   begin
      for Pane in Side range 1 .. Pane_Count (View) loop
         declare
            Where : constant String := Path (View, Pane);
            Done : constant Boolean := Settled (View, Pane) and then Load (View, Pane) in Loaded | Failed;
         begin
            if not Same (Reported_Path (Pane), Where) then
               Save (Reported_Path (Pane), Where);
               Reported_Settled (Pane) := False;
               debugPrint ("files: pane" & Side'Image (Pane) & " entered " & Where & ASCII.LF);
            end if;
            if Done and then not Reported_Settled (Pane) then
               Reported_Settled (Pane) := True;
               if Load (View, Pane) = Loaded then
                  debugPrint ("files: pane" & Side'Image (Pane) & " listed " & Where & " entries="
                              & Decimal (Unsigned_64 (Listed (View, Pane))) & " us="
                              & Decimal (Listing_Us (View, Pane)) & ASCII.LF);
                  Log_Timing ("files: listed " & Where, "listing", Listing_Us (View, Pane),
                              Unsigned_64 (Listed (View, Pane)));
               else
                  debugPrint ("files: pane" & Side'Image (Pane) & " failed " & Where & ASCII.LF);
               end if;
            elsif not Done then
               Reported_Settled (Pane) := False;
            end if;
         end;
      end loop;
      if Active (View) /= Reported_Active then
         Reported_Active := Active (View);
         debugPrint ("files: active pane" & Side'Image (Reported_Active) & ASCII.LF);
      end if;
      declare
         Message : constant String := Status_Message (View);
      begin
         if not Same (Reported_Status, Message) then
            Save (Reported_Status, Message);
            if Message'Length > 0 then
               debugPrint ("files: status " & Message & ASCII.LF);
            end if;
         end if;
      end;
   end Report;

   procedure Collect (Dirty : in out Rect) is
      Area : Rect;
   begin
      Take_Damage (View, Area);
      if not Is_Empty (Area) then
         Dirty := Union_Rect (Dirty, Area);
      end if;
      Report;
   end Collect;

   procedure Pump_View (Dirty : in out Rect) is
      Start : constant Unsigned_64 := Now_Us;
      Changed : Boolean;
   begin
      Pump (View, PUMP_BUDGET, Start, Busy, Changed);
      Last_Pump_Us := Now_Us - Start;
      Collect (Dirty);
   end Pump_View;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      Start : constant Unsigned_64 := Now_Us;
   begin
      Files_View.Render (View, CuBit.UI.App.Canvas (Win, Damage), CuBit.UI.App.Full_Rect (Win), UI, Controls);
      Note_Frame (View, Now_Us - Start, Last_Pump_Us);
      if First_Frame then
         First_Frame := False;
         debugPrint ("files: first frame presented render_us=" & Decimal (Now_Us - Start) & ASCII.LF);
         Log_Timing ("files: first frame", "render", Now_Us - Start);
      end if;
   end Render;

   function Key_Of (Code : Unsigned_64; Shift : Boolean) return Key_Name is
     (if Code = KEY_ESCAPE then Escape
      elsif Code = KEY_ENTER then Enter
      elsif Code = KEY_BACKSPACE then Backspace
      elsif Code = KEY_TAB then Tab
      elsif Code = KEY_SPACE then Space
      elsif Code = KEY_UP then Up
      elsif Code = KEY_DOWN then Down
      elsif Code = KEY_LEFT then Left
      elsif Code = KEY_RIGHT then Right
      elsif Code = KEY_PAGE_UP then Page_Up
      elsif Code = KEY_PAGE_DOWN then Page_Down
      elsif Code = KEY_HOME then Home
      elsif Code = KEY_END then End_Key
      elsif Code = KEY_INSERT then Insert
      elsif Code = KEY_DELETE then Delete
      --  Shift+F10 opens the context menu (no Menu key on most keyboards).
      elsif Code = KEY_F10 and then Shift then Menu_Key
      elsif Code in KEY_F1 .. KEY_F10 then Key_Name'Val (Key_Name'Pos (F1) + Integer (Code - KEY_F1))
      elsif Code = KEY_F11 then F11
      elsif Code = KEY_F12 then F12
      elsif Code = KEY_A then Letter_A
      elsif Code = KEY_B then Letter_B
      elsif Code = KEY_D then Letter_D
      elsif Code = KEY_I then Letter_I
      elsif Code = KEY_N then Letter_N
      elsif Code = KEY_R then Letter_R
      elsif Code = KEY_T then Letter_T
      elsif Code = KEY_U then Letter_U
      elsif Code = KEY_W then Letter_W
      else No_Key);

   procedure Handle_Event
     (Win : in out CuBit.UI.App.Window; Event : CuBit.UI.App.Input_Event; Dirty : in out Rect;
      Running : in out Boolean)
   is
      Redraw : Boolean := False;
      function Current_Modifiers return Unsigned_64 is
        (if Event.kind in CuBit.UI.Input.INPUT_KEY_DOWN | CuBit.UI.Input.INPUT_KEY_UP
         then Event.payload1 and KEY_MODIFIERS_MASK
         elsif Event.kind = CuBit.UI.Input.INPUT_RESYNC
         then Shift_Right (Event.payload1, RESYNC_MODIFIERS_SHIFT)
         else Modifiers);
      Shift : constant Boolean := (Current_Modifiers and CuBit.UI.App.KEYMOD_SHIFT) /= 0;
      Control : constant Boolean := (Current_Modifiers and CuBit.UI.App.KEYMOD_CTRL) /= 0;
      Alt : constant Boolean := (Current_Modifiers and CuBit.UI.App.KEYMOD_ALT) /= 0;
      Time_Ms : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      procedure Send (Item : Files_View.Event) is
      begin
         Set_Clock (View, Now_Us);
         Handle (View, Item, Controls, Redraw);
      end Send;
   begin
      Modifiers := Current_Modifiers;
      if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Send ((Kind => Resize, X => CuBit.UI.App.Width (Win), Y => CuBit.UI.App.Height (Win), others => <>));
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then
         if Key_Of (Event.payload0, Shift) /= No_Key then
            Send ((Kind => Key_Event, Key => Key_Of (Event.payload0, Shift), Shift => Shift, Control => Control,
                   Alt => Alt, others => <>));
         end if;
      elsif Event.kind = CuBit.UI.Input.INPUT_TEXT then
         if (Event.payload0 and CODE_POINT_MASK) in Character'Pos (' ') .. Character'Pos ('~') then
            Send ((Kind => Text_Event, Character_Value => Character'Val (Event.payload0 and CODE_POINT_MASK),
                   Control => Control, Alt => Alt, others => <>));
         end if;
      elsif Event.kind in CuBit.UI.Input.INPUT_POINTER_DOWN | CuBit.UI.Input.INPUT_POINTER_UP
                          | CuBit.UI.Input.INPUT_POINTER_MOVE
      then
         --  After App.Run's retained dispatch (capture, hover, damage); the
         --  view handles what the press means. The desktop reports only the
         --  primary button's edges as presses; the secondary's (context
         --  menus) and middle's (closing a tab) arrive as a change in the
         --  held buttons.
         declare
            Held : constant Unsigned_64 := Event.payload1 and (PRIMARY_BUTTON or SECONDARY_BUTTON or MIDDLE_BUTTON);
            X : constant Natural := CuBit.UI.Input.Pointer_X (Event);
            Y : constant Natural := CuBit.UI.Input.Pointer_Y (Event);
         begin
            if Event.kind = CuBit.UI.Input.INPUT_POINTER_MOVE
              and then (Held and SECONDARY_BUTTON) /= (Buttons and SECONDARY_BUTTON)
            then
               Send ((Kind => Pointer_Event,
                      Action => (if (Held and SECONDARY_BUTTON) /= 0 then CuBit.UI.Controls.Pointer_Press
                                 else CuBit.UI.Controls.Pointer_Release),
                      X => X, Y => Y, Time_Ms => Time_Ms, Secondary => True, Control => Control, Shift => Shift,
                      others => <>));
            elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_MOVE
              and then (Held and MIDDLE_BUTTON) /= (Buttons and MIDDLE_BUTTON)
            then
               Send ((Kind => Pointer_Event,
                      Action => (if (Held and MIDDLE_BUTTON) /= 0 then CuBit.UI.Controls.Pointer_Press
                                 else CuBit.UI.Controls.Pointer_Release),
                      X => X, Y => Y, Time_Ms => Time_Ms, Middle => True, others => <>));
            else
               Send ((Kind => Pointer_Event,
                      Action => (if Event.kind = CuBit.UI.Input.INPUT_POINTER_DOWN then CuBit.UI.Controls.Pointer_Press
                                 elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_UP
                                 then CuBit.UI.Controls.Pointer_Release
                                 else CuBit.UI.Controls.Pointer_Move),
                      X => X, Y => Y, Time_Ms => Time_Ms, Control => Control, Shift => Shift, others => <>));
            end if;
            Buttons := Held;
         end;
      elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_WHEEL then
         Send ((Kind => Wheel, Steps => CuBit.UI.Input.Pointer_Wheel_Delta (Event),
                X => CuBit.UI.Input.Pointer_X (Event), Y => CuBit.UI.Input.Pointer_Y (Event), others => <>));
      end if;
      --  Input often submits requests (a folder opened): move them now.
      Pump_View (Dirty);
      if Quit_Requested (View) then
         Running := False;
      end if;
   end Handle_Event;

   --  Run's deadline, in GETTIME milliseconds: now while pump work waits,
   --  the view's own deadline otherwise; zero for none (wait for input or
   --  the filesystem's wake only).
   function Deadline return Unsigned_64 is
      Due_Us : constant Unsigned_64 := Next_Deadline_Us (View);
      Now_Ms : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Current_Us : Unsigned_64;
   begin
      if Busy then
         return Now_Ms;
      elsif Due_Us = Unsigned_64'Last then
         return 0;
      end if;
      Current_Us := Now_Us;
      return (if Due_Us <= Current_Us then Now_Ms
              else Now_Ms + (Due_Us - Current_Us) / MICROSECONDS_PER_MILLISECOND + 1);
   end Deadline;

   procedure Tick (Win : in out CuBit.UI.App.Window; Dirty : in out Rect; Running : in out Boolean) is
      pragma Unreferenced (Win);
   begin
      Pump_View (Dirty);
      if Quit_Requested (View) then
         Running := False;
      end if;
   end Tick;

   procedure Completed
     (Win : in out CuBit.UI.App.Window; Receipt : CompletionEntry; Consumed : out Boolean; Dirty : in out Rect;
      Running : in out Boolean)
   is
      pragma Unreferenced (Win, Running);
      Woken : Boolean;
   begin
      Files_Queue.Complete (Receipt, Consumed, Woken);
      if Consumed then
         Pump_View (Dirty);
      end if;
   end Completed;

   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event, Next_Deadline => Deadline,
      On_Deadline => Tick, Activity_Wait => True, On_Completion => Completed);
begin
   debugPrint ("files: starting" & ASCII.LF);
   CuBit.Logging.Announce ("files: started", Opened);
   --  The first listings start here: time them from now.
   Set_Clock (View, Now_Us);
   Initialize (View, PANE_ENTRIES, PANE_NAME_BYTES, VOLUME_PLACE, WORKSPACE_PLACE);
   Add_Place (View, "NVMe volume 0", VOLUME_PLACE, CuBit.UI.Icons.Drive);
   Add_Place (View, "Live workspace", WORKSPACE_PLACE, CuBit.UI.Icons.Folder);
   debugPrint ((if Files_Queue.Ready then "files: filesystem queue ready" else "files: no filesystem queue")
               & ASCII.LF);
   CuBit.UI.App.Open
     (Win, INITIAL_WIDTH, INITIAL_HEIGHT,
      CuBit.Desktop_Protocol.Feature_Bits
        ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
          CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
          CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
          others => False]),
      Opened, title => "Files", protected_frames => True, batched_input => False,
      minimum_width => MINIMUM_WIDTH, minimum_height => MINIMUM_HEIGHT);
   if Opened then
      debugPrint ("files: native window ready" & ASCII.LF);
      declare
         Ignore : Boolean;
      begin
         Handle (View, (Kind => Resize, X => CuBit.UI.App.Width (Win), Y => CuBit.UI.App.Height (Win),
                        others => <>), Controls, Ignore);
      end;
      Run (Win);
      CuBit.UI.App.Close (Win);
   end if;
   Close (View);
   debugPrint ("files: closed" & ASCII.LF);
end Main;

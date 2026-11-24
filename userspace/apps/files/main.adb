------------------------------------------------------------------------------
--  CuBit Files (docs/files-app.md)
--  The platform glue only: the view (Files_View) holds the panes, listings,
--  operations and drawing, the same units the hosted harness runs
--  (tests/files-app). This translates the desktop's input into view events,
--  runs the view's pump between frames, and waits once, in CuBit.UI.App.Run
--  (Activity_Wait), for input, the filesystem's wake (OP_FS_WAKE) or the
--  view's next deadline. Nothing here blocks on the filesystem.
------------------------------------------------------------------------------
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with CuBit.UI_Metrics;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol;
with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Monotonic;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Clipboard;
with CuBit.UI.Editor_Clipboard;
with Clipboard_Chords;
with Clipboard_Text;
with CuBit.UI.Controls;
with CuBit.UI.Input;
with CuBit.UI.Keys;
with CuBit.UI.State;
with Files_Filter;
with Files_Limits;
with Files_Queue;
with Files_View;
with Startup;

procedure Main is
   use Files_View;
   use type CuBit.UI.Controls.Pointer_Action;

   --  Pointer events carry the held buttons in payload1, never modifiers:
   --  the primary button shares bit 0 with KEYMOD_SHIFT. Modifiers come from
   --  key events (payload1's low word) and INPUT_RESYNC (its high word).
   RESYNC_MODIFIERS_SHIFT : constant := 32;
   PRIMARY_BUTTON : constant Unsigned_64 := 1;
   SECONDARY_BUTTON : constant Unsigned_64 := 2;
   MIDDLE_BUTTON : constant Unsigned_64 := 4;
   --  Text events: the code point in payload0's low word.
   CODE_POINT_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;

   --  Fits a 1024x768 screen above the taskbar.
   INITIAL_WIDTH : constant := 62 * CuBit.UI_Metrics.Base_Em;
   INITIAL_HEIGHT : constant := 560;
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

   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   View : View_State;
   Logger : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Opened : Boolean;
   --  More pump work waits: Run comes straight back (deadline now).
   Busy : Boolean := True;
   Buttons : Unsigned_64 := 0;
   --  The desktop's modifier state as of the latest key or resync event.
   Modifiers : Unsigned_64 := 0;
   use type Clipboard_Chords.Command;
   --  The two forms of a copied selection: names or paths, one per line,
   --  and the List-String literal; at most the clipboard's own bound.
   Copy_Text_Form, Copy_Literal : Clipboard_Text.Builder (Clipboard_Text.Maximum_Form);
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
              else CuBit.Monotonic.Milliseconds * MICROSECONDS_PER_MILLISECOND);
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

   --  The held modifiers whose keys the frame reveals (UI-052), logged when
   --  they change: tests and traces read it.
   use type CuBit.UI.Keys.Modifier_Set;
   Logged_Reveal : CuBit.UI.Keys.Modifier_Set := CuBit.UI.Keys.No_Modifiers;
   function Reveal_Image (Set : CuBit.UI.Keys.Modifier_Set) return String is
     ((if Set (CuBit.UI.Keys.Ctrl) then " ctrl" else "") & (if Set (CuBit.UI.Keys.Shift) then " shift" else "")
      & (if Set (CuBit.UI.Keys.Alt) then " alt" else "")
      & (if Set = CuBit.UI.Keys.No_Modifiers then " none" else ""));

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      Start : constant Unsigned_64 := Now_Us;
   begin
      Files_View.Render (View, CuBit.UI.App.Canvas (Win, Damage), CuBit.UI.App.Full_Rect (Win), UI, Controls);
      if Revealed (View) /= Logged_Reveal then
         Logged_Reveal := Revealed (View);
         debugPrint ("files: revealed" & Reveal_Image (Logged_Reveal) & ASCII.LF);
      end if;
      Note_Frame (View, Now_Us - Start, Last_Pump_Us);
      if First_Frame then
         First_Frame := False;
         debugPrint ("files: first frame presented render_us=" & Decimal (Now_Us - Start) & ASCII.LF);
         Log_Timing ("files: first frame", "render", Now_Us - Start);
      end if;
   end Render;

   procedure Handle_Event
     (Win : in out CuBit.UI.App.Window; Event : CuBit.UI.App.Input_Event; Dirty : in out Rect;
      Running : in out Boolean)
   is
      Redraw : Boolean := False;
      function Current_Modifiers return Unsigned_64 is
        (if Event.kind in CuBit.UI.Input.INPUT_KEY_DOWN | CuBit.UI.Input.INPUT_KEY_UP
         then CuBit.UI.Input.Key_Modifiers (Event)
         elsif Event.kind = CuBit.UI.Input.INPUT_RESYNC
         then Shift_Right (Event.payload1, RESYNC_MODIFIERS_SHIFT)
         else Modifiers);
      --  Keys are decoded once, by the toolkit (CuBit.UI.Keys).
      Held : constant CuBit.UI.Keys.Modifier_Set := CuBit.UI.Keys.Modifiers_Of (Current_Modifiers);
      Shift : constant Boolean := Held (CuBit.UI.Keys.Shift);
      Control : constant Boolean := Held (CuBit.UI.Keys.Ctrl);
      Alt : constant Boolean := Held (CuBit.UI.Keys.Alt);
      Time_Ms : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
      procedure Send (Item : Files_View.Event) is
      begin
         Set_Clock (View, Now_Us);
         Handle (View, Item, Controls, Redraw);
      end Send;
      Command : constant Clipboard_Chords.Command :=
        (if Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN
         then CuBit.UI.Clipboard.Command_Of (Event.payload0, Current_Modifiers)
         else Clipboard_Chords.No_Command);

      --  Copy the active pane's selection (docs/clipboard.md): names, or
      --  full paths, as a typed List-String with a line-per-entry text form.
      procedure Copy_Selection (Form : Copy_Form) is
         Count : Natural;
         Result : CuBit.UI.Clipboard.Outcome;
      begin
         Selection_Copy (View, Form, Copy_Text_Form, Copy_Literal, Count);
         if Count = 0 then
            debugPrint ("files: nothing to copy" & ASCII.LF);
            return;
         end if;
         CuBit.UI.Clipboard.Copy_Built
           (CuBit.UI.App.Surface_ID (Win), Clipboard_Text.String_List_Type, Copy_Text_Form, Copy_Literal, Result);
         debugPrint ("files: copy" & Count'Image & (if Form = Copy_Paths then " paths " else " names ")
                     & CuBit.UI.Clipboard.Image (Result) & ASCII.LF);
      end Copy_Selection;

      --  A clipboard chord on the prompt's name field.
      procedure Prompt_Chord (Text : in out String; Length : in out Natural; Changed : out Boolean) is
         R : constant CuBit.UI.Editor_Clipboard.Result :=
           CuBit.UI.Editor_Clipboard.Handle_Line_Key
             (Text, Length, CuBit.UI.App.Surface_ID (Win), Event.payload0, Current_Modifiers);
      begin
         Changed := R.Changed;
         debugPrint ("files: prompt chord " & CuBit.UI.Clipboard.Image (R.Outcome) & ASCII.LF);
      end Prompt_Chord;
      procedure Edit_Prompt_Text is new Edit_Prompt (Prompt_Chord);

      procedure Paste_Into_Filter is
         --  Paste types into the filter, as if each character were typed.
         --  Line breaks end the paste: a filter is one line.
         Text : String (1 .. Files_Filter.MAXIMUM_QUERY);
         Last : Natural;
         Kind : CuBit.UI.Clipboard.Sensitivity;
         Result : CuBit.UI.Clipboard.Outcome;
         Count : Natural := 0;
         use type CuBit.UI.Clipboard.Outcome;
      begin
         CuBit.UI.Clipboard.Paste_Text
           (CuBit.UI.App.Surface_ID (Win), CuBit.UI.Clipboard.Ordinary, Text, Last, Kind, Result);
         if Result = CuBit.UI.Clipboard.Done then
            for I in Text'First .. Last loop
               exit when Text (I) not in ' ' .. '~';
               Send ((Kind => Text_Event, Character_Value => Text (I), others => <>));
               Count := Count + 1;
            end loop;
            debugPrint ("files: pasted" & Count'Image & " characters into the filter" & ASCII.LF);
         else
            debugPrint ("files: paste " & CuBit.UI.Clipboard.Image (Result) & ASCII.LF);
         end if;
      end Paste_Into_Filter;

      --  Ctrl+C copies names (Ctrl+Shift+C full paths). Ctrl+X does the
      --  same: Files has no cut of files to the clipboard (moving is F6).
      --  A prompt asking for a name takes the chords on its text; else a
      --  paste goes to the filter.
      procedure Clipboard_Key is
      begin
         if Prompt_Takes_Text (View) then
            Edit_Prompt_Text (View);
         elsif Command = Clipboard_Chords.Paste_Command then
            Paste_Into_Filter;
         else
            Copy_Selection
              ((if Shift and then Command = Clipboard_Chords.Copy_Command then Copy_Paths else Copy_Names));
         end if;
      end Clipboard_Key;

      procedure Take_Menu_Copy is
         Request : Clipboard_Request;
      begin
         Take_Clipboard_Request (View, Request);
         case Request is
            when No_Request => null;
            when Request_Copy_Names => Copy_Selection (Copy_Names);
            when Request_Copy_Paths => Copy_Selection (Copy_Paths);
         end case;
      end Take_Menu_Copy;
   begin
      Modifiers := Current_Modifiers;
      if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Send ((Kind => Resize, X => CuBit.UI.App.Width (Win), Y => CuBit.UI.App.Height (Win), others => <>));
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN and then
        Command in Clipboard_Chords.Copy_Command | Clipboard_Chords.Cut_Command | Clipboard_Chords.Paste_Command
      then
         Clipboard_Key;
      elsif Event.kind in CuBit.UI.Input.INPUT_KEY_DOWN | CuBit.UI.Input.INPUT_KEY_UP then
         --  Releases too: held modifiers reveal their hints (UI-052).
         if CuBit.UI.Keys.Is_Modifier_Key (CuBit.UI.Keys.Key_Of (Event.payload0)) then
            debugPrint ("files: modifier " & CuBit.UI.Keys.Label (CuBit.UI.Keys.Key_Of (Event.payload0))
                        & (if Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then " down" else " up") & ASCII.LF);
         end if;
         Send ((Kind => (if Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then Key_Event else Key_Release),
                Key => CuBit.UI.Keys.Key_Of (Event.payload0), Shift => Shift, Control => Control,
                Alt => Alt, others => <>));
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
      --  A Copy chosen from the context menu, by this event's click or Enter.
      Take_Menu_Copy;
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
      Now_Ms : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
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
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   debugPrint ("files: starting" & ASCII.LF);
   CuBit.Logging.Announce ("files: started", Opened, Startup.Logstore);
   --  The timing records' publisher: unbound, it would drop every record.
   --  The first listings start here: time them from now.
   Set_Clock (View, Now_Us);
   --  The places come from the service: the granted scopes (manifest)
   --  whose volume this machine has. No folder is remembered across runs
   --  yet, so both panes start at the first place.
   Initialize (View, PANE_ENTRIES, PANE_NAME_BYTES, "", "");
   debugPrint ((if Files_Queue.Ready then "files: filesystem queue ready" else "files: no filesystem queue")
               & ASCII.LF);
   declare
      Open_Outcome : CuBit.UI.App.Open_Result;
   begin
      CuBit.UI.App.Open
        (Win, INITIAL_WIDTH, INITIAL_HEIGHT,
         CuBit.Desktop_Protocol.Feature_Bits
           ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
             CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
             CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
             others => False]),
         Open_Outcome, title => "Files", protected_frames => True,
         minimum_width => MINIMUM_WIDTH, minimum_height => MINIMUM_HEIGHT);
      Opened := Open_Outcome in CuBit.UI.App.Opened;
   end;
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

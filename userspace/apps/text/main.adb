------------------------------------------------------------------------------
--  CuBit Text (docs/text-editor.md)
--  The platform glue only: the shell (Text_Shell) and the editor core
--  (userspace/lib/text) are the units the hosted harness runs
--  (tests/text-editor). This translates the desktop's input into shell
--  events, runs the shell's pump between frames, and waits once, in
--  CuBit.UI.App.Run (Activity_Wait), for input, the filesystem's wake or
--  the shell's next deadline. Nothing here blocks on the filesystem: files
--  load and save through Text_Host_Native, a request at a time.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol;
with CuBit.Logging;
with CuBit.Monotonic;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Input;
with CuBit.UI.State;
with CuBit.Startup_State;
with Startup;
with Text_Commands; use Text_Commands;
with Text_Documents;
with Text_Host_Native;
with Text_Limits;
with Text_Shell;

procedure Main is
   use type Text_Documents.Document_Access;
   --  Set-1 scan codes the desktop delivers (payload0 of a key event).
   KEY_ESCAPE : constant Unsigned_64 := CuBit.UI.App.KEY_ESC;
   KEY_MINUS : constant Unsigned_64 := 16#0C#;
   KEY_EQUALS : constant Unsigned_64 := 16#0D#;
   KEY_BACKSPACE : constant Unsigned_64 := 16#0E#;
   KEY_TAB : constant Unsigned_64 := 16#0F#;
   KEY_LEFT_BRACKET : constant Unsigned_64 := 16#1A#;
   KEY_RIGHT_BRACKET : constant Unsigned_64 := 16#1B#;
   KEY_ENTER : constant Unsigned_64 := 16#1C#;
   KEY_SEMICOLON : constant Unsigned_64 := 16#27#;
   KEY_QUOTE : constant Unsigned_64 := 16#28#;
   KEY_GRAVE : constant Unsigned_64 := 16#29#;
   KEY_BACKSLASH : constant Unsigned_64 := 16#2B#;
   KEY_COMMA : constant Unsigned_64 := 16#33#;
   KEY_PERIOD : constant Unsigned_64 := 16#34#;
   KEY_SLASH : constant Unsigned_64 := 16#35#;
   KEY_SPACE : constant Unsigned_64 := 16#39#;
   KEY_F1 : constant Unsigned_64 := 16#3B#;
   KEY_F10 : constant Unsigned_64 := 16#44#;
   KEY_F11 : constant Unsigned_64 := 16#57#;
   KEY_F12 : constant Unsigned_64 := 16#58#;
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
   --  Letters by set-1 code, in the keyboard's rows.
   type Letter_Row is array (Natural range <>) of Text_Commands.Key;
   TOP_ROW_FIRST : constant Unsigned_64 := 16#10#;     --  Q .. P
   TOP_ROW : constant Letter_Row := [Key_Q, Key_W, Key_E, Key_R, Key_T, Key_Y, Key_U, Key_I, Key_O, Key_P];
   HOME_ROW_FIRST : constant Unsigned_64 := 16#1E#;    --  A .. L
   HOME_ROW : constant Letter_Row := [Key_A, Key_S, Key_D, Key_F, Key_G, Key_H, Key_J, Key_K, Key_L];
   BOTTOM_ROW_FIRST : constant Unsigned_64 := 16#2C#;  --  Z .. M
   BOTTOM_ROW : constant Letter_Row := [Key_Z, Key_X, Key_C, Key_V, Key_B, Key_N, Key_M];
   DIGIT_ROW_FIRST : constant Unsigned_64 := 16#02#;   --  1 .. 9, 0
   DIGIT_ROW : constant Letter_Row := [Key_1, Key_2, Key_3, Key_4, Key_5, Key_6, Key_7, Key_8, Key_9, Key_0];

   --  Pointer events carry the held buttons in payload1; modifiers come from
   --  key events (payload1's low word) and INPUT_RESYNC (its high word).
   KEY_MODIFIERS_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;
   RESYNC_MODIFIERS_SHIFT : constant := 32;
   PRIMARY_BUTTON : constant Unsigned_64 := 1;
   CODE_POINT_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;
   FIRST_PRINTABLE : constant := 16#20#;
   DELETE_CODE : constant := 16#7F#;

   INITIAL_WIDTH : constant := 900;
   INITIAL_HEIGHT : constant := 600;
   MINIMUM_WIDTH : constant := 480;
   MINIMUM_HEIGHT : constant := 280;
   --  Room per document for typing and pasting beyond its file (explicit;
   --  the store grows in chunks only as text arrives).
   EDIT_BYTES : constant := 64 * 1_048_576;
   PUMP_BUDGET : constant Text_Limits.Work_Budget := 50_000;
   MICROSECONDS_PER_MILLISECOND : constant := 1_000;
   --  The document Text opens first, under its write grant.
   DOCUMENTS : constant String := "@nvme:0/Documents/";
   FIRST_DOCUMENT : constant String := DOCUMENTS & "notes.txt";

   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   Host : aliased Text_Host_Native.Host;
   App : Text_Shell.Shell;
   Opened : Boolean;
   Busy : Boolean := True;
   Modifiers : Unsigned_64 := 0;
   Buttons : Unsigned_64 := 0;
   First_Frame : Boolean := True;
   Last_Pump_Us : Unsigned_64 := 0;
   --  What the markers last reported.
   Was_Loading, Was_Saving : Boolean := False;
   Last_Message : String (1 .. 240) := [others => ' '];
   Last_Message_Length : Natural := 0;

   function Now_Us return Unsigned_64 is
      Reading : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (if Reading.Available then Reading.Microseconds
              else syscall (SYSCALL_GETTIME) * MICROSECONDS_PER_MILLISECOND);
   end Now_Us;

   Started_Us : Unsigned_64 := 0;

   --  Microseconds since Text started, for the serial markers.
   function Since (Moment : Unsigned_64) return String;

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Unsigned_64'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Since (Moment : Unsigned_64) return String is
     (if Moment >= Started_Us and then Moment /= 0 then Decimal (Moment - Started_Us) else "-");

   function In_Row (Code, First : Unsigned_64; Row : Letter_Row) return Text_Commands.Key is
     (if Code >= First and then Code - First <= Unsigned_64 (Row'Last) then Row (Natural (Code - First))
      else No_Key);

   function Key_Of (Code : Unsigned_64) return Text_Commands.Key is
      Letter : constant Text_Commands.Key :=
        (if In_Row (Code, TOP_ROW_FIRST, TOP_ROW) /= No_Key then In_Row (Code, TOP_ROW_FIRST, TOP_ROW)
         elsif In_Row (Code, HOME_ROW_FIRST, HOME_ROW) /= No_Key then In_Row (Code, HOME_ROW_FIRST, HOME_ROW)
         elsif In_Row (Code, BOTTOM_ROW_FIRST, BOTTOM_ROW) /= No_Key then In_Row (Code, BOTTOM_ROW_FIRST, BOTTOM_ROW)
         else In_Row (Code, DIGIT_ROW_FIRST, DIGIT_ROW));
   begin
      if Letter /= No_Key then
         return Letter;
      end if;
      return (if Code = KEY_ESCAPE then Escape elsif Code = KEY_ENTER then Enter
              elsif Code = KEY_BACKSPACE then Backspace elsif Code = KEY_TAB then Tab
              elsif Code = KEY_SPACE then Space elsif Code = KEY_UP then Up elsif Code = KEY_DOWN then Down
              elsif Code = KEY_LEFT then Left elsif Code = KEY_RIGHT then Right
              elsif Code = KEY_PAGE_UP then Page_Up elsif Code = KEY_PAGE_DOWN then Page_Down
              elsif Code = KEY_HOME then Home elsif Code = KEY_END then End_Key
              elsif Code = KEY_INSERT then Insert_Key elsif Code = KEY_DELETE then Delete
              elsif Code in KEY_F1 .. KEY_F10 then Text_Commands.Key'Val (Text_Commands.Key'Pos (F1) + Integer (Code - KEY_F1))
              elsif Code = KEY_F11 then F11 elsif Code = KEY_F12 then F12
              elsif Code = KEY_MINUS then Minus elsif Code = KEY_EQUALS then Equals
              elsif Code = KEY_LEFT_BRACKET then Left_Bracket elsif Code = KEY_RIGHT_BRACKET then Right_Bracket
              elsif Code = KEY_SEMICOLON then Semicolon elsif Code = KEY_QUOTE then Quote
              elsif Code = KEY_GRAVE then Grave elsif Code = KEY_BACKSLASH then Backslash
              elsif Code = KEY_COMMA then Comma elsif Code = KEY_PERIOD then Period
              elsif Code = KEY_SLASH then Slash
              else No_Key);
   end Key_Of;

   --  A code point as UTF-8.
   function UTF_8 (Code : Unsigned_64) return String is
      function B (Value : Unsigned_64) return Character is (Character'Val (Natural (Value and 16#FF#)));
   begin
      if Code < 16#80# then
         return [1 => B (Code)];
      elsif Code < 16#800# then
         return [B (16#C0# or Shift_Right (Code, 6)), B (16#80# or (Code and 16#3F#))];
      elsif Code < 16#1_0000# then
         return [B (16#E0# or Shift_Right (Code, 12)), B (16#80# or (Shift_Right (Code, 6) and 16#3F#)),
                 B (16#80# or (Code and 16#3F#))];
      elsif Code <= 16#10_FFFF# then
         return [B (16#F0# or Shift_Right (Code, 18)), B (16#80# or (Shift_Right (Code, 12) and 16#3F#)),
                 B (16#80# or (Shift_Right (Code, 6) and 16#3F#)), B (16#80# or (Code and 16#3F#))];
      end if;
      return "";
   end UTF_8;

   --  Visibility: serial markers (tests/headless) and log records follow
   --  what the shell shows; nothing in the shell depends on them.
   procedure Report is
      D : constant Text_Documents.Document_Access := Text_Shell.Active_Document (App);
      Message : constant String := Text_Shell.Status_Message (App);
   begin
      if Was_Loading and then not Text_Shell.Loading (App) and then D /= null then
         declare
            Times : constant Text_Host_Native.Load_Times := Text_Host_Native.Last_Load (Host);
         begin
            debugPrint ("text: opened " & Text_Documents.Name (D.all) & " lines="
                        & Decimal (Unsigned_64 (Text_Documents.Line_Total (D.all)))
                        & " bytes=" & Decimal (Unsigned_64 (Text_Documents.Length (D.all)))
                        & " t_us=" & Since (Now_Us) & " open_sent=" & Since (Times.Open_Sent)
                        & " open_reaped=" & Since (Times.Open_Reaped) & " read_sent=" & Since (Times.Read_Sent)
                        & " read_reaped=" & Since (Times.Read_Reaped) & ASCII.LF);
         end;
      end if;
      Was_Loading := Text_Shell.Loading (App);
      if Was_Saving and then not Text_Shell.Saving (App) and then D /= null then
         debugPrint ((if Text_Documents.Modified (D.all) then "text: save failed " else "text: saved ")
                     & Text_Documents.Name (D.all) & " bytes=" & Decimal (Unsigned_64 (Text_Documents.Length (D.all)))
                     & " reply=" & Decimal (Unsigned_64 (Text_Host_Native.Last_Reply (Host))) & ASCII.LF);
      end if;
      Was_Saving := Text_Shell.Saving (App);
      if Message'Length /= Last_Message_Length
        or else Message /= Last_Message (1 .. Natural'Min (Last_Message_Length, Last_Message'Length))
      then
         Last_Message_Length := Natural'Min (Message'Length, Last_Message'Length);
         Last_Message (1 .. Last_Message_Length) := Message (Message'First .. Message'First + Last_Message_Length - 1);
         if Message'Length > 0 then
            debugPrint ("text: status " & Message & ASCII.LF);
         end if;
      end if;
   end Report;

   procedure Collect (Dirty : in out Rect) is
      Area : Rect;
   begin
      Text_Shell.Set_Bounds (App, CuBit.UI.App.Full_Rect (Win));
      Text_Shell.Take_Damage (App, Area);
      if not Is_Empty (Area) then
         Dirty := Union_Rect (Dirty, Area);
      end if;
      Report;
   end Collect;

   procedure Pump_Shell (Dirty : in out Rect) is
      Start : constant Unsigned_64 := Now_Us;
      Changed : Boolean;
   begin
      Text_Shell.Set_Clock (App, Start);
      Text_Shell.Pump (App, PUMP_BUDGET, Busy, Changed);
      Last_Pump_Us := Now_Us - Start;
      Text_Host_Native.Arm_Wake (Host);
      Collect (Dirty);
   end Pump_Shell;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      Start : constant Unsigned_64 := Now_Us;
   begin
      Text_Shell.Render (App, CuBit.UI.App.Canvas (Win, Damage), CuBit.UI.App.Full_Rect (Win));
      Text_Shell.Note_Frame (App, Now_Us - Start, Last_Pump_Us);
      if First_Frame then
         First_Frame := False;
         debugPrint ("text: first frame presented render_us=" & Decimal (Now_Us - Start) & " t_us=" & Since (Now_Us)
                     & ASCII.LF);
      end if;
   end Render;

   procedure Handle_Event
     (Win : in out CuBit.UI.App.Window; Event : CuBit.UI.App.Input_Event; Dirty : in out Rect;
      Running : in out Boolean)
   is
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
   begin
      Modifiers := Current_Modifiers;
      Text_Shell.Set_Clock (App, Now_Us);
      if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Text_Shell.Set_Bounds (App, CuBit.UI.App.Full_Rect (Win));
         Text_Shell.Handle (App, (Kind => Text_Shell.Resize_Event, others => <>));
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then
         declare
            K : constant Text_Commands.Key := Key_Of (Event.payload0);
         begin
            --  Printable keys without Ctrl or Alt arrive as text events.
            if K /= No_Key and then (Control or else Alt or else K not in Key_A .. Key_9 | Space .. Grave) then
               Text_Shell.Handle (App, Text_Shell.Key (K, Control, Shift, Alt));
            end if;
         end;
      elsif Event.kind = CuBit.UI.Input.INPUT_TEXT then
         declare
            Code : constant Unsigned_64 := Event.payload0 and CODE_POINT_MASK;
         begin
            if not Control and then not Alt and then Code >= FIRST_PRINTABLE and then Code /= DELETE_CODE then
               Text_Shell.Handle (App, Text_Shell.Typed (UTF_8 (Code)));
            end if;
         end;
      elsif Event.kind in CuBit.UI.Input.INPUT_POINTER_DOWN | CuBit.UI.Input.INPUT_POINTER_UP
                          | CuBit.UI.Input.INPUT_POINTER_MOVE
      then
         Text_Shell.Handle
           (App, (Kind => Text_Shell.Pointer_Event,
                  Action => (if Event.kind = CuBit.UI.Input.INPUT_POINTER_DOWN then Text_Shell.Press
                             elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_UP then Text_Shell.Release
                             else Text_Shell.Move),
                  X => CuBit.UI.Input.Pointer_X (Event), Y => CuBit.UI.Input.Pointer_Y (Event), Time_Ms => Time_Ms,
                  Control => Control, Shift => Shift, Alt => Alt, others => <>));
         Buttons := Event.payload1 and PRIMARY_BUTTON;
      elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_WHEEL then
         Text_Shell.Handle (App, (Kind => Text_Shell.Wheel_Event, Steps => CuBit.UI.Input.Pointer_Wheel_Delta (Event),
                                  X => CuBit.UI.Input.Pointer_X (Event), Y => CuBit.UI.Input.Pointer_Y (Event),
                                  others => <>));
      end if;
      --  Input often submits requests (a save): move them now.
      Pump_Shell (Dirty);
      if Text_Shell.Quit_Requested (App) then
         Running := False;
      end if;
   end Handle_Event;

   --  Run's deadline, in GETTIME milliseconds: now while pump work waits,
   --  the shell's own deadline otherwise; zero for none.
   function Deadline return Unsigned_64 is
      Due_Us : constant Unsigned_64 := Text_Shell.Next_Deadline_Us (App);
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
      Pump_Shell (Dirty);
      if Text_Shell.Quit_Requested (App) then
         Running := False;
      end if;
   end Tick;

   procedure Completed
     (Win : in out CuBit.UI.App.Window; Receipt : CompletionEntry; Consumed : out Boolean; Dirty : in out Rect;
      Running : in out Boolean)
   is
      pragma Unreferenced (Win, Running);
   begin
      Text_Host_Native.Complete (Host, Receipt, Consumed);
      if Consumed then
         Pump_Shell (Dirty);
      end if;
   end Completed;

   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event, Next_Deadline => Deadline,
      On_Deadline => Tick, Activity_Wait => True, On_Completion => Completed);

   Queue_Ready : Boolean;
begin
   Started_Us := Now_Us;
   debugPrint ("text: starting" & ASCII.LF);
   if not CuBit.Startup_State.Ready (Startup.Verify) then
      debugPrint ("text: startup capabilities missing" & ASCII.LF);
      return;
   end if;
   CuBit.Logging.Announce ("text: started", Opened, Startup.Logstore);
   Text_Host_Native.Open (Host, Startup.Text_Filesystem, Queue_Ready);
   debugPrint ((if Queue_Ready then "text: filesystem queue ready" else "text: no filesystem queue")
               & " t_us=" & Since (Now_Us) & ASCII.LF);
   Text_Host_Native.Offer (Host, FIRST_DOCUMENT);
   Text_Shell.Initialize (App, Host'Unchecked_Access, Dark => False, Edit_Bytes => EDIT_BYTES);
   debugPrint ("text: shell ready t_us=" & Since (Now_Us) & ASCII.LF);
   Text_Shell.Set_Clock (App, Now_Us);
   Text_Shell.Open (App, FIRST_DOCUMENT);
   Was_Loading := Text_Shell.Loading (App);
   CuBit.UI.App.Open
     (Win, INITIAL_WIDTH, INITIAL_HEIGHT,
      CuBit.Desktop_Protocol.Feature_Bits
        ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
          CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
          CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
          others => False]),
      Opened, title => "Text", protected_frames => True, batched_input => False,
      minimum_width => MINIMUM_WIDTH, minimum_height => MINIMUM_HEIGHT);
   if Opened then
      debugPrint ("text: native window ready t_us=" & Since (Now_Us) & ASCII.LF);
      Text_Host_Native.Set_Surface (Host, CuBit.UI.App.Surface_ID (Win));
      Text_Shell.Set_Bounds (App, CuBit.UI.App.Full_Rect (Win));
      --  The document's open was answered while the window was being
      --  made: take the answer and ask for the first bytes before the
      --  first frame, not after it.
      declare
         Dirty : Rect := (others => 0);
      begin
         Pump_Shell (Dirty);
      end;
      Run (Win);
      CuBit.UI.App.Close (Win);
   end if;
   Text_Host_Native.Close (Host);
   debugPrint ("text: closed" & ASCII.LF);
end Main;

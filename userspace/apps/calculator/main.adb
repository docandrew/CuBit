------------------------------------------------------------------------------
--  CuBit Calculator (docs/calculator.md)
--  The platform glue only: the window (Calc_Shells) and the calculator core
--  (userspace/lib/calc) are the units the hosted harness runs
--  (tests/calculator). This translates the desktop's input into shell
--  events and waits once, in CuBit.UI.App.Run (Activity_Wait), for input.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol;
with CuBit.Logging;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Input;
with CuBit.UI.State;
with CuBit.Startup_State;
with Calc_Host_Native;
with Calc_Shell;
with Startup;

procedure Main is
   package Shell renames Calc_Shell;

   --  Set-1 scan codes the desktop delivers (payload0 of a key event).
   KEY_ESCAPE : constant Unsigned_64 := CuBit.UI.App.KEY_ESC;
   KEY_BACKSPACE : constant Unsigned_64 := 16#0E#;
   KEY_TAB : constant Unsigned_64 := 16#0F#;
   KEY_ENTER : constant Unsigned_64 := 16#1C#;
   KEY_ALT : constant Unsigned_64 := 16#38#;
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
   KEY_DELETE : constant Unsigned_64 := 16#53#;
   --  The keyboard's letter rows by set-1 code (menus' Alt+letter, Ctrl
   --  shortcuts: no text event comes with Ctrl or Alt).
   TOP_ROW_FIRST : constant Unsigned_64 := 16#10#;
   TOP_ROW : constant String := "qwertyuiop";
   HOME_ROW_FIRST : constant Unsigned_64 := 16#1E#;
   HOME_ROW : constant String := "asdfghjkl";
   BOTTOM_ROW_FIRST : constant Unsigned_64 := 16#2C#;
   BOTTOM_ROW : constant String := "zxcvbnm";
   --  The digit row (Ctrl+1, 2, 3 switch modes).
   DIGIT_ROW_FIRST : constant Unsigned_64 := 16#02#;
   DIGIT_ROW : constant String := "1234567890";
   CODE_POINT_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;
   FIRST_PRINTABLE : constant := 16#20#;
   LAST_PRINTABLE : constant := 16#7E#;

   --  The app's explicit limits (docs/calculator.md, "Limits"): results up
   --  to 32,768 limbs (about 315,000 digits) on Enter; the live preview of
   --  each keystroke within 2,048 limbs, so typing never waits.
   RESULT_LIMBS : constant := 32_768;
   PREVIEW_LIMBS : constant := 2_048;
   APPROXIMATE_DIGITS : constant := 50;
   HISTORY_ENTRIES : constant := 1_000;
   DECIMAL_PLACES : constant := 20;

   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   Host : aliased Calc_Host_Native.Host;
   App : Shell.Shell;
   Opened : Boolean;
   First_Frame : Boolean := True;
   Shown_Count : Natural := 0;
   Size : Shell.Window_Size;
   Shown_Mode : Shell.Calc.Mode_Member := Shell.Calc.Standard;

   function Decimal (Value : Natural) return String is
      Image : constant String := Natural'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Letter_Of (Code : Unsigned_64) return Character is
     (if Code >= TOP_ROW_FIRST and then Code - TOP_ROW_FIRST < TOP_ROW'Length
      then TOP_ROW (TOP_ROW'First + Natural (Code - TOP_ROW_FIRST))
      elsif Code >= HOME_ROW_FIRST and then Code - HOME_ROW_FIRST < HOME_ROW'Length
      then HOME_ROW (HOME_ROW'First + Natural (Code - HOME_ROW_FIRST))
      elsif Code >= BOTTOM_ROW_FIRST and then Code - BOTTOM_ROW_FIRST < BOTTOM_ROW'Length
      then BOTTOM_ROW (BOTTOM_ROW'First + Natural (Code - BOTTOM_ROW_FIRST))
      elsif Code >= DIGIT_ROW_FIRST and then Code - DIGIT_ROW_FIRST < DIGIT_ROW'Length
      then DIGIT_ROW (DIGIT_ROW'First + Natural (Code - DIGIT_ROW_FIRST))
      else ' ');

   function Key_Of (Code : Unsigned_64) return Shell.Key is
     (if Code = KEY_ENTER then Shell.Enter elsif Code = KEY_ESCAPE then Shell.Escape
      elsif Code = KEY_BACKSPACE then Shell.Backspace elsif Code = KEY_DELETE then Shell.Delete
      elsif Code = KEY_LEFT then Shell.Left elsif Code = KEY_RIGHT then Shell.Right
      elsif Code = KEY_UP then Shell.Up elsif Code = KEY_DOWN then Shell.Down
      elsif Code = KEY_HOME then Shell.Home elsif Code = KEY_END then Shell.End_Key
      elsif Code = KEY_PAGE_UP then Shell.Page_Up elsif Code = KEY_PAGE_DOWN then Shell.Page_Down
      elsif Code = KEY_TAB then Shell.Tab elsif Code = KEY_F1 then Shell.F1 elsif Code = KEY_F10 then Shell.F10
      else Shell.No_Key);

   --  Visibility: serial markers (tests/headless) follow what the window
   --  shows; nothing in the shell depends on them.
   procedure Report is
      Count : constant Natural := Shell.History_Count (App);
      Mode : constant Shell.Calc.Mode_Member := Shell.Options (App).Mode;
      use type Shell.Calc.Mode_Member;
   begin
      if Mode /= Shown_Mode then
         Shown_Mode := Mode;
         debugPrint ("calculator: mode "
                     & (case Mode is
                           when Shell.Calc.Standard => "STANDARD", when Shell.Calc.Scientific => "SCIENTIFIC",
                           when Shell.Calc.Programmer => "PROGRAMMER") & ASCII.LF);
      end if;
      if Count /= Shown_Count then
         Shown_Count := Count;
         if Count > 0 then
            debugPrint ("calculator: evaluated " & Shell.History_Source (App, 1) & " = "
                        & Shell.History_Result (App, 1) & " entries=" & Decimal (Count) & ASCII.LF);
         end if;
      end if;
   end Report;

   procedure Collect (Dirty : in out Rect) is
      Area : Rect;
   begin
      Shell.Pump (App);
      Shell.Take_Damage (App, Area);
      if not Is_Empty (Area) then
         Dirty := (if Is_Empty (Dirty) then Area else Union_Rect (Dirty, Area));
      end if;
      Report;
   end Collect;

   --  A mode switch: the window takes the new mode's size.
   procedure Follow_Mode (Win : in out CuBit.UI.App.Window) is
      Size : Shell.Window_Size;
      Wanted, Resized : Boolean;
   begin
      Shell.Take_Size_Request (App, Size, Wanted);
      if Wanted then
         CuBit.UI.App.Request_Size (Win, Size.Width, Size.Height, Size.Minimum_Width, Size.Minimum_Height, Resized);
         if not Resized then
            debugPrint ("calculator: the desktop refused the mode's size" & ASCII.LF);
         end if;
      end if;
   end Follow_Mode;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
   begin
      Shell.Render (App, CuBit.UI.App.Canvas (Win, Damage));
      if First_Frame then
         First_Frame := False;
         debugPrint ("calculator: first frame presented" & ASCII.LF);
      end if;
   end Render;

   procedure Handle_Event
     (Win : in out CuBit.UI.App.Window; Event : CuBit.UI.App.Input_Event; Dirty : in out Rect;
      Running : in out Boolean)
   is
      Modifiers : constant Unsigned_64 :=
        (if Event.kind in CuBit.UI.Input.INPUT_KEY_DOWN | CuBit.UI.Input.INPUT_KEY_UP
         then CuBit.UI.Input.Key_Modifiers (Event)
         elsif Event.kind in CuBit.UI.Input.INPUT_POINTER_DOWN | CuBit.UI.Input.INPUT_POINTER_UP
                             | CuBit.UI.Input.INPUT_POINTER_MOVE
         then CuBit.UI.Input.Pointer_Modifiers (Event)
         else 0);
      Shift : constant Boolean := (Modifiers and CuBit.UI.App.KEYMOD_SHIFT) /= 0;
      Control : constant Boolean := (Modifiers and CuBit.UI.App.KEYMOD_CTRL) /= 0;
      Alt : constant Boolean := (Modifiers and CuBit.UI.App.KEYMOD_ALT) /= 0;
   begin
      if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Shell.Set_Bounds (App, CuBit.UI.App.Full_Rect (Win));
         Shell.Handle (App, (Kind => Shell.Resize_Event, others => <>));
      elsif Event.kind in CuBit.UI.Input.INPUT_KEY_DOWN | CuBit.UI.Input.INPUT_KEY_UP
        and then Event.payload0 = KEY_ALT
      then
         Shell.Handle (App, (Kind => Shell.Modifier_Event, Alt => Alt, others => <>));
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then
         declare
            K : constant Shell.Key := Key_Of (Event.payload0);
            Letter : constant Character := Letter_Of (Event.payload0);
            use type Shell.Key;
         begin
            --  Printable keys without Ctrl or Alt arrive as text events.
            if K /= Shell.No_Key or else ((Control or else Alt) and then Letter /= ' ') then
               Shell.Handle (App, (Kind => Shell.Key_Event, Pressed => K, Typed => Letter,
                                   Control => Control, Shift => Shift, Alt => Alt, others => <>));
            end if;
         end;
      elsif Event.kind = CuBit.UI.Input.INPUT_TEXT then
         declare
            Code : constant Unsigned_64 := Event.payload0 and CODE_POINT_MASK;
         begin
            if not Control and then not Alt and then Code in FIRST_PRINTABLE .. LAST_PRINTABLE then
               Shell.Handle (App, Shell.Text (Character'Val (Natural (Code))));
            end if;
         end;
      elsif Event.kind in CuBit.UI.Input.INPUT_POINTER_DOWN | CuBit.UI.Input.INPUT_POINTER_UP
                          | CuBit.UI.Input.INPUT_POINTER_MOVE
      then
         Shell.Handle
           (App, (Kind => Shell.Pointer_Event,
                  Action => (if Event.kind = CuBit.UI.Input.INPUT_POINTER_DOWN then Shell.Press
                             elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_UP then Shell.Release
                             else Shell.Move),
                  X => CuBit.UI.Input.Pointer_X (Event), Y => CuBit.UI.Input.Pointer_Y (Event),
                  Time_Ms => syscall (SYSCALL_GETTIME), Control => Control, Shift => Shift, Alt => Alt,
                  others => <>));
      elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_WHEEL then
         Shell.Handle (App, (Kind => Shell.Wheel_Event, Steps => CuBit.UI.Input.Pointer_Wheel_Delta (Event),
                             X => CuBit.UI.Input.Pointer_X (Event), Y => CuBit.UI.Input.Pointer_Y (Event),
                             others => <>));
      end if;
      Collect (Dirty);
      Follow_Mode (Win);
      if Shell.Quit_Requested (App) then
         Running := False;
      end if;
   end Handle_Event;

   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event, Activity_Wait => True);
begin
   debugPrint ("calculator: starting" & ASCII.LF);
   if not CuBit.Startup_State.Ready (Startup.Verify) then
      debugPrint ("calculator: startup capabilities missing" & ASCII.LF);
      return;
   end if;
   CuBit.Logging.Announce ("calculator: started", Opened, Startup.Logstore);
   Shell.Initialize
     (App, Host'Unchecked_Access,
      (Result_Budget => (Budget => RESULT_LIMBS, Precision => APPROXIMATE_DIGITS),
       Preview_Budget => (Budget => PREVIEW_LIMBS, Precision => APPROXIMATE_DIGITS),
       History_Capacity => HISTORY_ENTRIES, Places => DECIMAL_PLACES));
   Size := Shell.Size_Of (Shell.Options (App).Mode);
   --  The window opens at the remembered mode's designed size (a compact
   --  tool, not the desktop's default share of the screen).
   CuBit.UI.App.Open
     (Win, Size.Width, Size.Height,
      CuBit.Desktop_Protocol.Feature_Bits
        ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
          CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
          CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
          others => False]),
      Opened, title => "Calculator", protected_frames => True, batched_input => False,
      minimum_width => Size.Minimum_Width, minimum_height => Size.Minimum_Height, preferred_size => True);
   if Opened then
      Calc_Host_Native.Set_Surface (Host, CuBit.UI.App.Surface_ID (Win));
      Shell.Set_Bounds (App, CuBit.UI.App.Full_Rect (Win));
      debugPrint ("calculator: window ready" & ASCII.LF);
      Run (Win);
      CuBit.UI.App.Close (Win);
   end if;
   debugPrint ("calculator: closed" & ASCII.LF);
end Main;

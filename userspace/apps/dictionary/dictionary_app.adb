with CuBit.Desktop_Protocol;
with CuBit.Logging;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Startup_State;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Input;
with CuBit.UI.State;
with Lexicon_Client;
with Startup;

package body Dictionary_App is
   use Interfaces;
   use type Lexicon_Client.Ask_Status;
   use type Views.Key_Name;

   --  Set-1 scan codes (CuBit.UI.App names the ones it uses itself).
   KEY_BACKSPACE : constant Unsigned_64 := 16#0E#;
   KEY_TAB       : constant Unsigned_64 := 16#0F#;
   KEY_ENTER     : constant Unsigned_64 := 16#1C#;
   KEY_HOME      : constant Unsigned_64 := 16#47#;
   KEY_UP        : constant Unsigned_64 := 16#48#;
   KEY_PAGE_UP   : constant Unsigned_64 := 16#49#;
   KEY_LEFT      : constant Unsigned_64 := 16#4B#;
   KEY_RIGHT     : constant Unsigned_64 := 16#4D#;
   KEY_END       : constant Unsigned_64 := 16#4F#;
   KEY_DOWN      : constant Unsigned_64 := 16#50#;
   KEY_PAGE_DOWN : constant Unsigned_64 := 16#51#;
   KEY_DELETE    : constant Unsigned_64 := 16#53#;
   WINDOW_WIDTH  : constant := 900;
   WINDOW_HEIGHT : constant := 660;
   MINIMUM_WIDTH : constant := 520;
   MINIMUM_HEIGHT : constant := 320;

   procedure Ask
     (Op : Lexicon_Answers.Operation; Text : String; Resume : Unsigned_32;
      Suggestions : Lexicon_Answers.Suggestion_Count; Into : out Lexicon_Wire.Answer_Bytes;
      Used : out Natural; Available : out Boolean)
   is
      Page : Lexicon_Channel.Page;
      Status : Lexicon_Client.Ask_Status;
   begin
      Lexicon_Client.Ask (Startup.Lexicon, Op, Text, Resume, Suggestions, Page, Used, Status);
      Available := Status = Lexicon_Client.Answered;
      Into := [others => 0];
      if Available then
         Into (Into'First .. Into'First + Used - 1) := Page (0 .. Used - 1);
      else
         Used := 0;
      end if;
   end Ask;

   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Map : CuBit.UI.Controls.Control_Map;
   View : Views.View_State;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
   begin
      Views.Render (View, CuBit.UI.App.Canvas (Win, Damage), CuBit.UI.App.Full_Rect (Win), UI, Map);
   end Render;

   procedure Handle_Event
     (Win : in out CuBit.UI.App.Window; Event : CuBit.UI.App.Input_Event; Dirty : in out Rect;
      Running : in out Boolean)
   is
      Redraw : Boolean := False;
      Shift : constant Boolean := (Event.payload1 and CuBit.UI.App.KEYMOD_SHIFT) /= 0;
      Control : constant Boolean := (Event.payload1 and CuBit.UI.App.KEYMOD_CTRL) /= 0;
      Alt : constant Boolean := (Event.payload1 and CuBit.UI.App.KEYMOD_ALT) /= 0;
      function Key_Of (Code : Unsigned_64) return Views.Key_Name is
        (if Code = CuBit.UI.App.KEY_ESC then Views.Escape
         elsif Code = KEY_ENTER then Views.Enter
         elsif Code = KEY_BACKSPACE then Views.Backspace
         elsif Code = KEY_TAB then Views.Tab
         elsif Code = KEY_LEFT then Views.Left
         elsif Code = KEY_RIGHT then Views.Right
         elsif Code = KEY_DELETE then Views.Delete
         elsif Code = KEY_UP then Views.Up
         elsif Code = KEY_DOWN then Views.Down
         elsif Code = KEY_PAGE_UP then Views.Page_Up
         elsif Code = KEY_PAGE_DOWN then Views.Page_Down
         elsif Code = KEY_HOME then Views.Home
         elsif Code = KEY_END then Views.End_Key
         else Views.No_Key);
      procedure Send (Item : Views.Event) is
      begin
         Views.Handle (View, Item, Map, Redraw);
      end Send;
   begin
      if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Send ((Kind => Views.Resize, X => CuBit.UI.App.Width (Win), Y => CuBit.UI.App.Height (Win), others => <>));
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then
         if Key_Of (Event.payload0) /= Views.No_Key then
            Send ((Kind => Views.Key_Event, Key => Key_Of (Event.payload0), Shift => Shift, Control => Control,
                   Alt => Alt, others => <>));
         end if;
      elsif Event.kind = CuBit.UI.Input.INPUT_TEXT then
         if (Event.payload0 and 16#FFFF_FFFF#) in 32 .. 126 then
            Send ((Kind => Views.Text_Event, Character_Value => Character'Val (Event.payload0 and 16#FF#),
                   others => <>));
         end if;
      elsif Event.kind in CuBit.UI.Input.INPUT_POINTER_DOWN | CuBit.UI.Input.INPUT_POINTER_UP |
                          CuBit.UI.Input.INPUT_POINTER_MOVE
      then
         Send ((Kind => Views.Pointer_Event,
                Action => (if Event.kind = CuBit.UI.Input.INPUT_POINTER_DOWN then CuBit.UI.Controls.Pointer_Press
                           elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_UP then CuBit.UI.Controls.Pointer_Release
                           else CuBit.UI.Controls.Pointer_Move),
                X => CuBit.UI.Input.Pointer_X (Event), Y => CuBit.UI.Input.Pointer_Y (Event), others => <>));
      elsif Event.kind = CuBit.UI.Input.INPUT_POINTER_WHEEL then
         Send ((Kind => Views.Wheel, Steps => CuBit.UI.Input.Pointer_Wheel_Delta (Event), others => <>));
      end if;
      if Redraw then Dirty := CuBit.UI.App.Full_Rect (Win); end if;
   end Handle_Event;

   procedure Run_UI is new CuBit.UI.App.Run
     (UI, Map, Render => Render, Handle_Event => Handle_Event, Activity_Wait => True);

   procedure Run is
      Opened, Announced : Boolean;
      Name : constant String := "dictionary";
   begin
      debugPrint (Name & ": starting" & ASCII.LF);
      if not CuBit.Startup_State.Ready (Startup.Verify) then
         debugPrint (Name & ": startup capabilities missing" & ASCII.LF);
         return;
      end if;
      CuBit.Logging.Announce (Name & ": started", Announced, Startup.Logstore);
      Views.Initialize (View, Views.Dictionary_Mode);
      CuBit.UI.App.Open (Win, WINDOW_WIDTH, WINDOW_HEIGHT,
        CuBit.Desktop_Protocol.Feature_Bits
          ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
            CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
            CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
            others => False]),
        Opened, title => "Dictionary", protected_frames => True,
        minimum_width => MINIMUM_WIDTH, minimum_height => MINIMUM_HEIGHT);
      if Opened then
         Views.Handle (View, (Kind => Views.Resize, X => CuBit.UI.App.Width (Win),
                              Y => CuBit.UI.App.Height (Win), others => <>), Map, Opened);
         debugPrint (Name & ": window ready" & ASCII.LF);
         Run_UI (Win);
         CuBit.UI.App.Close (Win);
      end if;
   end Run;
end Dictionary_App;

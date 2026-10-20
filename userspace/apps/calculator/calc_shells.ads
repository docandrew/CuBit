with Interfaces;
with CuBit.UI;
with CuBit.UI.Menus;
with Calc_Engine;
with Calc_Format;

--  The Calculator's window (docs/calculator.md, "The app"): one host of the
--  calculator core (userspace/lib/calc). Keyboard first: type an
--  expression, Enter evaluates, Up and Down recall earlier ones; a button
--  pad for the pointer. Above the entry, the history: typed expressions
--  with their exact results. Live: the result of what is typed shows as
--  it is typed, from the same evaluation as Enter and as calc.evaluate.
--
--  Platform-free: the native main and the hosted harness translate input
--  into Events and render into their canvas; the clipboard is the host's.
generic
   type Host_Type (<>) is limited private;
   with procedure Copy (H : in out Host_Type; Text : String) is <>;
   with procedure Request_Paste (H : in out Host_Type) is <>;
   with procedure Poll_Paste (H : in out Host_Type; Into : out String; Length : out Natural; Ready : out Boolean)
     is <>;
package Calc_Shells is
   use Interfaces;

   type Key is
     (No_Key, Enter, Escape, Backspace, Delete, Left, Right, Up, Down, Home, End_Key, Page_Up, Page_Down,
      Tab, F1, F10, Letter_A, Letter_C, Letter_D, Letter_L, Letter_Q, Letter_V, Letter_X);
   type Pointer_Action is (Press, Release, Move);
   --  Modifier_Event: Alt went down or up (Alt: held now), for the menu
   --  bar's keyboard cues.
   type Event_Kind is (Key_Event, Text_Event, Pointer_Event, Wheel_Event, Resize_Event, Modifier_Event);
   type Event is record
      Kind : Event_Kind := Key_Event;
      Pressed : Key := No_Key;
      --  Text: one printable character as typed.
      Typed : Character := ' ';
      Action : Pointer_Action := Move;
      X, Y : Natural := 0;
      Control, Shift, Alt : Boolean := False;
      --  Wheel: positive scrolls toward older entries.
      Steps : Integer := 0;
      --  Monotonic milliseconds (double clicks).
      Time_Ms : Unsigned_64 := 0;
   end record;
   function Key_Press (Item : Key; Control, Shift : Boolean := False) return Event is
     ((Kind => Key_Event, Pressed => Item, Control => Control, Shift => Shift, others => <>));
   function Text (C : Character) return Event is ((Kind => Text_Event, Typed => C, others => <>));

   --  The app's explicit limits: Enter evaluates with Result_Budget; the
   --  live preview of each keystroke with the smaller Preview_Budget, so
   --  typing never waits on a huge computation (Enter computes it).
   type Settings is record
      Result_Budget, Preview_Budget : Calc_Engine.Limits;
      --  Entries kept (the oldest goes beyond).
      History_Capacity : Positive;
      Places : Calc_Format.Places_Count;
   end record;

   type Shell is limited private;

   procedure Initialize (S : in out Shell; Host : not null access Host_Type; Config : Settings);
   procedure Handle (S : in out Shell; Item : Event);
   --  The window's area (at start and after every resize).
   procedure Set_Bounds (S : in out Shell; Bounds : CuBit.UI.Rect);
   --  What changed since the last frame (empty: nothing to paint).
   procedure Take_Damage (S : in out Shell; Area : out CuBit.UI.Rect);
   procedure Render (S : in out Shell; C : CuBit.UI.Canvas);
   --  A paste requested by Ctrl+V arrives here (call between events).
   procedure Pump (S : in out Shell);
   function Quit_Requested (S : Shell) return Boolean;

   --  What the window shows, for tests and visibility markers.
   function Entry_Text (S : Shell) return String;
   function Preview_Text (S : Shell) return String;
   function Preview_Failure (S : Shell) return Calc_Engine.Failure_Kind;
   function History_Count (S : Shell) return Natural;
   --  Entry Index (1 = newest): its expression and its shown result.
   function History_Source (S : Shell; Index : Positive) return String
     with Pre => Index <= History_Count (S);
   function History_Result (S : Shell; Index : Positive) return String
     with Pre => Index <= History_Count (S);
   function Status_Message (S : Shell) return String;
   function Places (S : Shell) return Calc_Format.Places_Count;
   function Menu_Bar (S : Shell) return CuBit.UI.Menus.Model;
   function Open_Menu (S : Shell) return Natural;
   function Help_Shown (S : Shell) return Boolean;
   --  Where a pad button is (tests click it): its label as on the pad.
   function Button_Area (S : Shell; Label : String) return CuBit.UI.Rect;
private
   type Shell_State;
   type Shell_State_Access is access Shell_State;
   type Shell is limited record
      State : Shell_State_Access;
   end record;
end Calc_Shells;

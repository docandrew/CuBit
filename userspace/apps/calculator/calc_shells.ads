with Interfaces;
with CuBit.UI;
with CuBit.UI.Menus;
with Calc_Engine;
with Calc_Format;
with CCL_Calc_Options;

--  The Calculator's window (docs/calculator.md, "The app"): one host of the
--  calculator core (userspace/lib/calc). Keyboard first: type an
--  expression, Enter evaluates, Up and Down recall earlier ones; a button
--  pad for the pointer. Above the entry, the history: typed expressions
--  with their results. Live: the result of what is typed shows as it is
--  typed, from the same evaluation as Enter and as calc.evaluate-with.
--
--  Three modes over the one core, each its own pad and window size:
--  Standard (exact arithmetic), Scientific (elementary functions, an angle
--  unit, approximations with their stated error bound) and Programmer
--  (integers in a word of 8 to 64 bits or unbounded, signed or unsigned,
--  shown in four bases with a bit field to toggle). The mode and its
--  options are a typed CCL value (CCL_Calc_Options.Choice, the
--  Calc_Options of calc.evaluate-with), remembered by the host.
--
--  Platform-free: the native main and the hosted harness translate input
--  into Events and render into their canvas; the clipboard and the
--  remembered options are the host's.
generic
   type Host_Type (<>) is limited private;
   with procedure Copy (H : in out Host_Type; Text : String) is <>;
   with procedure Request_Paste (H : in out Host_Type) is <>;
   with procedure Poll_Paste (H : in out Host_Type; Into : out String; Length : out Natural; Ready : out Boolean)
     is <>;
   --  The remembered mode and options (Found False: none yet, or not
   --  readable as a Calc_Options).
   with procedure Load_Options (H : in out Host_Type; Item : out CCL_Calc_Options.Choice; Found : out Boolean)
     is <>;
   with procedure Save_Options (H : in out Host_Type; Item : CCL_Calc_Options.Choice) is <>;
package Calc_Shells is
   use Interfaces;
   package Calc renames CCL_Calc_Options.Calc;

   type Key is
     (No_Key, Enter, Escape, Backspace, Delete, Left, Right, Up, Down, Home, End_Key, Page_Up, Page_Down,
      Tab, F1, F10);
   type Pointer_Action is (Press, Release, Move);
   --  Modifier_Event: Alt went down or up (Alt: held now), for the menu
   --  bar's keyboard cues.
   type Event_Kind is (Key_Event, Text_Event, Pointer_Event, Wheel_Event, Resize_Event, Modifier_Event);
   type Event is record
      Kind : Event_Kind := Key_Event;
      Pressed : Key := No_Key;
      --  Text: one printable character as typed. With Control or Alt: the
      --  key's character (shortcuts, menu letters).
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

   --  A mode's window: the client size it is designed for (it opens at
   --  it) and the smallest that still shows its pad and display.
   type Window_Size is record
      Width, Height, Minimum_Width, Minimum_Height : Positive;
   end record;
   function Size_Of (Mode : Calc.Mode_Member) return Window_Size;

   type Shell is limited private;

   --  Starts in the remembered mode (Load_Options), else Standard.
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
   --  The mode changed since the last call: the window should take Size
   --  (the host asks the desktop; the new bounds come as a resize).
   procedure Take_Size_Request (S : in out Shell; Size : out Window_Size; Wanted : out Boolean);

   function Options (S : Shell) return CCL_Calc_Options.Choice;

   --  What the window shows, for tests and visibility markers.
   function Entry_Text (S : Shell) return String;
   function Preview_Text (S : Shell) return String;
   --  The live result's error bound ("2e-21"; empty when exact).
   function Preview_Bound (S : Shell) return String;
   --  Programmer mode: the live result in Base (no prefix; empty when
   --  there is none), and whether it wrapped into the word.
   function Preview_In (S : Shell; Base : Calc.Base_Member) return String;
   function Preview_Wrapped (S : Shell) return Boolean;
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
   --  Programmer mode: the bit field has the keyboard (Tab).
   function Bit_Focus (S : Shell) return Boolean;
   --  Where things are (tests click them): a pad button by its label as
   --  typed ("7", "sqrt", "del", "AND" is "&"), bit Bit of the bit field,
   --  a strip chip by its caption ("DEG", "16", "Signed").
   function Button_Area (S : Shell; Label : String) return CuBit.UI.Rect;
   function Bit_Area (S : Shell; Bit : Natural) return CuBit.UI.Rect;
   function Chip_Area (S : Shell; Caption : String) return CuBit.UI.Rect;
   function Base_Row_Area (S : Shell; Base : Calc.Base_Member) return CuBit.UI.Rect;
private
   type Shell_State;
   type Shell_State_Access is access Shell_State;
   type Shell is limited record
      State : Shell_State_Access;
   end record;
end Calc_Shells;

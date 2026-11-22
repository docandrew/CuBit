------------------------------------------------------------------------------
--  CuBit Control Language desktop platform-link anchor
------------------------------------------------------------------------------

--  CCL desktop applications (the Workbench, the console) import a
--  deliberately tiny C-compatible window boundary so their bodies are
--  identical on Linux and CuBit. Calling Activate makes the selected platform
--  body's exported implementation part of the executable.
with System;
with Interfaces;
with CuBit.UI;
package CCL_Desktop_Platform is
   --  The event kinds of ccl_window_poll, shared with native_window.c.
   type Window_Event is
     (No_Event, Close_Request, Text_Input, Backspace, Enter, Left, Right,
      Home, End_Key, Delete, Select_All, Pointer_Down, Pointer_Drag,
      Pointer_Up, Double_Click, Triple_Click, Up, Down, Wheel_Up, Wheel_Down,
      Page_Up, Page_Down, Escape, Undo, Redo, Run_Source, Pointer_Hover,
      Wheel_Left, Wheel_Right, Match_Parenthesis, Select_To_Parenthesis,
      Add_Next_Occurrence, Open_Find, Find_Next, Open_Source, Save_Source,
      Tab, Toggle_REPL, Toggle_Watch, Complete_Operation, Toggle_Syntax,
      Copy_Selection, Cut_Selection, Paste_Clipboard, Open_Menu, Alt_Changed);
   for Window_Event use
     (No_Event => 0, Close_Request => 1, Text_Input => 2, Backspace => 3,
      Enter => 4, Left => 5, Right => 6, Home => 7, End_Key => 8, Delete => 9,
      Select_All => 10, Pointer_Down => 11, Pointer_Drag => 12,
      Pointer_Up => 13, Double_Click => 14, Triple_Click => 15, Up => 16,
      Down => 17, Wheel_Up => 18, Wheel_Down => 19, Page_Up => 20,
      Page_Down => 21, Escape => 22, Undo => 23, Redo => 24,
      Run_Source => 25, Pointer_Hover => 26, Wheel_Left => 27,
      Wheel_Right => 28, Match_Parenthesis => 29,
      Select_To_Parenthesis => 30, Add_Next_Occurrence => 31,
      Open_Find => 32, Find_Next => 33, Open_Source => 34, Save_Source => 35,
      Tab => 36, Toggle_REPL => 37, Toggle_Watch => 38,
      Complete_Operation => 39, Toggle_Syntax => 40,
      Copy_Selection => 41, Cut_Selection => 42, Paste_Clipboard => 43,
      Open_Menu => 44, Alt_Changed => 45);
   --  Open_Menu: F10 (code 0) or Alt+letter (code: the lower-case letter),
   --  for the application's menu bar (CuBit.UI.Menus). Alt_Changed: the
   --  Alt key went down (code 1) or up (code 0), for the menu bar's
   --  keyboard cues (CuBit.UI.Menus.Set_Alt_Held).
   --  The event a wire code names; unknown codes are No_Event.
   function Event_Of (Code : Interfaces.Integer_32) return Window_Event is
     (if Code in Window_Event'Enum_Rep (Window_Event'First) ..
                 Window_Event'Enum_Rep (Window_Event'Last)
      then Window_Event'Enum_Val (Code) else No_Event);
   --  The desktop clipboard (docs/clipboard.md). Call these only while
   --  handling the Copy_Selection, Cut_Selection or Paste_Clipboard event the
   --  user's own chord produced: Desktop refuses a copy or paste without one.
   --  A typed copy carries the value's type name, a text form for plain text
   --  fields and the literal that reads back as the value.
   function Copy_Text (Text : String) return Boolean;
   function Copy_Value (Type_Name, Text, Literal : String) return Boolean;
   --  Paste the typed form when there is one (Typed: Text holds the
   --  literal), else the text form. Text (Text'First .. Last).
   procedure Paste
     (Text : out String; Last : out Natural; Typed, Pasted : out Boolean);
   --  The wire's modifier bits.
   SHIFT_MODIFIER   : constant Interfaces.Unsigned_32 := 1;
   CONTROL_MODIFIER : constant Interfaces.Unsigned_32 := 2;
   ALT_MODIFIER     : constant Interfaces.Unsigned_32 := 4;
   type Live_Label_Event is (Started, Stopped, Sampled, Faulted);
   procedure Live_Label_Changed (Event : Live_Label_Event);
   --  Name prefixes this application's diagnostics ("ccl-console: ...");
   --  Title names its window.
   procedure Activate (Name, Title : String);
   --  Optional lifecycle diagnostic. Never logs submitted source or values.
   --  One REPL entry finished; Result is its transcript line (tests read it).
   procedure REPL_Completed (Result : String);

   --  The window's event loop (UI-039): the application handles input,
   --  does its timed work and draws; the platform polls, waits and
   --  presents. On CuBit that is the toolkit's loop (CuBit.UI.App.Run),
   --  as every app has; the Linux preview keeps its SDL loop.
   generic
      --  One input event (Window_Event and its wire fields, as Poll gave).
      with procedure Handle
        (Event : Window_Event; Code, Modifiers : Interfaces.Unsigned_32;
         X, Y : Interfaces.Integer_32);
      --  After input, after other completions and when Wake_At passes:
      --  the application's own work (streams, tasks, live cells).
      with procedure Work;
      --  The area to draw now (empty: nothing); taking it clears it.
      with function Take_Damage return CuBit.UI.Rect;
      --  Draw Area of the window into Canvas (clipped to Area).
      with procedure Draw (Canvas : in out CuBit.UI.Canvas; Area : CuBit.UI.Rect);
      --  When Work wants to run next: an absolute Ticks millisecond; 0:
      --  only on input. A time already past means: as soon as possible.
      with function Wake_At return Interfaces.Unsigned_64;
      --  False ends the loop (a close, or the window could not be kept).
      with function Running return Boolean;
   procedure Run_Window (Handle_Window : System.Address);
end CCL_Desktop_Platform;

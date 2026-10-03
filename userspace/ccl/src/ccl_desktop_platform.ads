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
   -- Native frames are acquired directly from the protected window owner.
   -- Hosted SDL retains its own CPU image; common rendering owns no image.
   procedure Begin_Frame
     (Canvas : in out CuBit.UI.Canvas; Changed : CuBit.UI.Rect;
      Repair : out CuBit.UI.Rect; Ready : out Boolean);
   function Submit_Frame
     (Handle : System.Address; Canvas : in out CuBit.UI.Canvas;
      Rendered : CuBit.UI.Rect) return Boolean;
   function Frame_Pending return Boolean;
   function Frame_Deadline (Application : Interfaces.Unsigned_64)
     return Interfaces.Unsigned_64;

   --  The event kinds of ccl_window_poll, shared with native_window.c.
   type Window_Event is
     (No_Event, Close_Request, Text_Input, Backspace, Enter, Left, Right,
      Home, End_Key, Delete, Select_All, Pointer_Down, Pointer_Drag,
      Pointer_Up, Double_Click, Triple_Click, Up, Down, Wheel_Up, Wheel_Down,
      Page_Up, Page_Down, Escape, Undo, Redo, Run_Source, Pointer_Hover,
      Wheel_Left, Wheel_Right, Match_Parenthesis, Select_To_Parenthesis,
      Add_Next_Occurrence, Open_Find, Find_Next, Open_Source, Save_Source,
      Tab, Toggle_REPL, Toggle_Watch, Complete_Operation, Toggle_Syntax);
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
      Complete_Operation => 39, Toggle_Syntax => 40);
   --  The event a wire code names; unknown codes are No_Event.
   function Event_Of (Code : Interfaces.Integer_32) return Window_Event is
     (if Code in Window_Event'Enum_Rep (Window_Event'First) ..
                 Window_Event'Enum_Rep (Window_Event'Last)
      then Window_Event'Enum_Val (Code) else No_Event);
   --  The wire's modifier bits.
   SHIFT_MODIFIER   : constant Interfaces.Unsigned_32 := 1;
   CONTROL_MODIFIER : constant Interfaces.Unsigned_32 := 2;
   ALT_MODIFIER     : constant Interfaces.Unsigned_32 := 4;
   type Live_Label_Event is (Started, Stopped, Sampled, Faulted);
   procedure Live_Label_Changed (Event : Live_Label_Event);
   -- Runnable input backlog: yield to peers without a timed sleep.
   procedure Yield_Input;
   procedure Finish_Input;
   --  Name prefixes this application's diagnostics ("ccl-console: ...");
   --  Title names its window.
   procedure Activate (Name, Title : String);
   --  Optional lifecycle diagnostic. Never logs submitted source or values.
   --  One REPL entry finished; Result is its transcript line (tests read it).
   procedure REPL_Completed (Result : String);
end CCL_Desktop_Platform;

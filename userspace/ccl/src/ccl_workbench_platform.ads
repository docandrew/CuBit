------------------------------------------------------------------------------
--  CuBit Control Language Workbench platform-link anchor
------------------------------------------------------------------------------

--  The Workbench imports a deliberately tiny C-compatible window boundary so
--  its editor/compiler/debugger body is identical on Linux and CuBit. Calling
--  Activate makes the selected platform body's exported implementation part
--  of the executable.
with System;
with Interfaces;
with CuBit.UI;
package CCL_Workbench_Platform is
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

   Open_Source_Event : constant := 34;
   Save_Source_Event : constant := 35;
   Tab_Event : constant := 36;
   Toggle_REPL_Event : constant := 37;
   Toggle_Watch_Event : constant := 38;
   Complete_Operation_Event : constant := 39;
   Toggle_Syntax_Event : constant := 40;
   type Live_Label_Event is (Started, Stopped, Sampled, Faulted);
   procedure Live_Label_Changed (Event : Live_Label_Event);
   -- Runnable input backlog: yield to peers without a timed sleep.
   procedure Yield_Input;
   procedure Finish_Input;
   procedure Activate;
   --  Optional lifecycle diagnostic. Never logs submitted source or values.
   --  One REPL entry finished; Result is its transcript line (tests read it).
   procedure REPL_Completed (Result : String);
end CCL_Workbench_Platform;

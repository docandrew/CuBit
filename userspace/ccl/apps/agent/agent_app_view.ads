with Interfaces;
with CuBit.UI;
with CuBit.UI.Editor;
with CCL_Agent_Bindings;

--  The Agent app's window (AGENT-002 step 3, docs/agent-harness.md "The
--  Agent app"): the transcript with tool cards, the authority and budget
--  panel, and one input line that is either the prompt or the API key's
--  masked entry. Presentation and input only: the app (CCL_Agent_App)
--  runs missions through the CCL host environment, and the key goes from
--  the masked field straight to secrets.svc (never to CCL, the transcript,
--  a log or a file).
package Agent_App_View is
   type Event_Kind is
     (No_Event, Text_Input, Backspace, Delete, Left, Right, Home, End_Key, Enter, Escape,
      Page_Up, Page_Down, Wheel_Up, Wheel_Down, Pointer_Down, Select_All, Set_Key);
   type View_Event is record
      Kind : Event_Kind := No_Event;
      Character_Value : Character := ' ';
      X, Y : Natural := 0;
   end record;

   --  What the app is asked to do after an event.
   type Action is (Nothing, Redraw, Ask, Store_Key, Cancel_Mission);

   type Input_Mode is (Prompt_Mode, Key_Mode);

   type View_State is limited private;

   procedure Initialize (State : out View_State);

   procedure Handle (State : in out View_State; Event : View_Event; Bounds : CuBit.UI.Rect; Result : out Action);
   --  Pasted text into the focused field (the key's field is Sensitive).
   procedure Insert_Pasted (State : in out View_State; Text : String; Changed : out Boolean);

   function Mode (State : View_State) return Input_Mode;
   procedure Set_Mode (State : in out View_State; Mode : Input_Mode);
   --  The prompt typed (Ask), then cleared by the app.
   function Prompt (State : View_State) return String;
   procedure Clear_Prompt (State : in out View_State);
   --  The key typed (Store_Key): read once by the app, then wiped.
   function Key_Text (State : View_State) return String;
   procedure Wipe_Key (State : in out View_State);

   --  Whether a secret is stored, and the note shown beside the field.
   procedure Set_Key_Status (State : in out View_State; Stored : Boolean; Note : String);
   procedure Set_Status (State : in out View_State; Text : String);

   --  The transcript (growable): text appended as the mission streams it.
   procedure Append_Transcript (State : in out View_State; Text : String);
   function Transcript_Length (State : View_State) return Natural;

   --  The mission being shown (Found False: none yet).
   procedure Set_Mission
     (State : in out View_State; Item : CCL_Agent_Bindings.Mission_View; Tools : String;
      Now_Ms : Interfaces.Unsigned_64);

   procedure Draw (State : View_State; Canvas : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect);

   --  For tests: how a transcript line is drawn.
   type Line_Style is (Plain_Line, Tool_Card, Tool_Result, Stop_Line, Compaction_Line);
   function Style_Of (Line : String) return Line_Style;
private
   type Text_Access is access String;

   MAXIMUM_NOTE : constant := 160;
   MAXIMUM_TOOLS : constant := 256;
   subtype Note_Length is Natural range 0 .. MAXIMUM_NOTE;

   type View_State is limited record
      Mode : Input_Mode := Prompt_Mode;
      Prompt_Field : CuBit.UI.Editor.Edit_State;
      Key_Field : CuBit.UI.Editor.Edit_State;
      Transcript : Text_Access := null;
      Transcript_Length : Natural := 0;
      --  Rows scrolled up from the newest (0: follow the newest).
      Scroll_Rows : Natural := 0;
      Key_Stored : Boolean := False;
      Key_Note : String (1 .. MAXIMUM_NOTE) := [others => ' '];
      Key_Note_Length : Note_Length := 0;
      Status : String (1 .. MAXIMUM_NOTE) := [others => ' '];
      Status_Length : Note_Length := 0;
      Mission : CCL_Agent_Bindings.Mission_View;
      Tools : String (1 .. MAXIMUM_TOOLS) := [others => ' '];
      Tools_Length : Natural range 0 .. MAXIMUM_TOOLS := 0;
      Elapsed_Ms : Interfaces.Unsigned_64 := 0;
   end record;
end Agent_App_View;

with Interfaces;
with CCL.Catalog;
with CCL.Catalog.Completion;
with CCL.Completions;
with CCL.Language;
with CCL.Sessions;
with CCL.Interfaces.Console;
with CCL.Language.Views;
with CCL.Streams;
with CuBit.UI;
with CuBit.UI.Editor;

--  The CCL console: a full-window REPL over CCL.Sessions. Platform-free, so
--  the native app, the Linux preview and tests drive the same state with the
--  same events and draw it onto any canvas. Evaluation, the transcript and
--  the session environment belong to CCL.Sessions; this package owns only
--  presentation: highlighting, the multi-line editor, completion, scrolling,
--  hover and the clickable regions of the last frame.
package CCL_Console_View is
   type Event_Kind is
     (No_Event, Text_Input, Backspace, Delete, Left, Right, Home, End_Key,
      Up, Down, Page_Up, Page_Down, Enter, Run, Tab, Complete, Escape,
      Select_All, Pointer_Move, Pointer_Down, Pointer_Drag, Pointer_Up,
      Wheel_Up, Wheel_Down,
      --  Lisp <-> BASIC: the whole transcript and the input switch notation.
      Toggle_Notation);
   type View_Event is record
      Kind : Event_Kind := No_Event;
      Character_Value : Character := ' ';
      X, Y : Natural := 0;
      Shift, Control : Boolean := False;
   end record;

   type View_State is limited private;
   procedure Initialize
     (State : out View_State; Catalog : CCL.Catalog.Interface_Catalog);

   --  Execute evaluates one entry (with the front end's grants and commands);
   --  Now_Ms times it. Redraw is set when the event changed what is shown.
   generic
      with procedure Execute
        (Item : in out CCL.Sessions.Session; Source : String;
         Fuel : CCL.Sessions.Fuel_Budget;
         Outcome : out CCL.Language.Interpretation_Result);
      with function Now_Ms return Interfaces.Unsigned_64;
   procedure Handle
     (State : in out View_State; Event : View_Event;
      Submitted, Redraw : out Boolean);

   --  Live cells. ":watch N" makes the newest entry re-run every N seconds
   --  (1 when omitted) and update in place; ":unwatch" stops every live
   --  cell, as does a click on a cell's LIVE mark. Only a plain expression
   --  is re-run (CCL.Sessions.Reevaluate_With_Values); the session's
   --  environment and history do not change.
   MAXIMUM_LIVE_CELLS : constant := 16;
   generic
      with procedure Reevaluate
        (Item : in out CCL.Sessions.Session; Index : CCL.Sessions.History_Index;
         Fuel : CCL.Sessions.Fuel_Budget;
         Outcome : out CCL.Language.Interpretation_Result; Reevaluated : out Boolean);
      with function Now_Ms return Interfaces.Unsigned_64;
   procedure Refresh (State : in out View_State; Redraw : out Boolean);
   --  Run Source (CCL source) as an entry of its own, as if typed, and make
   --  it live, re-running whenever stream elements arrive or a task
   --  completes (no period): the card a front end adds for each outlet and
   --  the outcome of a program an entry started
   --  (docs/ccl-launch-parameters.md, "Every outlet gets a card").
   generic
      with procedure Execute
        (Item : in out CCL.Sessions.Session; Source : String;
         Fuel : CCL.Sessions.Fuel_Budget;
         Outcome : out CCL.Language.Interpretation_Result);
      with function Now_Ms return Interfaces.Unsigned_64;
   procedure Follow (State : in out View_State; Source : String);

   --  Let the front end act on the session directly (resuming an entry
   --  that waited for a task, CCL.Sessions.Resume_With_Values); the
   --  transcript is drawn again when Act changed it.
   generic
      with procedure Act (Item : in out CCL.Sessions.Session; Changed : out Boolean);
   procedure Update_Session (State : in out View_State; Redraw : out Boolean);

   --  The newest entry's source (for a front end that follows what it
   --  started).
   function Latest_Source (State : View_State) return String;

   --  When Refresh next has work (absolute milliseconds), or 0 for none.
   function Next_Deadline (State : View_State) return Interfaces.Unsigned_64;
   --  Elements arrived on the session's streams: every live cell runs again
   --  at the next Refresh (once, however many arrived: reruns coalesce to
   --  the frame, and a slow cell never queues runs).
   procedure Note_Arrival (State : in out View_State);

   --  The console as an object (console.*, interfaces/console.schema).
   --  Notation: how every cell's source and the input are written. Cells
   --  are converted, not re-run; what they computed does not change.
   function Notation (State : View_State) return CCL.Interfaces.Console.Notation;
   procedure Set_Notation (State : in out View_State; Value : CCL.Interfaces.Console.Notation);
   function Theme return CCL.Interfaces.Console.Theme;
   procedure Set_Theme (Value : CCL.Interfaces.Console.Theme);
   --  Cells, live cells and their runs, the newest and slowest cell's time
   --  (Streams is the host's to fill in).
   function Statistics (State : View_State) return CCL.Interfaces.Console.Statistics;
   --  Cell Index's source as the transcript shows it (after a Draw).
   function Cell_Source (State : View_State; Index : Positive) return String;
   --  Whether the session still binds the stream (CCL.Sessions.Holds_Stream).
   function Holds_Stream (State : View_State; Handle : CCL.Streams.Handle) return Boolean;
   --  How often the live cell at entry Index has re-run (0 if not live).
   function Live_Runs (State : View_State; Index : Positive) return Natural;

   procedure Draw
     (State : in out View_State; Canvas : CuBit.UI.Canvas;
      Bounds : CuBit.UI.Rect);

   --  The newest entry's result, as the transcript shows it ("" if none).
   function Latest_Result (State : View_State) return String;
   --  The current input, for tests.
   function Input_Text (State : View_State) return String;
   --  The pointer shape for the last pointer position.
   function Pointer_Style (State : View_State) return CuBit.UI.Pointer_Cursor_Style;

   --  What the last frame put on screen, so tests and automation act on
   --  what a person would see and click.
   type Region_Kind is
     (Entry_Source, Entry_Result, Entry_Name, Result_Type, Example, Input, Suggestion,
      Table_Cell, Table_Header, Live_Mark);
   --  The Index-th region of a kind in the last frame (empty if none):
   --  entries count from the oldest shown, suggestions from the top.
   function Region
     (State : View_State; Kind : Region_Kind; Index : Positive) return CuBit.UI.Rect;
   --  The completion popup's suggestions ("" past the last).
   function Suggestions (State : View_State) return Natural;
   function Suggestion_Name (State : View_State; Index : Positive) return String;
private
   use CuBit.UI;

   Maximum_Examples : constant := 6;
   subtype Example_Index is Positive range 1 .. Maximum_Examples;

   --  Timing and cost the session does not record, aligned with its history.
   type Entry_Meta is record
      Elapsed_Ms : Interfaces.Unsigned_64 := 0;
      Fuel_Used : CCL.Sessions.Fuel_Budget := 0;
      Live : Boolean := False;
      Interval_Ms : Interfaces.Unsigned_64 := 0;
      Due_Ms : Interfaces.Unsigned_64 := 0;
      Runs : Natural := 0;
   end record;
   type Meta_Array is array (CCL.Sessions.History_Index) of Entry_Meta;

   --  What a region of the last frame does under the pointer.
   type Hit_Kind is
     (No_Hit, Source_Hit, Result_Hit, Name_Hit, Badge_Hit, Example_Hit,
      Input_Hit, Completion_Hit, Cell_Hit, Header_Hit, Live_Hit);
   type Hit is record
      Kind : Hit_Kind := No_Hit;
      Area : Rect := (others => 0);
      Item : Natural := 0;          --  history entry, example or suggestion
      First, Last : Natural := 0;   --  Name_Hit: the name within the source;
                                    --  Cell_Hit: the cell within the literal
      Column : Natural := 0;        --  Cell_Hit, Header_Hit: the field
   end record;
   Maximum_Hits : constant := 160;
   subtype Hit_Count is Natural range 0 .. Maximum_Hits;
   type Hit_Array is array (1 .. Maximum_Hits) of Hit;

   --  Completion candidates: host operations, then the language's own words.
   type Candidate_Origin is (Host_Candidate, Builtin_Candidate, Form_Candidate);
   type Candidate is record
      Suggestion : CCL.Catalog.Completion.Suggestion;
      Origin : Candidate_Origin := Host_Candidate;
   end record;
   Maximum_Candidates : constant := 10;
   subtype Candidate_Count is Natural range 0 .. Maximum_Candidates;
   type Candidate_Array is array (1 .. Maximum_Candidates) of Candidate;

   --  A cell's source as the current notation writes it, converted once.
   type Shown_Source is record
      Valid : Boolean := False;
      Into : CCL.Interfaces.Console.Notation := CCL.Interfaces.Console.Lisp;
      Source : String (1 .. CCL.Language.MAX_SOURCE_LENGTH) := [others => ' '];
      Source_Length : Natural range 0 .. CCL.Language.MAX_SOURCE_LENGTH := 0;
      Shown : CCL.Language.Views.Text;
   end record;
   type Shown_Array is array (CCL.Sessions.History_Index) of Shown_Source;

   type View_State is limited record
      Session : CCL.Sessions.Session;
      Meta : Meta_Array := [others => <>];
      Notation : CCL.Interfaces.Console.Notation := CCL.Interfaces.Console.Lisp;
      Shown : Shown_Array;
      Input : CuBit.UI.Editor.Edit_State;
      Draft : CuBit.UI.Editor.Edit_State;
      Recalled : CCL.Sessions.History_Count := 0;
      Visible_Interfaces : CCL.Catalog.Interface_Count := 0;
      --  Pixels scrolled up from the newest entry; 0 follows the newest.
      Scroll : Natural := 0;
      Scroll_Limit : Natural := 0;
      Page : Positive := 1;
      Columns : Positive := 80;        --  input columns in the last frame
      Hits : Hit_Array;
      Hit_Total : Hit_Count := 0;
      Pointer_X, Pointer_Y : Natural := 0;
      Pointer_Known : Boolean := False;
      Hovered : Hit_Count := 0;
      Selecting : Boolean := False;
      --  Completion popup.
      Candidates : Candidate_Array;
      Candidate_Total : Candidate_Count := 0;
      Matches_Beyond : Boolean := False;
      Selected : Positive := 1;
      Popup_Open : Boolean := False;
      Prefix_Length : Natural := 0;
      --  Parameter help for the call around the caret.
      Signature : CCL.Catalog.Completion.Suggestion;
      Signature_Visible : Boolean := False;
      Signature_Origin : CCL.Completions.Origin := CCL.Completions.Host_Operation;
      --  A short message for the status line, until the next edit.
      Notice : String (1 .. 120) := [others => ' '];
      Notice_Length : Natural range 0 .. 120 := 0;
   end record;
end CCL_Console_View;

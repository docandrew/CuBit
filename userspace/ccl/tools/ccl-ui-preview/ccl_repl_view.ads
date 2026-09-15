with CCL.Catalog;
with CCL.Catalog.Completion;
with CCL.Sessions;
with CCL.Language;
with CuBit.UI;
with CuBit.UI.Editor;

--  Shared native/hosted presentation. All evaluation and transcript ownership
--  live in CCL.Sessions, not in the Workbench or this view.
package CCL_REPL_View is
   type Event_Kind is
     (No_Event, Text_Input, Backspace, Delete, Left, Right, Home, End_Key,
      Select_All, Complete, Accept_Completion, Submit, Previous, Next, Wheel_Up, Wheel_Down,
      Pointer_Down, Pointer_Drag, Pointer_Up);
   type View_Event is record
      Kind : Event_Kind := No_Event;
      Character_Value : Character := ' ';
      X, Y : Natural := 0;
      Shift, Control : Boolean := False;
   end record;
   type View_State is private;
   procedure Initialize
     (State : out View_State; Catalog : CCL.Catalog.Interface_Catalog);
   procedure Deactivate (State : in out View_State);
   procedure Handle
     (State : in out View_State; Event : View_Event; Bounds : CuBit.UI.Rect;
      Submitted : out Boolean);
   generic
      with procedure Execute
        (Item : in out CCL.Sessions.Session; Source : String;
         Fuel : CCL.Sessions.Fuel_Budget;
         Outcome : out CCL.Language.Interpretation_Result);
   procedure Handle_With_Executor
     (State : in out View_State; Event : View_Event; Bounds : CuBit.UI.Rect;
      Submitted : out Boolean);
   procedure Draw
     (State : in out View_State; Canvas : CuBit.UI.Canvas;
      Bounds : CuBit.UI.Rect; Colors : CuBit.UI.Theme);
private
   type Capture_Kind is (No_Capture, Input_Capture, Clear_Capture);
   type View_State is record
      Session : CCL.Sessions.Session;
      Input : CuBit.UI.Editor.Edit_State;
      Draft : CuBit.UI.Editor.Edit_State;
      Recalled : CCL.Sessions.History_Count := 0;
      First_Entry : Positive := 1;
      First_Character : Positive := 1;
      Follow_Latest : Boolean := True;
      Capture : Capture_Kind := No_Capture;
      Visible_Interfaces : CCL.Catalog.Interface_Count := 0;
      Completion_Hint : String (1 .. 200) := [others => ' '];
      Completion_Hint_Length : Natural range 0 .. 200 := 0;
      Suggested_Suffix : String (1 .. CCL.Catalog.MAX_NAME_LENGTH * 2 + 1) := [others => ' '];
      Suggested_Length : Natural range 0 .. CCL.Catalog.MAX_NAME_LENGTH * 2 + 1 := 0;
      Signature : CCL.Catalog.Completion.Suggestion;
      Signature_Visible : Boolean := False;
      Signature_Arguments : Boolean := False;
   end record;
end CCL_REPL_View;

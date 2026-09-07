with CCL.Catalog;
with CCL.Sessions;
with CuBit.UI;
with CuBit.UI.Editor;

--  Shared native/hosted presentation. All evaluation and transcript ownership
--  live in CCL.Sessions, not in the Workbench or this view.
package CCL_REPL_View is
   type Event_Kind is
     (No_Event, Text_Input, Backspace, Delete, Left, Right, Home, End_Key,
      Select_All, Submit, Previous, Next, Wheel_Up, Wheel_Down,
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
   end record;
end CCL_REPL_View;

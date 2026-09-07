with CuBit.File_Selection;
with CuBit.UI.Editor;
with CuBit.UI.State;

--  Reusable, application-modal selection UI. The caller supplies names and
--  performs I/O after Submit; this widget cannot enumerate or grant access.
package CuBit.UI.File_Dialogs is
   type Dialog_Mode is (Open_File, Save_New_File);
   type Dialog_Action is (No_Action, Submit, Cancelled);
   type Event_Kind is
     (No_Event, Text_Input, Backspace, Delete, Left, Right, Home, End_Key,
      Select_All, Up, Down, Page_Up, Page_Down, Enter, Escape, Tab,
      Pointer_Down, Pointer_Drag, Pointer_Up, Double_Click,
      Wheel_Up, Wheel_Down);
   type Dialog_Event is record
      Kind : Event_Kind := No_Event;
      Character_Value : Character := ' ';
      X, Y : Natural := 0;
      Shift, Control : Boolean := False;
   end record;
   type Dialog_State is private;
   procedure Show
     (State : out Dialog_State; Mode : Dialog_Mode;
      Files : CuBit.File_Selection.File_List; Location : String;
      Suggested_Name : String := "");
   function Is_Open (State : Dialog_State) return Boolean;
   function Mode (State : Dialog_State) return Dialog_Mode;
   function Filename (State : Dialog_State) return String;
   function Bounds (Width, Height : Natural) return Rect;
   procedure Close (State : in out Dialog_State);
   procedure Set_Error (State : in out Dialog_State; Message : String);
   procedure Handle
     (State : in out Dialog_State; Event : Dialog_Event;
      Width, Height : Natural; Action : out Dialog_Action);
   procedure Draw (C : Canvas; State : Dialog_State; Colors : Theme);
private
   type Focus_Target is (File_List, Name_Field, Accept_Button, Cancel_Button);
   type Press_Target is (No_Press, Accept_Press, Cancel_Press, Name_Press);
   type Dialog_State is record
      Visible : Boolean := False;
      Operation : Dialog_Mode := Open_File;
      Files : CuBit.File_Selection.File_List;
      Selected : CuBit.File_Selection.File_Count := 0;
      First_Row : Natural := 1;
      Name : CuBit.UI.Editor.Edit_State;
      Location_Text : String (1 .. 128) := [others => ' '];
      Location_Length : Natural range 0 .. 128 := 0;
      Error_Text : String (1 .. 128) := [others => ' '];
      Error_Length : Natural range 0 .. 128 := 0;
      Focus : Focus_Target := File_List;
      Press : Press_Target := No_Press;
      Scroll_State : CuBit.UI.State.UI_State;
   end record;
end CuBit.UI.File_Dialogs;

with CuBit.UI; with CuBit.UI.Controls; with CuBit.UI.State;
with CuBit.UI.Editor; with CuBit.UI.Combo_Boxes; with CuBit.UI.Input;
with Servo_Bookmark_Model;
package Servo_Bookmarks is
   type Dialog is private;
   type Outcome is (Unchanged, Repaint, Closed, Navigate);
   procedure Open (S : in out Dialog; URL, Title : String; Add_Page : Boolean);
   function Is_Open (S : Dialog) return Boolean;
   function Location (S : Dialog) return String;
   procedure Handle (S : in out Dialog; Input : CuBit.UI.Input.Input_Event; Result : out Outcome);
   procedure Draw (C : CuBit.UI.Canvas; S : in out Dialog; Colors : CuBit.UI.Theme);
private
   package Model renames Servo_Bookmark_Model;
   type Flags is array (1 .. Model.Capacity) of Boolean;
   type IDs is array (1 .. Model.Capacity) of Model.ID;
   type Dialog is record
      Opened, Folder, Failed, Confirm_Delete : Boolean := False;
      Editing : Model.ID := 0;
      Icon : Model.Icon_Pixels := [others => 0];
      Revision : Natural := 0;
      Name, URL : CuBit.UI.Editor.Edit_State;
      Page_URL, Page_Title : CuBit.UI.Editor.Edit_State;
      Focus : Positive range 1 .. 9 := 1;
      Expanded : Flags := [others => True];
      Row_IDs, Depths : IDs := [others => 0];
      Row_Count, Scroll : Natural := 0;
      Visible : Positive := 10;
      Parent_IDs : IDs := [others => 0];
      Parent : CuBit.UI.Combo_Boxes.Combo_State;
      Map : CuBit.UI.Controls.Control_Map;
      UI : CuBit.UI.State.UI_State;
      Captured : Natural := 0;
      Message : String (1 .. 100) := [others => ' '];
      Message_Last : Natural := 0;
   end record;
end Servo_Bookmarks;

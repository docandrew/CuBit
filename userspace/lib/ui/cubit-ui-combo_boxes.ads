-- Bounded, non-editable native combo box. Draw after surrounding content so
-- the popup is on top. Rebuild registrations after state or layout changes.
with CuBit.UI.Controls;
package CuBit.UI.Combo_Boxes is
   Max_Choices : constant := 64;
   Default_Height : constant Positive := 26;
   type Text is access constant String;
   type Choice is record
      Caption : Text := null;
      Enabled : Boolean := True;
   end record;
   type Choice_List is array (Positive range 1 .. Max_Choices) of Choice;
   type Model is record
      Choices : Choice_List;
      Count : Natural range 0 .. Max_Choices := 0;
   end record;
   type Combo_State is private;
   subtype ID_Base is Positive range 1 .. Natural'Last - Max_Choices - 1;
   -- Reserve Base .. Base+65: field, choices, popup shield.
   function Choice_ID (Base : ID_Base; Index : Positive)
      return Controls.Control_ID with Pre => Index <= Max_Choices;
   function Is_Combo_Control (Base : ID_Base; Target : Controls.Control_ID)
      return Boolean;
   function Selection (State : Combo_State) return Natural;
   function Is_Open (State : Combo_State) return Boolean;
   procedure Set_Selection (State : in out Combo_State; Definition : Model;
                            Index : Natural);
   procedure Dismiss (State : in out Combo_State);
   procedure Draw
     (C : Canvas; Map : in out Controls.Control_Map; State : Combo_State;
      Definition : Model; Base : ID_Base; Bounds : Rect; Colors : Theme;
      Focused : Boolean := False; Enabled : Boolean := True;
      Row_Height : Positive := 26; Visible_Rows : Positive := 8);
   type Key is (Toggle, Up, Down, Home, End_Key, Commit, Cancel, Tab_Key,
                Type_Character);
   -- Route focused input: Alt+Down/F4 -> Toggle, Enter/Space -> Commit,
   -- Escape -> Cancel. Closed arrows commit; open arrows only preview.
   -- Tab dismisses without consuming focus traversal. Letters cycle matches.
   procedure Handle_Key
     (State : in out Combo_State; Definition : Model; Event : Key;
      Changed, Handled : out Boolean; Letter : Character := ' ';
      Enabled : Boolean := True);
   -- Same retained routing contract as Menus: outside presses go here BEFORE
   -- App pointer dispatch and are consumed while open; other events follow
   -- Controls.Dispatch_Pointer/App.Apply_Pointer_Event. Target is current Hit.
   procedure Handle_Pointer
     (State : in out Combo_State; Definition : Model;
      Map : in out Controls.Control_Map; Base : ID_Base;
      Target : Controls.Control_ID; Action : Controls.Pointer_Action;
      Changed, Handled : out Boolean; Enabled : Boolean := True);
   -- A wheel step moves the popup highlight/visible page without committing.
   procedure Handle_Wheel
     (State : in out Combo_State; Definition : Model; Wheel_Delta : Integer;
      Handled : out Boolean; Enabled : Boolean := True);
private
   type Combo_State is record
      Selected, Highlighted : Natural range 0 .. Max_Choices := 0;
      Opened, Hot, Dragging : Boolean := False;
   end record;
end CuBit.UI.Combo_Boxes;

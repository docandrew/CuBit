with CuBit.Appearance;
with CuBit.UI;
with CuBit.Display_Layouts;

--  Desktop-owned appearance and display pages. No IPC handlers or file authority.
--  Uses shared toolkit drawing/hit semantics, separate from compositor state.
package Desktop_Settings is
   type Control is (None, Appearance_Tab, Displays_Tab, Light, Dark, Image, Cubie, Slate, Ocean,
                    Fill, Fit, Center, Apply, Revert, Make_Primary, Scale_Down, Scale_Up);
   type Page is (Appearance, Displays);
   type Save_Status is (Unchanged, Saved_In_Config, Session_Only, Rejected, Scale_Rejected);
   type Diagram_Transform is record
      X, Y : Integer := 0;
      Logical_X, Logical_Y : Integer := 0;
      Divisor : Positive := 1;
   end record;
   type State is record
      Current_Page : Page := Appearance;
      Pending : CuBit.Appearance.Preferences := CuBit.Appearance.Default;
      Applied : CuBit.Appearance.Preferences := CuBit.Appearance.Default;
      Hovered, Pressed : Control := None;
      Focused : Control := Light;
      Status : Save_Status := Unchanged;
      Pending_Layout, Applied_Layout, Drag_Original : CuBit.Display_Layouts.Layout;
      Pending_Primary, Applied_Primary : CuBit.Display_Layouts.Named_Display_ID := 1;
      Selected : CuBit.Display_Layouts.Viewport_Count := 0;
      Dragging : Boolean := False;
      Drag_X, Drag_Y : Integer := 0;
      Transform : Diagram_Transform;
      Layout_Status : Save_Status := Unchanged;
   end record;
   procedure Open (View : out State; Current : CuBit.Appearance.Preferences;
                   Layout : CuBit.Display_Layouts.Layout;
                   Primary : CuBit.Display_Layouts.Named_Display_ID);
   procedure Draw (View : State; C : CuBit.UI.Canvas; Window_Bounds : CuBit.UI.Rect);
   procedure Pointer
     (View : in out State; Bounds : CuBit.UI.Rect;
      Pointer : CuBit.UI.Pointer_State; Apply_Requested : out Boolean);
   procedure Key
     (View : in out State; Scan_Code : Natural; Backwards : Boolean;
      Apply_Requested : out Boolean);
end Desktop_Settings;

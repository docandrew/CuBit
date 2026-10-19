with CuBit.Appearance;
with CuBit.UI;
with CuBit.Display_Layouts;
with Time_Zones;

--  Desktop-owned appearance, display and date & time pages. No IPC handlers
--  or file authority. The time-zone list is the bundled IANA database
--  (Time_Zones), so Settings can only store a zone the clock service knows.
--  Uses shared toolkit drawing/hit semantics, separate from compositor state.
package Desktop_Settings is
   type Control is (None, Appearance_Tab, Displays_Tab, Date_Time_Tab, Monitor_Tab, Light, Dark, Image, Cubie, Slate, Ocean,
                    Fill, Fit, Center, Apply, Revert, Make_Primary, Scale_Down, Scale_Up, Scale_Auto,
                    Zone_List, Zone_Previous, Zone_Next);
   subtype Tab_Control is Control range Appearance_Tab .. Monitor_Tab;
   type Page is (Appearance, Displays, Date_Time, Monitor);
   --  The Monitor page (read-only, DISP-005): what the primary output's
   --  monitor says (EDID through display.svc's catalog) and each of its
   --  modes with the GPU's verdict. Desktop fills it when Settings opens.
   --  Rows is what the page can show; Total counts every mode.
   Monitor_Text_Length : constant := 96;
   type Monitor_Line is record
      Text : String (1 .. Monitor_Text_Length) := [others => ' '];
      Length : Natural range 0 .. Monitor_Text_Length := 0;
      Available : Boolean := False;
   end record;
   Monitor_Rows : constant := 14;
   type Monitor_Lines is array (1 .. Monitor_Rows) of Monitor_Line;
   type Monitor_Info is record
      Summary, Detail : Monitor_Line;
      Lines : Monitor_Lines;
      Shown : Natural range 0 .. Monitor_Rows := 0;
      Total : Natural := 0;
   end record;
   function Line_Of (Text : String; Available : Boolean := False) return Monitor_Line;
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
      --  The display scale follows each output's size (desktop.display.scale
      --  = Auto) until the user picks one with Scale_Down / Scale_Up.
      Pending_Auto, Applied_Auto : Boolean := True;
      --  clock.time-zone: the chosen zone, the one in Config, and the first
      --  row the zone list shows.
      Pending_Zone, Applied_Zone, Zone_Top : Time_Zones.Time_Zone := Time_Zones.Zone_UTC;
      Zone_Status : Save_Status := Unchanged;
      Monitor_View : Monitor_Info;
   end record;
   procedure Open (View : out State; Current : CuBit.Appearance.Preferences;
                   Layout : CuBit.Display_Layouts.Layout;
                   Primary : CuBit.Display_Layouts.Named_Display_ID;
                   Auto_Scale : Boolean;
                   Zone : Time_Zones.Time_Zone);
   procedure Set_Monitor (View : in out State; Info : Monitor_Info);
   -- Layout emits synchronous drawing operations. Every primitive is required:
   -- a physical-output renderer must not silently inherit a canvas pixel writer.
   -- Canvas carries logical bounds/clip; Render never dereferences its address.
   generic
      with procedure Fill_Rect
        (C : CuBit.UI.Canvas; R : CuBit.UI.Rect; Fill : CuBit.UI.Color);
      with procedure Stroke_Rect
        (C : CuBit.UI.Canvas; R : CuBit.UI.Rect; Light, Dark : CuBit.UI.Color);
      with procedure Fill_Vertical_Gradient
        (C : CuBit.UI.Canvas; R : CuBit.UI.Rect; TopColor, BottomColor : CuBit.UI.Color);
      with procedure Draw_UI_Text
        (C : CuBit.UI.Canvas; X, Y : Natural; Text : String; FG, BG : CuBit.UI.Color);
      with procedure Draw_Button
        (C : CuBit.UI.Canvas; R : CuBit.UI.Rect; Colors : CuBit.UI.Theme;
         Style : CuBit.UI.Button_Style; Label : String);
      with procedure Draw_Tab
        (C : CuBit.UI.Canvas; R : CuBit.UI.Rect; Colors : CuBit.UI.Theme;
         Selected, Hot, Active : Boolean; Label : String;
         Orientation : CuBit.UI.Tab_Orientation := CuBit.UI.Horizontal);
      with procedure Paint_Wallpaper
        (C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect; Style : CuBit.Appearance.Preferences);
   procedure Render (View : State; C : CuBit.UI.Canvas; Window_Bounds : CuBit.UI.Rect);

   procedure Draw (View : State; C : CuBit.UI.Canvas; Window_Bounds : CuBit.UI.Rect);
   procedure Pointer
     (View : in out State; Bounds : CuBit.UI.Rect;
      Pointer : CuBit.UI.Pointer_State; Apply_Requested : out Boolean);
   procedure Key
     (View : in out State; Scan_Code : Natural; Backwards : Boolean;
      Apply_Requested : out Boolean);
end Desktop_Settings;

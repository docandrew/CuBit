with CuBit.Appearance;
with CuBit.UI;
with CuBit.Display_Layouts;
with CuBit.Output_Modes;
with Time_Zones;

--  Desktop-owned appearance, display, date & time and monitor pages. No IPC
--  handlers or file authority. The time-zone list is the bundled IANA
--  database (Time_Zones), so Settings can only store a zone the clock
--  service knows. Uses shared toolkit drawing/hit semantics, separate from
--  compositor state.
package Desktop_Settings is
   type Control is (None, Appearance_Tab, Displays_Tab, Date_Time_Tab, Monitor_Tab, Light, Dark, Image, Cubie, Slate, Ocean,
                    Fill, Fit, Center, Apply, Revert, Make_Primary, Scale_Down, Scale_Up, Scale_Auto,
                    Zone_List, Zone_Previous, Zone_Next, Mode_List, Mode_Previous, Mode_Next,
                    Keep_Mode, Revert_Mode);
   subtype Tab_Control is Control range Appearance_Tab .. Monitor_Tab;
   type Page is (Appearance, Displays, Date_Time, Monitor);
   --  The Monitor page (DISP-005/016): what the primary output's monitor
   --  says (EDID through display.svc) and its modes with the GPU's verdict,
   --  sorted (CuBit.Output_Modes.Before) under an Auto row (the driver's
   --  best admitted mode). Admitted rows can be chosen and applied; refused
   --  rows stay visible with their reason. Desktop fills it when Settings
   --  opens and after every mode change, and runs the switch and its
   --  "Keep this display mode?" question (Desktop_Mode_Trial).
   Monitor_Text_Length : constant := 96;
   type Monitor_Line is record
      Text : String (1 .. Monitor_Text_Length) := [others => ' '];
      Length : Natural range 0 .. Monitor_Text_Length := 0;
      Available : Boolean := False;
   end record;
   function Line_Of (Text : String; Available : Boolean := False) return Monitor_Line;
   type Monitor_Modes is array (Positive range <>) of CuBit.Output_Modes.Listed_Mode;
   type Monitor_Mode_List is access Monitor_Modes;
   procedure Free (Modes : in out Monitor_Mode_List);
   --  A row of the mode list: Auto_Row, then the modes from 1.
   subtype Mode_Row is Natural;
   Auto_Row : constant Mode_Row := 0;
   type Monitor_Info is record
      Summary, Detail : Monitor_Line;
      --  Sorted; null when the output lists no modes.
      Modes : Monitor_Mode_List;
      --  The monitor has an EDID identity, so a choice can be saved for it.
      Identified : Boolean := False;
      --  The saved choice: Auto, or the row of the saved mode (Auto_Row
      --  also when the saved mode is no longer listed).
      Saved_Auto : Boolean := True;
      Saved_Row : Mode_Row := Auto_Row;
   end record;
   --  The outcome of the last Apply on the Monitor page.
   type Mode_Status is
     (Mode_Ready, Mode_Switching, Mode_Refused, Mode_Kept, Mode_Saved_Session_Only,
      Mode_Reverted, Mode_Timed_Out);
   --  The answer to "Keep this display mode?", taken by Desktop.
   type Trial_Response is (No_Response, Keep_Response, Revert_Response);
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
      --  The chosen row and the first row the mode list shows.
      Mode_Choice, Mode_Top : Mode_Row := Auto_Row;
      Monitor_Status : Mode_Status := Mode_Ready;
      --  A switch on trial: the question and its countdown (Desktop sets
      --  both), and the user's answer (Desktop takes it).
      Trial_Open : Boolean := False;
      Trial_Seconds : Natural := 0;
      Answer : Trial_Response := No_Response;
   end record;
   procedure Open (View : out State; Current : CuBit.Appearance.Preferences;
                   Layout : CuBit.Display_Layouts.Layout;
                   Primary : CuBit.Display_Layouts.Named_Display_ID;
                   Auto_Scale : Boolean;
                   Zone : Time_Zones.Time_Zone);
   --  Replace the Monitor page's contents (freeing the previous mode list)
   --  and choose the saved row.
   procedure Set_Monitor (View : in out State; Info : Monitor_Info);
   --  Choose a listed row (Auto or an admitted mode) and scroll it into view.
   procedure Choose_Mode (View : in out State; Row : Mode_Row);
   --  The chosen row's mode (Mode_Choice /= Auto_Row and listed).
   function Chosen_Mode (View : State) return CuBit.Output_Modes.Listed_Mode
     with Pre => View.Monitor_View.Modes /= null and then View.Mode_Choice in View.Monitor_View.Modes'Range;
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

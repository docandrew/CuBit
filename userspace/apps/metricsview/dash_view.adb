pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with CuBit.UI.Combo_Boxes;
with CuBit.UI.Controls;
with CuBit.UI.File_Dialogs;
with CuBit.UI.Menus;
with CuBit.UI_Metrics;
with Charts_Buckets;
with Charts_Columns;
with Charts_Format;
with Charts_Palette;
with Charts_Time_Axis;
with Charts_Value_Axis;
with Dash_Commands;
with Dash_Edit;
with Dash_Grid;
with Dash_Labels;
with Dash_Layers;
with Dash_Pixels;
with Dash_Sync;
with Dash_Tracks;

package body Dash_View is
   use CuBit.UI;
   use type Dash_Model.Panel_Kind;
   use type Dash_Tracks.Track_State;
   use type Dash_Feeds.Series_Kind;
   use type Charts_Columns.Origin_Kind;
   use type Charts_Time_Axis.Viewport;
   use type Dash_Model.Match_Kind;
   use type Dash_Model.Statistic;
   use type Dash_Tracks.Column_Ring_Access;
   use type Dash_Tracks.Bucket_Ring_Access;
   use type Dash_Feeds.Window_Stats;
   package Axis renames Charts_Time_Axis;
   package Values renames Charts_Value_Axis;
   package Columns renames Charts_Columns;
   package Palette renames Charts_Palette;
   package Format renames Charts_Format;
   package M renames CuBit.UI_Metrics;
   package CB renames CuBit.UI.Combo_Boxes;
   package Controls renames CuBit.UI.Controls;
   package Dialogs renames CuBit.UI.File_Dialogs;
   package Files renames CuBit.File_Selection;
   package Menus renames CuBit.UI.Menus;
   package Commands renames Dash_Commands;
   use type Commands.Command;
   use type Dialogs.Dialog_Action;
   use type Dialogs.Dialog_Mode;
   use type Dialogs.Event_Kind;
   use type CB.Text;
   use type CB.Combo_State;
   use type Controls.Pointer_Action;
   subtype Column_Number is Axis.Column_Number;

   ---------------------------------------------------------------------------
   --  Geometry (logical pixels)
   ---------------------------------------------------------------------------
   Toolbar_Height : constant := M.Control_Height + 2 * M.Space_M;
   Status_Height : constant := M.Compact_Row_Height + M.Space_S;
   Margin : constant := M.Space_L;
   Gap : constant := M.Space_M + M.Space_S;
   Padding : constant := M.Space_L;
   Header_Height : constant := M.Line_Height + M.Space_M;
   Left_Axis_Width : constant := 4 * M.Base_Em;
   Bottom_Axis_Height : constant := M.Line_Height + M.Space_XS;
   Heat_Label_Width : constant := 9 * M.Base_Em;
   Minimum_Row_Unit : constant := 2 * M.Base_Em + M.Space_M;
   Minimum_Plot : constant := 8;
   Tick_Spacing_Pixels : constant := 96;
   Legend_Swatch : constant := M.Space_L;
   Legend_Gap : constant := M.Space_L;
   Line_Thickness : constant := 2;
   Dot_Size : constant := 3;
   Dash_On : constant := 4;
   Dash_Period : constant := 8;
   Segment_Padding : constant := M.Space_M;
   Segment_Height : constant := M.Control_Height;
   Bar_Gap : constant := M.Space_S;
   Spark_Height : constant := 3 * M.Base_Em;
   Minimum_Spark : constant := M.Base_Em + M.Space_S;
   Minimum_Spark_Points : constant := 3;
   --  The mark over a sparkline value clipped at the lane's top.
   Clip_Mark : constant := 3;
   --  A short stat tile's sparkline lane, on the right: this share of it.
   Spark_Lane_Percent : constant := 38;
   Tooltip_Offset : constant := M.Space_L;
   Tooltip_Padding : constant := M.Tooltip_Padding;
   Tooltip_Row : constant := M.Line_Height;
   Tooltip_Swatch : constant := M.Space_M + M.Space_XS;
   Click_Slop : constant := 3;
   --  The editor: the selection's outline, its resize grip (bottom right),
   --  and the CCL overlay's inset from the grid.
   Selection_Stroke : constant := 2;
   Grip_Size : constant := M.Space_L;
   Source_Inset : constant := 2 * M.Space_L;
   Source_Indent : constant := 2;
   Edit_Label : constant String := "Edit";
   Done_Label : constant String := "Done";
   Status_Dot : constant := M.Space_M;
   --  The menu bar (as Text's and Calculator's).
   Menu_Bar_Height : constant := M.Menu_Bar_Height;
   Menu_Row : constant := M.Row_Height;
   --  Each panel's source button (its header's right end) and the picker
   --  it opens: a card of two fields under the button.
   Source_Button_Width : constant := M.Control_Height;
   Chevron_Width : constant := M.Space_M + M.Space_S;
   Picker_Width : constant := 30 * M.Base_Em;
   Picker_Padding : constant := M.Space_L;
   Picker_Label_Width : constant := 4 * M.Base_Em;
   Picker_Row_Gap : constant := M.Space_S;
   Picker_Rows : constant := 12;
   --  The control map's ranges: the menu bar, then the picker's fields.
   Menu_Base : constant Menus.ID_Base := 1;
   Series_Base : constant CB.ID_Base := 100;
   View_Base : constant CB.ID_Base := 200;
   --  Two presses this close in time and place are a double click (the
   --  file dialog's list opens on one).
   Double_Click_Us : constant := 400_000;
   Double_Click_Slop : constant := 4;
   --  A status-bar notice (opened, saved as, why not) shows this long.
   Notice_Lifetime_Us : constant := 6_000_000;
   Maximum_Notice : constant := 240;
   --  The picker reads the store's catalog of series again this often
   --  while it is open.
   Catalog_Refresh_Us : constant := 2_000_000;
   Dashboard_Kind : constant String := "Dashboard";
   Folder_Label : constant String := "Documents/Dashboards";

   --  Tints over the card (percent of the mark's colour).
   Band_Tint : constant := 16;
   Area_Tint : constant := 22;
   Grid_Tint_Dark : constant := 16;
   Grid_Tint_Light : constant := 26;
   Selection_Tint : constant := 22;
   --  Histogram bars at or over the budget: the series hue toward warning.
   Late_Tint : constant := 70;
   Crosshair_Tint : constant := 45;
   Edge_Tint : constant := 55;

   --  Zoom: one wheel step multiplies the span by 5/4.
   Zoom_Numerator : constant := 5;
   Zoom_Denominator : constant := 4;
   Pan_Fraction : constant := 10;
   Microseconds_Per_Millisecond : constant := 1_000;

   type Preset_Index is range 1 .. 6;
   type Preset_Spans is array (Preset_Index) of Unsigned_64;
   Presets : constant Preset_Spans :=
     [Axis.Microseconds_Per_Minute, 5 * Axis.Microseconds_Per_Minute, 15 * Axis.Microseconds_Per_Minute,
      Axis.Microseconds_Per_Hour, 6 * Axis.Microseconds_Per_Hour, Axis.Microseconds_Per_Day];
   type Preset_Labels is array (Preset_Index) of String (1 .. 3);
   Preset_Names : constant Preset_Labels := [" 1m", " 5m", "15m", " 1h", " 6h", "24h"];
   Live_Label : constant String := "Live";

   ---------------------------------------------------------------------------
   --  State (one window per process)
   ---------------------------------------------------------------------------
   type Panel_Geometry is record
      Outer, Header, Body_Area, Plot, Left_Axis, Bottom_Axis : Rect;
   end record;
   type Geometry_List is array (Dash_Model.Panel_Index) of Panel_Geometry;
   type Number_List is array (Dash_Model.Panel_Index) of Unsigned_64;
   type Viewport_List is array (Dash_Model.Panel_Index) of Axis.Viewport;
   type Flag_List is array (Dash_Model.Panel_Index) of Boolean;

   --  Tracks by reference: an edit moves them between panels (the data
   --  drawn follows the panel), and a removed panel's tracks wait in
   --  Retired until the data thread is done with them.
   type Track_Access is access Dash_Tracks.Track;
   type Track_Array is array (Positive range <>) of Track_Access;
   type Track_Array_Access is access Track_Array;
   type Board_Access is access Dash_Model.Dashboard;

   --  A prefix series expands to at most this many tracks.
   Prefix_Tracks : constant := 8;

   Feed : Feed_Access;
   Board : Board_Access;
   Tracks : Track_Array_Access;
   Whole, Menu_Area, Toolbar, Status, Grid : Rect := (others => 0);
   Panels : Geometry_List := [others => (others => (others => 0))];
   Ports : Viewport_List := [others => (others => <>)];
   Has_Port : Flag_List := [others => False];
   Tops : Number_List := [others => 1];
   --  A heatmap's smallest value in view (its colour scale's bottom).
   Lows : Number_List := [others => 1];

   Span : Axis.Span := 5 * Axis.Microseconds_Per_Minute;
   Live_Mode : Boolean := True;
   End_Us : Unsigned_64 := 0;

   --  Pointer
   Hover_Panel : Natural := 0;
   Hover_X, Hover_Y : Natural := 0;
   Hover_Time : Unsigned_64 := 0;
   Hover_Valid : Boolean := False;
   Pressed : Boolean := False;
   Press_X, Press_Y : Natural := 0;
   Press_End : Unsigned_64 := 0;
   Press_Panel : Natural := 0;
   Dragged, Selecting : Boolean := False;
   Select_From, Select_To : Unsigned_64 := 0;
   Tooltip : Rect := (others => 0);
   --  The drill-down slice.
   Detail_Valid : Boolean := False;
   Detail_Panel : Natural := 0;
   Detail_From, Detail_To : Unsigned_64 := 0;

   Last_Refresh_Cost : Unsigned_64 := 0;
   Last_Refresh_Calls : Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   --  Colours
   ---------------------------------------------------------------------------
   type Colors is record
      Dark : Boolean := True;
      Page, Card, Edge, Grid_Line, Text, Muted, Axis_Text, Accent, Selection, Selection_Text : Color := 0;
      Bar, Rule : Color := 0;
      Series : Palette.Slot_Colors := Palette.Dark_Series;
      Ramp : Palette.Ramp := Palette.Dark_Ramp;
   end record;

   function Luma (C : Color) return Natural is
     ((Natural (Shift_Right (C, 16) and 16#FF#) * 3 + Natural (Shift_Right (C, 8) and 16#FF#) * 6
       + Natural (C and 16#FF#)) / 10);
   Dark_Below : constant := 128;

   function Current_Colors return Colors is
      T : constant Theme := Current_Theme;
      Dark : constant Boolean := Luma (T.field) < Dark_Below;
      R : Colors;
   begin
      R.Dark := Dark;
      R.Card := T.field;
      R.Page := (if Dark then Palette.Mix (T.darkShadow, T.shadow, 50) else Palette.Mix (T.panel, T.field, 40));
      R.Edge := Palette.Mix (T.edge, T.field, Edge_Tint);
      R.Grid_Line := Palette.Mix (T.edge, T.field, (if Dark then Grid_Tint_Dark else Grid_Tint_Light));
      R.Text := T.text;
      R.Muted := T.muted;
      R.Axis_Text := T.muted;
      R.Accent := T.accent;
      R.Selection := T.selection;
      R.Selection_Text := T.selectionText;
      R.Bar := T.titleBar;
      R.Rule := T.menuRule;
      R.Series := (if Dark then Palette.Dark_Series else Palette.Light_Series);
      R.Ramp := (if Dark then Palette.Dark_Ramp else Palette.Light_Ramp);
      return R;
   end Current_Colors;

   ---------------------------------------------------------------------------
   --  Small helpers
   ---------------------------------------------------------------------------
   function Intersects (A, B : Rect) return Boolean is
     (A.w > 0 and then B.w > 0 and then A.h > 0 and then B.h > 0 and then
      A.x < B.x + B.w and then B.x < A.x + A.w and then A.y < B.y + B.h and then B.y < A.y + A.h);

   function Add_Rect (A, B : Rect) return Rect is
     (if Is_Empty (A) then B elsif Is_Empty (B) then A else Union_Rect (A, B));

   --  The editor's CCL overlay is shown (over the grid, opaque).
   Source_Shown : Boolean := False;
   --  Which: the dashboard's CCL, or the keys (? or F1).
   type Overlay_Kind is (Source_Overlay, Keys_Overlay);
   Shown_Overlay : Overlay_Kind := Source_Overlay;
   --  Where the overlay is.
   function Source_Box return Rect is
     ((Grid.x + Natural'Min (Source_Inset, Grid.w / 4), Grid.y + Natural'Min (Source_Inset, Grid.h / 4),
       (if Grid.w > 2 * Source_Inset then Grid.w - 2 * Source_Inset else Grid.w),
       (if Grid.h > 2 * Source_Inset then Grid.h - 2 * Source_Inset else Grid.h)));

   --  What the shown overlay covers: the CCL's box, or the keys card
   --  (sized to its lines, centred in it).
   function Overlay_Box return Rect;
   --  The keys card's size, measured once a scale.
   Keys_W, Keys_H : Natural := 0;


   function Inside (Outer, R : Rect) return Boolean is
     (R.x >= Outer.x and then R.y >= Outer.y and then R.x + R.w <= Outer.x + Outer.w
      and then R.y + R.h <= Outer.y + Outer.h);


   ---------------------------------------------------------------------------
   --  Layers over the dashboard: the menu bar's open menu, a panel's source
   --  picker, the file dialog. Each is drawn into the retained frame over
   --  what is under it (clipped like everything else, so a change under a
   --  layer redraws both there); what changes wholly under one is not drawn
   --  until the layer goes, which redraws its place.
   ---------------------------------------------------------------------------
   --  The window's controls (hit testing for the menu bar and the picker's
   --  fields), registered whenever they change (Register); painting
   --  registers into a map of its own that nothing reads.
   Map, Paint_Map : Controls.Control_Map;
   Menu_State : Menus.Menu_State;
   Menu_Model : Menus.Model;
   Menu_Width : Positive := 1;
   --  A press outside an open menu or the picker closes it; its release is
   --  swallowed too (no click-through).
   Block_Release : Boolean := False;
   function Menu_Popup return Rect is
     (Menus.Popup_Bounds (Menu_State, Menu_Model, Menu_Area, Whole, Menu_Width, Menu_Row));

   --  The picker: its panel (by index), its two fields and their choices.
   Picker_On : Boolean := False;
   Picker_Target : Natural := 0;
   Picker_Focus : Picker_Field := Series_Field;
   Series_Combo, View_Combo : CB.Combo_State;
   Series_Model, View_Model : CB.Model;
   function Picker_Box return Rect;
   function Field_Box (Field : Picker_Field) return Rect;
   function Picker_List return Rect is
     (if not Picker_On then (others => 0)
      elsif CB.Is_Open (Series_Combo)
      then CB.Popup_Bounds (Series_Combo, Series_Model, Field_Box (Series_Field), Whole.y + Whole.h,
                            Visible_Rows => Picker_Rows)
      elsif CB.Is_Open (View_Combo)
      then CB.Popup_Bounds (View_Combo, View_Model, Field_Box (View_Field), Whole.y + Whole.h,
                            Visible_Rows => Picker_Rows)
      else (others => 0));
   function Picker_Cover return Rect is (if Picker_On then Add_Rect (Picker_Box, Picker_List) else (others => 0));

   Dialog : Dialogs.Dialog_State;
   function Dialog_Box return Rect is
     (if Dialogs.Is_Open (Dialog) then Dialogs.Bounds (Whole.x + Whole.w, Whole.y + Whole.h) else (others => 0));

   --  The store's series this app may read (the picker's choices), as
   --  the data thread last listed them: up to a combo box's choices.
   type Catalog_Entry is record
      Name : Dash_Model.Text;
   end record;
   subtype Catalog_Count is Natural range 0 .. CB.Max_Choices;
   type Catalog_List is array (1 .. CB.Max_Choices) of Catalog_Entry;
   Catalog : Catalog_List;
   Catalog_Used : Catalog_Count := 0;
   --  Bumped at each listing (the picker's choices follow), and asked for
   --  by the UI thread (the picker opened): the data thread lists then.
   Catalog_Generation : Natural := 0;
   Catalog_Wanted : Boolean := True with Atomic;
   Next_Catalog_Us : Unsigned_64 := 0;

   --  The status bar's notice: what a file command did, or why not.
   Notice_Text : String (1 .. Maximum_Notice) := [others => ' '];
   Notice_Length : Natural range 0 .. Maximum_Notice := 0;
   Notice_Until_Us : Unsigned_64 := 0;

   --  Captions made at run time (a recent file, a series): on the heap,
   --  freed when the menu or the picker is made again.
   type Caption_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, Caption_Access);

   --  The dashboard's own file (a leaf in Dash_Model.Dashboards_Place).
   Current_Leaf : Files.File_Name;
   --  The folder as last listed, and the recent dashboards in it (newest
   --  first) as the File menu shows them.
   Folder : Folder_Listing;
   Recent_Leaves : array (Commands.Recent_Index) of Files.File_Name;
   Recent_Captions : array (Commands.Recent_Index) of Caption_Access := [others => null];
   Recent_Used : Commands.Recent_Count := 0;
   --  The data thread's work asked for (one of each kind at most: a newer
   --  request replaces an older one), and its answers, taken by the UI
   --  thread at its next refresh.
   Want_List : Boolean := False;
   Want_Open, Want_Save : Files.File_Name;
   --  A command waiting for the listing it needs (a new name must not be
   --  a file already there).
   Deferred : Commands.Command := Commands.No_Command;
   type Board_Access_Answer is access Dash_Model.Dashboard;
   List_Answer : Folder_Listing;
   List_Answered, Open_Answered, Open_Done, Save_Answered, Save_Done : Boolean := False;
   Open_Board : Board_Access_Answer;
   Open_Leaf, Save_Leaf : Files.File_Name;
   Answer_Reason : String (1 .. Maximum_Notice) := [others => ' '];
   Answer_Reason_Length : Natural range 0 .. Maximum_Notice := 0;
   Copy_Wanted : Boolean := False;
   --  A dashboard left (another opened over it) with edits not yet
   --  written: saved first, under its own name.
   Parting_Source : Caption_Access;
   Parting_Name : Files.File_Name;
   Saving_Parting : Boolean := False;
   --  The edit a save in flight holds, and the first edit of the
   --  dashboard shown (an answer for an older one changes nothing here).
   In_Flight_Generation : Natural := 0;
   Board_First_Generation : Natural := 0;
   --  The file dialog's pointer: a press held, and the last press (a
   --  second one soon after is a double click).
   Dialog_Down : Boolean := False;
   Last_Press_Us : Unsigned_64 := 0;
   Last_Press_X, Last_Press_Y : Natural := 0;

   --  The picker's choices: each series choice's source, each view
   --  choice's view, and whether the first choice keeps what the panel
   --  has (several series, a series the store does not list, a view
   --  outside the list).
   type Source_Table is array (1 .. CB.Max_Choices) of Dash_Choices.Source_Choice;
   Series_Sources : Source_Table;
   Series_Captions : array (1 .. CB.Max_Choices) of Caption_Access := [others => null];
   type View_Table is array (1 .. CB.Max_Choices) of Dash_Choices.View_Choice;
   View_Choices : View_Table;
   View_Captions : array (Dash_Choices.View_Choice) of Caption_Access := [others => null];
   Series_Keeps, View_Keeps : Boolean := False;
   Keep_View_Caption : aliased constant String := "As its CCL has it";
   Built_Generation : Natural := Natural'Last;

   procedure Rebuild_Menus;
   procedure Register;

   --  R lies wholly under one opaque layer (drawn over everything there).
   function Under_Layer (R : Rect) return Boolean is
     (not Is_Empty (R)
      and then ((Menus.Is_Open (Menu_State) and then Inside (Menu_Popup, R))
                or else (Picker_On and then (Inside (Picker_Box, R) or else Inside (Picker_List, R)))
                or else (Dialogs.Is_Open (Dialog) and then Inside (Dialog_Box, R))));
   --  R reaches under a layer.
   function Touches_Layer (R : Rect) return Boolean is
     (Intersects (R, Menu_Popup) or else Intersects (R, Picker_Cover) or else Intersects (R, Dialog_Box));

   --  R changed: the retained frame redraws exactly R (Dash_Layers), the
   --  toolkit's frame grows by it.
   --  Pixels marked since the last Render (Take_Copy_Stats), and its copy.
   Marked_Pixels, Last_Exact_Pixels, Last_Union_Pixels, Last_Copy_Us, Last_Paint_Us : Unsigned_64 := 0;

   procedure Mark_Exactly (Damage : in out Rect; R : Rect);
   procedure Mark_Exactly (Damage : in out Rect; R : Rect) is
   begin
      if not Is_Empty (R) then
         Marked_Pixels := Marked_Pixels + Unsigned_64 (R.w) * Unsigned_64 (R.h);
         Damage := Add_Rect (Damage, R);
         Dash_Layers.Invalidate (R);
      end if;
   end Mark_Exactly;

   --  While the CCL overlay is shown, what changes under it is not drawn
   --  (closing it redraws the window), and a change that reaches into it
   --  redraws it whole: its text is never cut at a damage edge.
   procedure Mark (Damage : in out Rect; R : Rect);
   procedure Mark (Damage : in out Rect; R : Rect) is
   begin
      if Under_Layer (R) then
         --  Not seen until the layer goes (which redraws its place).
         return;
      elsif Source_Shown and then Intersects (R, Overlay_Box) then
         if not Inside (Overlay_Box, R) then
            Mark_Exactly (Damage, Add_Rect (R, Overlay_Box));
         end if;
      else
         Mark_Exactly (Damage, R);
      end if;
   end Mark;

   procedure Text (C : Canvas; X, Y : Natural; S : String; Ink : Color) is
   begin
      Draw_UI_Text_Transparent (C, X, Y, S, Ink);
   end Text;

   procedure Text_Right (C : Canvas; Right, Y : Natural; S : String; Ink : Color) is
      W : constant Natural := UI_Text_Width (S);
   begin
      Text (C, (if Right > W then Right - W else 0), Y, S, Ink);
   end Text_Right;

   procedure Strong (C : Canvas; X, Y : Natural; S : String; Ink : Color) is
   begin
      Draw_Emphasis_Text_Transparent (C, X, Y, S, Ink);
   end Strong;

   function Panel_Of (P : Dash_Model.Panel_Index) return Dash_Model.Panel is (Board.Panels (P));
   function Panels_Kind (P : Dash_Model.Panel_Index) return Dash_Model.Panel_Kind is (Board.Panels (P).Kind);

   function Time_Based (K : Dash_Model.Panel_Kind) return Boolean is
     (K in Dash_Model.Time_Series | Dash_Model.Heatmap | Dash_Model.Stat);

   --  Live, the plots' right edge moves in steps of Live_Quantum (the widest
   --  column of any plot), all at once and ahead of the clock: a new sample
   --  lands left of the edge (a sliver), and a scroll repaints the plots
   --  together at most once per step instead of whenever any one of them
   --  crosses a column.
   Live_Quantum : Unsigned_64 := 1;
   function Current_End return Unsigned_64 is
     (if Live_Mode then (Feed.Now_Us / Live_Quantum + 1) * Live_Quantum - 1 else End_Us);

   function Value_Of (S : Columns.Summary; Stat : Dash_Model.Statistic; W : Unsigned_64) return Unsigned_64 is
     (case Stat is
        when Dash_Model.Minimum => (if S.Count = 0 then 0 else S.Minimum),
        when Dash_Model.P50 => S.P50,
        when Dash_Model.P99 => S.P99,
        when Dash_Model.Maximum => S.Maximum,
        when Dash_Model.Mean => Columns.Mean (S),
        when Dash_Model.Count => S.Count,
        when Dash_Model.Rate => S.Count * Axis.Microseconds_Per_Second / Unsigned_64'Max (W, 1),
        when Dash_Model.Last => S.Last);

   function Statistic_Name (Stat : Dash_Model.Statistic) return String is
     (case Stat is
        when Dash_Model.Minimum => "min", when Dash_Model.P50 => "p50", when Dash_Model.P99 => "p99",
        when Dash_Model.Maximum => "max", when Dash_Model.Mean => "mean", when Dash_Model.Count => "count",
        when Dash_Model.Rate => "per s", when Dash_Model.Last => "last");

   function Unit_Of (T : Dash_Tracks.Track) return Format.Unit is (T.Info.Unit);

   function Series_Label (T : Dash_Tracks.Track) return String is
     (if T.Spec.Label.Length > 0 then Dash_Model.Image (T.Spec.Label)
      elsif T.State = Dash_Tracks.Resolved then Dash_Model.Image (T.Info.Name)
      else Dash_Model.Image (T.Spec.Name));

   --  The tracks of panel P: First .. Last (none when First > Last).
   procedure Tracks_Of (P : Dash_Model.Panel_Index; First, Last : out Natural);
   procedure Tracks_Of (P : Dash_Model.Panel_Index; First, Last : out Natural) is
   begin
      First := 1;
      Last := 0;
      if Tracks = null then
         return;
      end if;
      for I in Tracks'Range loop
         if Tracks (I).Panel = P then
            if Last = 0 then
               First := I;
            end if;
            Last := I;
         end if;
      end loop;
   end Tracks_Of;

   ---------------------------------------------------------------------------
   --  Layout
   ---------------------------------------------------------------------------
   --  One grid column's and row's pixels, as last laid out (an editor's
   --  drag resizes by them).
   Grid_Unit_W, Grid_Unit_H : Natural := 0;
   --  Grid rows the window's height is shared among (Lay_Out).
   Rows_Basis : Natural := 0;

   procedure Lay_Out;
   procedure Lay_Out is
      --  Skyline packing on the 12-column grid: each panel, in order, at the
      --  lowest place its width fits, leftmost on a tie (panels stack in a
      --  column beside a taller one).
      Placed : Dash_Grid.Placements := [others => <>];
      Total_Rows : Dash_Grid.Row_Count := 0;
      Unit_W, Unit_H : Natural;
   begin
      Grid_Unit_W := 0;
      Grid_Unit_H := 0;
      Menu_Area := (Whole.x, Whole.y, Whole.w, Natural'Min (Menu_Bar_Height, Whole.h));
      Toolbar := (Whole.x, Menu_Area.y + Menu_Area.h, Whole.w,
                  Natural'Min (Toolbar_Height, Whole.h - Menu_Area.h));
      Status := (Whole.x, (if Whole.h > Status_Height then Whole.y + Whole.h - Status_Height else Whole.y),
                 Whole.w, Natural'Min (Status_Height, Whole.h));
      Grid := (Whole.x + Margin, Toolbar.y + Toolbar.h + Margin,
               (if Whole.w > 2 * Margin then Whole.w - 2 * Margin else 0),
               (if Whole.h > Menu_Area.h + Toolbar.h + Status.h + 2 * Margin
                then Whole.h - Menu_Area.h - Toolbar.h - Status.h - 2 * Margin else 0));
      if Board = null then
         return;
      end if;
      Dash_Grid.Pack (Dash_Grid.Sizes_Of (Board.all), Board.Panels_Used, Placed, Total_Rows);
      --  A grid unit is a cell plus one gap; cards are a unit less the gap.
      --  The row unit holds while the dashboard is edited (Rows_Basis grows,
      --  never shrinks, until the window is laid out afresh): a panel keeps
      --  its size wherever it moves, so moving it moves its pixels.
      Rows_Basis := Natural'Max (Rows_Basis, Total_Rows);
      Unit_W := (Grid.w + Gap) / Dash_Model.Grid_Columns;
      Unit_H := Natural'Max (Minimum_Row_Unit, (if Rows_Basis > 0 then (Grid.h + Gap) / Rows_Basis else 0));
      Grid_Unit_W := Unit_W;
      Grid_Unit_H := Unit_H;
      for P in 1 .. Board.Panels_Used loop
         declare
            Item : constant Dash_Model.Panel := Board.Panels (P);
            G : Panel_Geometry renames Panels (P);
            --  Whole units only (what is left over is shared out as margin):
            --  a card's size never depends on where it is.
            Slack_X : constant Natural := (Grid.w + Gap - Natural'Min (Grid.w + Gap, Dash_Model.Grid_Columns * Unit_W)) / 2;
            X : constant Natural := Grid.x + Slack_X + Placed (P).Column * Unit_W;
            Y : constant Natural := Grid.y + Placed (P).Row * Unit_H;
            W : constant Natural := (if Item.Width * Unit_W > Gap then Item.Width * Unit_W - Gap else 1);
            H : constant Natural := (if Item.Height * Unit_H > Gap then Item.Height * Unit_H - Gap else 1);
            Inner_W : constant Natural := (if W > 2 * Padding then W - 2 * Padding else 0);
            Body_Top : constant Natural := Y + Padding + Header_Height;
            Body_H : constant Natural := (if H > 2 * Padding + Header_Height then H - 2 * Padding - Header_Height
                                          else 0);
         begin
            G.Outer := (X, Y, W, H);
            G.Header := (X + Padding, Y + Padding - M.Space_XS, Inner_W, Header_Height);
            G.Body_Area := (X + Padding, Body_Top, Inner_W, Body_H);
            declare
               B : constant Rect := G.Body_Area;
               Left : constant Natural :=
                 (case Item.Kind is
                    when Dash_Model.Time_Series | Dash_Model.Histogram => Left_Axis_Width,
                    when Dash_Model.Heatmap => Heat_Label_Width,
                    when others => 0);
               Bottom : constant Natural :=
                 (case Item.Kind is
                    when Dash_Model.Time_Series | Dash_Model.Heatmap | Dash_Model.Histogram => Bottom_Axis_Height,
                    when others => 0);
            begin
               if Item.Kind = Dash_Model.Stat then
                  --  The number and its note on top, the sparkline in its own
                  --  lane below them; a short tile puts the lane on the right.
                  declare
                     Value_Block : constant Natural :=
                       Styled_Line_Height (Emphasis_Face, Heading_Size) + UI_Text_Height + M.Space_S;
                  begin
                     if B.h < Styled_Line_Height (Emphasis_Face, Heading_Size) then
                        --  Too short for the large number: the number at the
                        --  body's size on the left, the sparkline's lane right.
                        G.Plot := (B.x + B.w - B.w * Spark_Lane_Percent / Palette.Percent, B.y,
                                   B.w * Spark_Lane_Percent / Palette.Percent, B.h);
                     elsif B.h >= Value_Block + Minimum_Spark then
                        G.Plot := (B.x, B.y + Value_Block, B.w, Natural'Min (Spark_Height, B.h - Value_Block));
                        G.Plot.y := B.y + B.h - G.Plot.h;
                     else
                        G.Plot := (B.x + B.w - B.w * Spark_Lane_Percent / Palette.Percent, B.y + M.Space_S,
                                   B.w * Spark_Lane_Percent / Palette.Percent,
                                   (if B.h > M.Space_S then B.h - M.Space_S else B.h));
                     end if;
                  end;
               else
                  G.Plot := (B.x + Natural'Min (Left, B.w), B.y,
                             (if B.w > Left + Minimum_Plot then B.w - Left else Minimum_Plot),
                             (if B.h > Bottom + Minimum_Plot then B.h - Bottom else Minimum_Plot));
               end if;
               G.Left_Axis := (B.x, B.y, Natural'Min (Left, B.w), G.Plot.h);
               G.Bottom_Axis := (G.Plot.x, G.Plot.y + G.Plot.h, G.Plot.w, Bottom);
            end;
         end;
      end loop;
   end Lay_Out;


   --  The widest column any time-based plot has at the current span.
   procedure Update_Quantum;
   procedure Update_Quantum is
   begin
      Live_Quantum := 1;
      if Board = null then
         return;
      end if;
      for P in 1 .. Board.Panels_Used loop
         if Time_Based (Board.Panels (P).Kind) and then Panels (P).Plot.w > 0 then
            Live_Quantum := Unsigned_64'Max
              (Live_Quantum, Axis.Width_For (Span, Natural'Min (Panels (P).Plot.w, Axis.Maximum_Columns)));
         end if;
      end loop;
   end Update_Quantum;

   function Viewport_For (P : Dash_Model.Panel_Index) return Axis.Viewport is
      Plot : constant Rect := Panels (P).Plot;
      Count : constant Axis.Column_Count := Natural'Max (1, Natural'Min (Plot.w, Axis.Maximum_Columns));
      Right : constant Unsigned_64 := Unsigned_64'Min (Current_End, Axis.Maximum_Time_Us / 2);
   begin
      return Axis.Ending_At (Right, Axis.Width_For (Span, Count), Count);
   end Viewport_For;

   ---------------------------------------------------------------------------
   --  Setup
   ---------------------------------------------------------------------------
   procedure Initialize (V : in out View; Source : Feed_Access; D : Dash_Model.Dashboard) is
      pragma Unreferenced (V);
      Count : Natural := 0;
   begin
      Feed := Source;
      Board := new Dash_Model.Dashboard'(D);
      Span := Unsigned_64'Max (Axis.Minimum_Span_Us,
                               Unsigned_64'Min (D.Span * Microseconds_Per_Millisecond, Axis.Maximum_Span_Us));
      Live_Mode := D.Live;
      End_Us := Feed.Now_Us;
      for P in 1 .. D.Panels_Used loop
         for S in 1 .. D.Panels (P).Series_Used loop
            Count := Count + (if D.Panels (P).Series (S).Match = Dash_Model.Prefix then Prefix_Tracks else 1);
         end loop;
      end loop;
      Tracks := new Track_Array'(1 .. Count => null);
      for I in Tracks'Range loop
         Tracks (I) := new Dash_Tracks.Track;
      end loop;
      Count := 0;
      for P in 1 .. D.Panels_Used loop
         for S in 1 .. D.Panels (P).Series_Used loop
            for Match in 1 .. (if D.Panels (P).Series (S).Match = Dash_Model.Prefix then Prefix_Tracks else 1) loop
               Count := Count + 1;
               Tracks (Count).Panel := P;
               Tracks (Count).Panel_Id := D.Panels (P).Id;
               Tracks (Count).Spec_Index := S;
               Tracks (Count).Match_Index := Match;
               Tracks (Count).Spec := D.Panels (P).Series (S);
            end loop;
         end loop;
      end loop;
      Has_Port := [others => False];
      Dash_Edit.Start (D);
      declare
         Accepted : Boolean;
      begin
         Files.Set (Current_Leaf, Dash_Model.File_Leaf (Dash_Model.Image (D.Title)), Accepted);
      end;
      if Open_Board = null then
         Open_Board := new Dash_Model.Dashboard;
      end if;
      Rebuild_Menus;
      Want_List := True;
   end Initialize;

   procedure Set_Bounds (V : in out View; Bounds : Rect; Numerator, Denominator : Positive := 1) is
      pragma Unreferenced (V);
   begin
      Whole := Bounds;
      Keys_W := 0;
      Rows_Basis := 0;
      Dash_Layers.Resize (Bounds.x + Bounds.w, Bounds.y + Bounds.h, Numerator, Denominator);
      Lay_Out;
      Update_Quantum;
      --  Plot widths changed: every track follows on the next refresh.
      Has_Port := [others => False];
      Register;
   end Set_Bounds;

   function Bounds (V : View) return Rect is
      pragma Unreferenced (V);
   begin
      return Whole;
   end Bounds;

   ---------------------------------------------------------------------------
   --  Damage helpers
   ---------------------------------------------------------------------------
   function Plot_Area (P : Dash_Model.Panel_Index) return Rect is
      G : constant Panel_Geometry := Panels (P);
   begin
      return Add_Rect (Add_Rect (G.Plot, G.Bottom_Axis), G.Left_Axis);
   end Plot_Area;

   --  Heatmap cells: never narrower than the store's finest tier (1 s) or
   --  Minimum_Cell_Pixels, so sparse samples read as blocks, not a barcode.
   Minimum_Cell_Pixels : constant := 3;
   --  An over-budget cell's red top edge.
   Over_Edge : constant := 3;
   function Cell_Columns (P : Dash_Model.Panel_Index) return Positive is
     (if not Has_Port (P) then 1
      else Positive'Max (Minimum_Cell_Pixels, Positive (Unsigned_64'Min (Unsigned_64 (Axis.Maximum_Columns),
                         (Dash_Tracks.Finest_Tier_Us + Ports (P).Width - 1) / Ports (P).Width))));

   --  How far a change in one column reaches on screen: a line joins bin
   --  centres, so the neighbouring bins' joins move with it.
   function Bridge_Of (P : Dash_Model.Panel_Index) return Natural is (2 * Cell_Columns (P) + 1);

   function Column_Strip (P : Dash_Model.Panel_Index; First, Last : Column_Number) return Rect is
      Port : constant Axis.Viewport := Ports (P);
      Plot : constant Rect := Panels (P).Plot;
      Lo : constant Column_Number := Unsigned_64'Max (First, Axis.First (Port));
      Hi : constant Column_Number := Unsigned_64'Min (Last, Port.Last);
      Reach : constant Natural := Bridge_Of (P);
   begin
      if Lo > Hi or else not Axis.Visible (Port, Lo) or else not Axis.Visible (Port, Hi) then
         return (others => 0);
      end if;
      declare
         --  To the left as far as a line from an earlier sample reaches;
         --  a dot is a pixel wider than its column.
         X0 : constant Natural := Axis.X_Of (Port, Lo);
         X1 : constant Natural := Axis.X_Of (Port, Hi);
         Left : constant Natural := (if X0 > Reach then X0 - Reach else 0);
         Right : constant Natural := Natural'Min (X1 + Reach, Plot.w - 1);
      begin
         return (Plot.x + Left, Plot.y, Right - Left + 1, Plot.h);
      end;
   end Column_Strip;

   ---------------------------------------------------------------------------
   --  Refresh
   ---------------------------------------------------------------------------
   --  Run (X + 1) := the summary at pixel column X of Port (Nothing left
   --  of the clock's zero).
   procedure Copy_View (T : Dash_Tracks.Track; Port : Axis.Viewport; Run : out Columns.Summary_Array)
     with Pre => Run'First = 1 and then Run'Length = Port.Columns;
   procedure Copy_View (T : Dash_Tracks.Track; Port : Axis.Viewport; Run : out Columns.Summary_Array) is
      Missing : constant Natural :=
        (if Port.Last >= Unsigned_64 (Port.Columns - 1) then 0 else Port.Columns - 1 - Natural (Port.Last));
   begin
      Run (1 .. Missing) := [others => Columns.Nothing];
      Columns.Copy_Run (T.Columns.all, Port.Last + Unsigned_64 (Missing) - Unsigned_64 (Port.Columns - 1),
                        Run (Missing + 1 .. Run'Last));
   end Copy_View;

   --  One labelled value per Tick_Pixels of plot height, at least two.
   Tick_Pixels : constant := 48;
   function Ticks_For (P : Dash_Model.Panel_Index) return Values.Tick_Count is
     (Natural'Max (1, Natural'Min (Values.Maximum_Ticks, Panels (P).Plot.h / Tick_Pixels)));

   function Panel_Top (P : Dash_Model.Panel_Index) return Unsigned_64;
   function Panel_Top (P : Dash_Model.Panel_Index) return Unsigned_64 is
      Item : constant Dash_Model.Panel := Panel_Of (P);
      Port : constant Axis.Viewport := Ports (P);
      First, Last : Natural;
      High : Unsigned_64 := Item.Budget;
   begin
      Tracks_Of (P, First, Last);
      Lows (P) := Unsigned_64'Last;
      for I in First .. Last loop
         if Tracks (I).Columns /= null and then Tracks (I).State = Dash_Tracks.Resolved then
            declare
               --  The ring holds exactly the viewport's columns (the others
               --  are cleared as they scroll out): its items in any order.
               Run : Columns.Summary_Array renames Tracks (I).Columns.Items;
            begin
               for S of Run loop
                  if S.Origin /= Columns.Empty then
                     case Item.Kind is
                        when Dash_Model.Time_Series =>
                           for Stat in Dash_Model.Statistic loop
                              if Item.Lines (Stat) then
                                 High := Unsigned_64'Max (High, Value_Of (S, Stat, Port.Width));
                              end if;
                           end loop;
                           if Item.Band and then I = First then
                              High := Unsigned_64'Max (High, S.Maximum);
                           end if;
                        when Dash_Model.Heatmap | Dash_Model.Stat =>
                           High := Unsigned_64'Max (High, Value_Of (S, Item.Shown, Port.Width));
                           Lows (P) := Unsigned_64'Min (Lows (P), Unsigned_64'Max (1, Value_Of (S, Item.Shown,
                                                                                                  Port.Width)));
                        when others => null;
                     end case;
                  end if;
               end loop;
            end;
         end if;
      end loop;
      if Item.Kind = Dash_Model.Heatmap then
         return Unsigned_64'Max (1, High);
      end if;
      return Values.Axis_Top (Values.Clamp (High), Ticks_For (P));
   end Panel_Top;

   --  The live dot pulses: Pulse_Steps shades a second.
   Pulse_Steps : constant := 8;
   Pulse_Step_Us : constant := Axis.Microseconds_Per_Second / Pulse_Steps;
   Shown_Pulse : Natural := Natural'Last;
   function Pulse_Phase return Natural is (Natural ((Feed.Now_Us / Pulse_Step_Us) mod Pulse_Steps));
   function Live_Dot_Rect return Rect;

   function Cell_Strip (P : Dash_Model.Panel_Index; First, Last : Column_Number) return Rect is
      K : constant Unsigned_64 := Unsigned_64 (Cell_Columns (P));
      Port : constant Axis.Viewport := Ports (P);
      Lo : constant Column_Number := Unsigned_64'Max (First / K * K, Axis.First (Port));
      Hi : constant Column_Number := Unsigned_64'Min ((Last / K + 1) * K - 1, Port.Last);
      Plot : constant Rect := Panels (P).Plot;
   begin
      if Lo > Hi or else not Axis.Visible (Port, Lo) or else not Axis.Visible (Port, Hi) then
         return (others => 0);
      end if;
      return (Plot.x + Axis.X_Of (Port, Lo), Plot.y, Axis.X_Of (Port, Hi) - Axis.X_Of (Port, Lo) + 1, Plot.h);
   end Cell_Strip;

   procedure Scroll_Plot (P : Dash_Model.Panel_Index; Old, New_Port : Axis.Viewport; Damage : in out Rect);

   --  Which series have new samples: the store's listing, a page of a
   --  dozen series a call, every Probe_Interval_Us. That is how often the
   --  store takes what publishers put in their rings (metrics.svc drains
   --  outlets every 100 ms, Metric_Outlets.Drain_Interval_Ms): asking more
   --  often finds nothing new. Between probes a refresh makes no call; a
   --  series with nothing new makes none either (DESIGN-metrics-push.md
   --  removes both waits).
   Probe_Interval_Us : constant := 100 * Microseconds_Per_Millisecond;
   Probe_Page : constant := 16;
   Next_Probe_Us : Unsigned_64 := 0;

   procedure Probe;
   procedure Probe is
      Found : Dash_Feeds.Series_Infos (1 .. Probe_Page);
      Listed, Total : Natural;
      Skip : Natural := 0;
      Result : Dash_Feeds.Status;
      use type Dash_Feeds.Status;
   begin
      loop
         Feed.List_All (Skip, Found, Listed, Total, Result);
         exit when Result /= Dash_Feeds.OK or else Listed = 0;
         Dash_Sync.Lock;
         for F of Found (1 .. Listed) loop
            for T of Tracks.all loop
               if T.State = Dash_Tracks.Resolved and then T.Info.Id = F.Id then
                  T.Latest_Published := F.Published;
               end if;
            end loop;
         end loop;
         Dash_Sync.Unlock;
         Skip := Skip + Listed;
         exit when Skip >= Total;
      end loop;
   end Probe;

   --  Push (DESIGN-metrics-push.md): what the store pushed since the last
   --  frame, routed to the tracks of its series. No call: a ring read.
   Push_Tried : Boolean := False;
   Push_Since_Us : Unsigned_64 := 0;
   Updates_Per_Take : constant := 256;
   Pushed_Total : Unsigned_64 := 0;

   procedure Take_Pushed;
   procedure Take_Pushed is
      Batch : Dash_Feeds.Update_Array (1 .. Updates_Per_Take);
      Count : Natural;
      use type Dash_Feeds.Update_Kind;
   begin
      Dash_Sync.Lock;
      for T of Tracks.all loop
         if T.State = Dash_Tracks.Resolved and then not T.Pushed then
            --  A series resolved since the subscription: pushed from now.
            Dash_Tracks.Begin_Push (T.all, Unsigned_64'Max (Push_Since_Us, Feed.Now_Us));
         end if;
      end loop;
      Dash_Sync.Unlock;
      loop
         --  The ring read and decoding without the lock; the routing with it.
         Feed.Take_Updates (Batch, Count);
         exit when Count = 0;
         Dash_Sync.Lock;
         for U of Batch (1 .. Count) loop
            if U.Kind = Dash_Feeds.Sample then
               Pushed_Total := Pushed_Total + 1;
               for T of Tracks.all loop
                  if T.Pushed and then T.Info.Id = U.Series then
                     Dash_Tracks.Accept_Sample (T.all, U.Time_Us, U.Value);
                  end if;
               end loop;
            elsif U.Series = Dash_Feeds.No_Series then
               --  The subscription itself lost samples: the query path
               --  makes those columns again.
               for T of Tracks.all loop
                  Dash_Tracks.Push_Lost (T.all, U.First_Us, U.Last_Us);
               end loop;
            end if;
         end loop;
         Dash_Sync.Unlock;
         exit when Count < Batch'Length;
      end loop;
   end Take_Pushed;

   --  On first open the span fits the data there is, up to the dashboard's
   --  (so a young store is never a sliver at the right edge); it grows with
   --  the data in the time ticks' steps and stops at the dashboard's span,
   --  or at the first zoom, pan or range the person picks.
   Auto_Fit : Boolean := False;
   Selected_Span : Unsigned_64 := 0;
   Minimum_Fit_Us : constant := 10 * Axis.Microseconds_Per_Second;
   Fit_Margin_Divisor : constant := 10;

   procedure Fit_Span (Damage : in out Rect);
   procedure Fit_Span (Damage : in out Rect) is
      Earliest : Unsigned_64 := Unsigned_64'Last;
      Now : constant Unsigned_64 := Feed.Now_Us;
      Candidate : Unsigned_64 := Selected_Span;
   begin
      for T of Tracks.all loop
         if T.State = Dash_Tracks.Resolved and then T.Info.First_Us > 0 then
            Earliest := Unsigned_64'Min (Earliest, T.Info.First_Us);
         end if;
      end loop;
      if Earliest = Unsigned_64'Last or else Earliest > Now then
         return;
      end if;
      declare
         Want : constant Unsigned_64 :=
           Unsigned_64'Max (Minimum_Fit_Us, (Now - Earliest) + (Now - Earliest) / Fit_Margin_Divisor);
      begin
         for I in Axis.Tick_Index loop
            if Axis.Steps (I) >= Want then
               Candidate := Unsigned_64'Min (Axis.Steps (I), Selected_Span);
               exit;
            end if;
         end loop;
      end;
      if Candidate >= Selected_Span then
         Candidate := Selected_Span;
         Auto_Fit := False;
      end if;
      if Candidate /= Span then
         Span := Unsigned_64'Max (Axis.Minimum_Span_Us, Candidate);
         Update_Quantum;
         Mark (Damage, Toolbar);
      end if;
   end Fit_Span;

   --  The state each track was last drawn in (a change redraws its panel).
   type State_List is array (Positive range <>) of Dash_Tracks.Track_State;
   type State_List_Access is access State_List;
   Drawn_States : State_List_Access;

   ---------------------------------------------------------------------------
   --  The editor
   ---------------------------------------------------------------------------
   Edit_Mode : Boolean := False;
   Selected_Panel : Natural := 0;
   Edit_Count : Natural := 0;
   --  Saving: the edit last written (or given up on), the one being
   --  written, when the last edit was made, and what the status bar says.
   Saved_Generation : Natural := 0;
   Save_Pending : Boolean := False;
   Last_Edit_Us : Unsigned_64 := 0;
   Shown_Save : Save_State := Unedited;
   Save_Reason : Dash_Model.Text;
   --  The data thread changed the save state: the status bar is redrawn
   --  at the next refresh.
   Save_Changed : Boolean := False;
   --  Bumped whenever the panels are laid out again (an edit): the data
   --  thread's step ends rather than walk a layout that changed under it.
   Layout_Generation : Natural := 0 with Atomic;
   --  Tracks of removed panels and the arrays they were in, given back by
   --  the data thread (it may still hold one across a call).
   type Retired_Item is record
      Track : Track_Access;
      List : Track_Array_Access;
   end record;
   type Retired_List is array (Positive range <>) of Retired_Item;
   type Retired_Access is access Retired_List;
   Retired : Retired_Access;
   Retired_Used : Natural := 0;
   procedure Free is new Ada.Unchecked_Deallocation (Dash_Tracks.Track, Track_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Track_Array, Track_Array_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Retired_List, Retired_Access);
   procedure Free is new Ada.Unchecked_Deallocation (State_List, State_List_Access);

   procedure Retire (Item : Retired_Item);
   procedure Retire (Item : Retired_Item) is
   begin
      if Retired = null then
         Retired := new Retired_List (1 .. 16);
      elsif Retired_Used = Retired'Length then
         declare
            Grown : constant Retired_Access := new Retired_List (1 .. 2 * Retired'Length);
         begin
            Grown (1 .. Retired_Used) := Retired (1 .. Retired_Used);
            Free (Retired);
            Retired := Grown;
         end;
      end if;
      Retired_Used := Retired_Used + 1;
      Retired (Retired_Used) := Item;
   end Retire;

   --  The data thread, at the start of a step (under the lock): nothing it
   --  holds is retired any more.
   procedure Release_Retired;
   procedure Release_Retired is
   begin
      for I in 1 .. Retired_Used loop
         if Retired (I).Track /= null then
            Dash_Tracks.Release (Retired (I).Track.all);
            Free (Retired (I).Track);
         end if;
         if Retired (I).List /= null then
            Free (Retired (I).List);
         end if;
      end loop;
      Retired_Used := 0;
   end Release_Retired;

   function Grip_Rect (P : Dash_Model.Panel_Index) return Rect;
   procedure Paint (C : Canvas; Damage : Rect);
   procedure Refresh_Panel (P : Dash_Model.Panel_Index; Damage : in out Rect);

   --  The toolkit's frame grows by R; the retained frame already holds it.
   procedure Mark_Copied (Damage : in out Rect; R : Rect);
   procedure Mark_Copied (Damage : in out Rect; R : Rect) is
   begin
      if not Is_Empty (R) then
         Marked_Pixels := Marked_Pixels + Unsigned_64 (R.w) * Unsigned_64 (R.h);
         Damage := Add_Rect (Damage, R);
      end if;
   end Mark_Copied;

   --  The selection's outline and grip (what selecting a panel changes),
   --  painted straight into the retained frame: the ring and the grip's
   --  corner hold only the card's edge and fill (the padding), so neither
   --  state needs the panel drawn again.
   procedure Draw_Selection (C : Canvas; K : Colors; P : Dash_Model.Panel_Index);
   procedure Mark_Outline (P : Natural; Damage : in out Rect);
   procedure Mark_Outline (P : Natural; Damage : in out Rect) is
      O : Rect;
      S : constant := Selection_Stroke;
      K : Colors;
   begin
      if Board = null or else P not in 1 .. Board.Panels_Used then
         return;
      end if;
      O := Panels (P).Outer;
      if not Dash_Layers.Ready or else Source_Shown or else Touches_Layer (O) then
         --  Painted straight in, it would land over a layer.
         Mark (Damage, O);
         return;
      end if;
      K := Current_Colors;
      declare
         C : constant Canvas := Dash_Layers.Canvas (O);
      begin
         if Edit_Mode and then P = Selected_Panel then
            Draw_Selection (C, K, P);
         else
            --  The plain card: its fill under the ring and the grip, its edge.
            for I in 1 .. S - 1 loop
               if O.w > 2 * I and then O.h > 2 * I then
                  Stroke_Rect (C, (O.x + I, O.y + I, O.w - 2 * I, O.h - 2 * I), K.Card, K.Card);
               end if;
            end loop;
            Fill_Rect (C, Grip_Rect (P), K.Card);
            Stroke_Rect (C, O, K.Edge, K.Edge);
         end if;
      end;
      Mark_Copied (Damage, (O.x, O.y, O.w, Natural'Min (S, O.h)));
      Mark_Copied (Damage, (O.x, (if O.h > S then O.y + O.h - S else O.y), O.w, Natural'Min (S, O.h)));
      Mark_Copied (Damage, (O.x, O.y, Natural'Min (S, O.w), O.h));
      Mark_Copied (Damage, ((if O.w > S then O.x + O.w - S else O.x), O.y, Natural'Min (S, O.w), O.h));
      Mark_Copied (Damage, Grip_Rect (P));
   end Mark_Outline;

   --  Paint what is waiting into the retained frame now (the layout it
   --  was marked for is about to change).
   procedure Flush_Layers;
   procedure Flush_Layers is
   begin
      for I in 1 .. Dash_Layers.Dirty_Count loop
         declare
            R : constant Rect := Dash_Layers.Dirty (I);
         begin
            Paint (Dash_Layers.Canvas (R), R);
         end;
      end loop;
      Dash_Layers.Clear_Dirty;
   end Flush_Layers;


   --  R less Cut: up to four pieces.
   type Piece_List is array (1 .. 4) of Rect;
   procedure Subtract (R, Cut : Rect; Pieces : out Piece_List; Count : out Natural);
   procedure Subtract (R, Cut : Rect; Pieces : out Piece_List; Count : out Natural) is
      Top, Bottom, Left, Right : Natural;
   begin
      Pieces := [others => (others => 0)];
      if not Intersects (R, Cut) then
         Pieces (1) := R;
         Count := 1;
         return;
      end if;
      Count := 0;
      Top := Natural'Max (R.y, Cut.y);
      Bottom := Natural'Min (R.y + R.h, Cut.y + Cut.h);
      Left := Natural'Max (R.x, Cut.x);
      Right := Natural'Min (R.x + R.w, Cut.x + Cut.w);
      if Top > R.y then
         Count := Count + 1;
         Pieces (Count) := (R.x, R.y, R.w, Top - R.y);
      end if;
      if Bottom < R.y + R.h then
         Count := Count + 1;
         Pieces (Count) := (R.x, Bottom, R.w, R.y + R.h - Bottom);
      end if;
      if Left > R.x then
         Count := Count + 1;
         Pieces (Count) := (R.x, Top, Left - R.x, Bottom - Top);
      end if;
      if Right < R.x + R.w then
         Count := Count + 1;
         Pieces (Count) := (Right, Top, R.x + R.w - Right, Bottom - Top);
      end if;
   end Subtract;

   --  D replaces the dashboard (an edit): panels are laid out again and each
   --  keeps the tracks it had (by panel identity), so the data stays;
   --  removed panels' tracks are retired. What is redrawn is only what
   --  changed: a panel that moved at the same size has its pixels moved
   --  (Dash_Layers.Move_Blocks), a resized or new one is drawn, and the page
   --  left bare is filled. Was_Selected: the panel identity selected before.
   --  UI thread, under the lock.
   procedure Apply_Dashboard (D : Dash_Model.Dashboard; Damage : in out Rect; Was_Selected : Natural := 0);
   procedure Apply_Dashboard (D : Dash_Model.Dashboard; Damage : in out Rect; Was_Selected : Natural := 0) is
      type Old_Panel is record
         Id : Natural := 0;
         Outer, Plot : Rect := (others => 0);
         Port : Axis.Viewport;
         Had_Port : Boolean := False;
         Top, Low : Unsigned_64 := 1;
      end record;
      type Old_List is array (Dash_Model.Panel_Index) of Old_Panel;
      Olds : Old_List;
      Old_Count : constant Natural := Board.Panels_Used;
      Count : Natural := 0;
      Old : Track_Array_Access := Tracks;
      Old_States : State_List_Access := Drawn_States;
      Old_Of : array (Dash_Model.Panel_Index) of Natural := [others => 0];
      Moves : Dash_Layers.Block_Moves (1 .. Dash_Model.Maximum_Panels);
      Move_Count : Natural := 0;
      Redraw : Flag_List := [others => False];
      Moved_Pixels : Boolean := False;
   begin
      Flush_Layers;
      for P in 1 .. Old_Count loop
         Olds (P) := (Board.Panels (P).Id, Panels (P).Outer, Panels (P).Plot, Ports (P), Has_Port (P), Tops (P),
                      Lows (P));
      end loop;
      for P in 1 .. D.Panels_Used loop
         for S in 1 .. D.Panels (P).Series_Used loop
            Count := Count + (if D.Panels (P).Series (S).Match = Dash_Model.Prefix then Prefix_Tracks else 1);
         end loop;
      end loop;
      Tracks := new Track_Array'(1 .. Count => null);
      if Old_States /= null then
         Drawn_States := new State_List'(Tracks'Range => Dash_Tracks.Unresolved);
      end if;
      Count := 0;
      for P in 1 .. D.Panels_Used loop
         for S in 1 .. D.Panels (P).Series_Used loop
            for Match in 1 .. (if D.Panels (P).Series (S).Match = Dash_Model.Prefix then Prefix_Tracks else 1) loop
               Count := Count + 1;
               for I in Old'Range loop
                  if Old (I) /= null and then Old (I).Panel_Id = D.Panels (P).Id and then Old (I).Spec_Index = S
                    and then Old (I).Match_Index = Match
                  then
                     Tracks (Count) := Old (I);
                     if Old_States /= null then
                        Drawn_States (Count) := Old_States (I);
                     end if;
                     Old (I) := null;
                     exit;
                  end if;
               end loop;
               if Tracks (Count) = null then
                  Tracks (Count) := new Dash_Tracks.Track;
                  Tracks (Count).Spec_Index := S;
                  Tracks (Count).Match_Index := Match;
                  Tracks (Count).Spec := D.Panels (P).Series (S);
                  Tracks (Count).Panel_Id := D.Panels (P).Id;
               end if;
               Tracks (Count).Panel := P;
            end loop;
         end loop;
      end loop;
      for I in Old'Range loop
         if Old (I) /= null then
            Retire ((Track => Old (I), List => null));
         end if;
      end loop;
      Retire ((Track => null, List => Old));
      if Old_States /= null then
         Free (Old_States);
      end if;
      Board.all := D;
      Layout_Generation := Layout_Generation + 1;
      Lay_Out;
      Update_Quantum;
      --  Each panel's state follows it (by identity); what it had drawn
      --  moves with it when its size is the same.
      Has_Port := [others => False];
      for P in 1 .. D.Panels_Used loop
         for Q in 1 .. Old_Count loop
            if Olds (Q).Id = D.Panels (P).Id then
               Old_Of (P) := Q;
            end if;
         end loop;
         declare
            Q : constant Natural := Old_Of (P);
            O : constant Rect := Panels (P).Outer;
         begin
            if Q = 0 then
               Redraw (P) := True;
            else
               Tops (P) := Olds (Q).Top;
               Lows (P) := Olds (Q).Low;
               if Olds (Q).Outer.w = O.w and then Olds (Q).Outer.h = O.h then
                  Ports (P) := Olds (Q).Port;
                  Has_Port (P) := Olds (Q).Had_Port;
                  if Olds (Q).Outer /= O then
                     Move_Count := Move_Count + 1;
                     Moves (Move_Count) := (From => Olds (Q).Outer, To => O);
                  end if;
               else
                  Redraw (P) := True;
               end if;
            end if;
         end;
      end loop;
      --  A layer's pixels must not move with a panel's.
      if (for some M of Moves (1 .. Move_Count) => Touches_Layer (M.From) or else Touches_Layer (M.To)) then
         Moved_Pixels := False;
      else
         Dash_Layers.Move_Blocks (Moves (1 .. Move_Count), Moved_Pixels);
      end if;
      for M of Moves (1 .. Move_Count) loop
         if Moved_Pixels then
            Mark_Copied (Damage, M.To);
         else
            Mark (Damage, M.To);
         end if;
      end loop;
      for P in 1 .. D.Panels_Used loop
         if Redraw (P) then
            Mark (Damage, Panels (P).Outer);
            if Drawn_States /= null then
               --  Its tracks follow the new plot now, so this frame draws it.
               Refresh_Panel (P, Damage);
            end if;
         end if;
      end loop;
      --  The page where a card was and none is now.
      for Q in 1 .. Old_Count loop
         declare
            type Piece_Array is array (1 .. 64) of Rect;
            Bare : Piece_Array;
            Bare_Count : Natural := 1;
            Unchanged : Boolean := False;
         begin
            for P in 1 .. D.Panels_Used loop
               if Old_Of (P) = Q and then Panels (P).Outer = Olds (Q).Outer then
                  Unchanged := True;
               end if;
            end loop;
            if not Unchanged then
               Bare (1) := Olds (Q).Outer;
               for P in 1 .. D.Panels_Used loop
                  declare
                     Next : Piece_Array;
                     Next_Count : Natural := 0;
                  begin
                     for I in 1 .. Bare_Count loop
                        declare
                           Pieces : Piece_List;
                           N : Natural;
                        begin
                           Subtract (Bare (I), Panels (P).Outer, Pieces, N);
                           for J in 1 .. N loop
                              if Next_Count < Next'Last then
                                 Next_Count := Next_Count + 1;
                                 Next (Next_Count) := Pieces (J);
                              else
                                 --  Too many pieces: the bounding box.
                                 Next (Next_Count) := Add_Rect (Next (Next_Count), Pieces (J));
                              end if;
                           end loop;
                        end;
                     end loop;
                     Bare := Next;
                     Bare_Count := Next_Count;
                  end;
               end loop;
               for I in 1 .. Bare_Count loop
                  Mark (Damage, Bare (I));
               end loop;
            end if;
         end;
      end loop;
      --  The selection: an outline that left a panel (or arrived on one
      --  whose pixels were not redrawn).
      if Was_Selected /= 0 and then (Selected_Panel not in 1 .. D.Panels_Used
                                     or else D.Panels (Selected_Panel).Id /= Was_Selected)
      then
         for P in 1 .. D.Panels_Used loop
            if D.Panels (P).Id = Was_Selected then
               Mark_Outline (P, Damage);
            end if;
         end loop;
         Mark_Outline (Selected_Panel, Damage);
      end if;
      if Detail_Valid then
         Detail_Valid := False;
      end if;
      Edit_Count := Edit_Count + 1;
      Last_Edit_Us := Feed.Now_Us;
      Shown_Save := Edited;
      Mark (Damage, Status);
   end Apply_Dashboard;

   procedure Edit (Op : Dash_Edit.Operation; Damage : in out Rect);
   procedure Edit (Op : Dash_Edit.Operation; Damage : in out Rect) is
      D : Dash_Model.Dashboard := Board.all;
      Done : Boolean;
      Selected : Natural;
   begin
      if Selected_Panel = 0 then
         return;
      end if;
      Dash_Edit.Apply (D, Op, Selected_Panel, Done, Selected);
      if Done then
         declare
            Was : constant Natural := Board.Panels (Selected_Panel).Id;
         begin
            Selected_Panel := Selected;
            Apply_Dashboard (D, Damage, Was);
         end;
      end if;
   end Edit;

   procedure Undo_Redo (Forward : Boolean; Damage : in out Rect);
   procedure Undo_Redo (Forward : Boolean; Damage : in out Rect) is
      D : Dash_Model.Dashboard := Board.all;
      Done : Boolean;
   begin
      if Forward then
         Dash_Edit.Redo (D, Done);
      else
         Dash_Edit.Undo (D, Done);
      end if;
      if Done then
         declare
            Was : constant Natural :=
              (if Selected_Panel in 1 .. Board.Panels_Used then Board.Panels (Selected_Panel).Id else 0);
         begin
            --  The selection stays on its panel when the undo keeps it.
            Selected_Panel := Natural'Min (Selected_Panel, D.Panels_Used);
            for P in 1 .. D.Panels_Used loop
               if D.Panels (P).Id = Was then
                  Selected_Panel := P;
               end if;
            end loop;
            Apply_Dashboard (D, Damage, Was);
         end;
      end if;
   end Undo_Redo;

   function Editing (V : View) return Boolean is
      pragma Unreferenced (V);
   begin
      return Edit_Mode;
   end Editing;

   function Dashboard_Source (V : View) return String is
      pragma Unreferenced (V);
   begin
      return (if Board = null then "" else Dash_Model.To_Source (Board.all));
   end Dashboard_Source;

   procedure Take_Save (V : in out View; Due : out Boolean; Generation : out Natural) is
      pragma Unreferenced (V);
   begin
      if Parting_Source /= null and then not Save_Pending then
         --  The dashboard left behind first.
         Due := True;
         Generation := 0;
         Saving_Parting := True;
         Save_Pending := True;
         return;
      end if;
      Due := Board /= null and then not Save_Pending and then Edit_Count /= Saved_Generation
        and then (not Edit_Mode or else Feed.Now_Us - Last_Edit_Us >= Autosave_Delay_Ms * Microseconds_Per_Millisecond);
      Generation := Edit_Count;
      if Due then
         Save_Pending := True;
         In_Flight_Generation := Generation;
         Shown_Save := Saving;
         Save_Changed := True;
      end if;
   end Take_Save;

   procedure Set_Notice_Text (S : String);
   procedure Set_Notice_Text (S : String) is
   begin
      Notice_Length := Natural'Min (S'Length, Maximum_Notice);
      Notice_Text (1 .. Notice_Length) := S (S'First .. S'First + Notice_Length - 1);
      Notice_Until_Us := Feed.Now_Us + Notice_Lifetime_Us;
   end Set_Notice_Text;

   procedure End_Save (V : in out View; Generation : Natural; Saved : Boolean; Reason : String := "") is
      pragma Unreferenced (V);
   begin
      Save_Pending := False;
      if Saving_Parting then
         Saving_Parting := False;
         if not Saved then
            Set_Notice_Text (Files.Value (Parting_Name) & ": " & Reason);
            Save_Changed := True;
         end if;
         Free (Parting_Source);
         return;
      end if;
      if Generation < Board_First_Generation then
         --  An answer for the dashboard shown before this one.
         return;
      end if;
      --  A failure is not retried until the next edit (no loop of failing
      --  writes); the status bar says why.
      Saved_Generation := Generation;
      Shown_Save := (if Edit_Count /= Generation then Edited elsif Saved then Dash_View.Saved else Not_Saved);
      Save_Reason := Dash_Model.To_Text (Reason);
      Save_Changed := True;
   end End_Save;

   function Save_Name (V : View) return String is
      pragma Unreferenced (V);
   begin
      return Dash_Model.Dashboards_Place
        & (if Saving_Parting then Files.Value (Parting_Name) else Files.Value (Current_Leaf));
   end Save_Name;

   function Save_Source (V : View) return String is
      pragma Unreferenced (V);
   begin
      return (if Saving_Parting and then Parting_Source /= null then Parting_Source.all
              elsif Board = null then "" else Dash_Model.To_Source (Board.all));
   end Save_Source;

   function Current_File (V : View) return String is
      pragma Unreferenced (V);
   begin
      return Files.Value (Current_Leaf);
   end Current_File;

   function Saving_State (V : View) return Save_State is
      pragma Unreferenced (V);
   begin
      return Shown_Save;
   end Saving_State;

   function Source_Showing (V : View) return Boolean is
      pragma Unreferenced (V);
   begin
      return Source_Shown and then Shown_Overlay = Source_Overlay;
   end Source_Showing;

   function Edits (V : View) return Natural is
      pragma Unreferenced (V);
   begin
      return Edit_Count;
   end Edits;

   --  The UI thread's waits for the lock, summed until taken.
   UI_Waited_Us : Unsigned_64 := 0;
   procedure Lock_UI;
   procedure Lock_UI is
      Waited : Unsigned_64;
   begin
      Dash_Sync.Lock_Waited (Waited);
      UI_Waited_Us := UI_Waited_Us + Waited;
   end Lock_UI;

   procedure Take_Lock_Wait (V : View; Waited_Us : out Unsigned_64) is
      pragma Unreferenced (V);
   begin
      Waited_Us := UI_Waited_Us;
      UI_Waited_Us := 0;
   end Take_Lock_Wait;

   --  The store's catalog, every page of it, by name (the data thread's;
   --  the lock only to publish it, and only when it changed).
   procedure Read_Catalog;
   procedure Read_Catalog is
      Page : Dash_Feeds.Series_Infos (1 .. Probe_Page);
      Names : Catalog_List;
      Used : Catalog_Count := 0;
      Listed, Total : Natural;
      Skip : Natural := 0;
      Result : Dash_Feeds.Status;
      use type Dash_Feeds.Status;
   begin
      loop
         Feed.List_All (Skip, Page, Listed, Total, Result);
         exit when Result /= Dash_Feeds.OK or else Listed = 0;
         for F of Page (1 .. Listed) loop
            declare
               Name : constant String := Dash_Model.Image (F.Name);
               At_Index : Positive := Used + 1;
               Known : Boolean := False;
            begin
               for K in 1 .. Used loop
                  if Dash_Model.Image (Names (K).Name) = Name then
                     Known := True;
                  elsif At_Index > Used and then Name < Dash_Model.Image (Names (K).Name) then
                     At_Index := K;
                  end if;
               end loop;
               if not Known and then Used < Names'Last then
                  Names (At_Index + 1 .. Used + 1) := Names (At_Index .. Used);
                  Names (At_Index) := (Name => F.Name);
                  Used := Used + 1;
               end if;
            end;
         end loop;
         Skip := Skip + Listed;
         exit when Skip >= Total;
      end loop;
      Dash_Sync.Lock;
      if Used /= Catalog_Used
        or else (for some K in 1 .. Used => Dash_Model.Image (Names (K).Name) /= Dash_Model.Image (Catalog (K).Name))
      then
         Catalog := Names;
         Catalog_Used := Used;
         Catalog_Generation := Catalog_Generation + 1;
      end if;
      Dash_Sync.Unlock;
   end Read_Catalog;

   procedure Data_Step (V : in out View) is
      pragma Unreferenced (V);
      Started : constant Unsigned_64 := Feed.Now_Us;
      Budget_End : constant Unsigned_64 := Started + Fetch_Budget_Us;
   begin
      if Board = null or else Tracks = null then
         return;
      end if;
      if Catalog_Wanted then
         Catalog_Wanted := False;
         Read_Catalog;
      end if;
      if not Push_Tried then
         --  Once: push if the store offers it (the query path stays for
         --  history, zoom and stats).
         Push_Tried := True;
         declare
            Started_Push : Boolean;
         begin
            Feed.Start_Push (Started_Push);
            Push_Since_Us := Feed.Now_Us;
         end;
      end if;
      if Feed.Pushing then
         Take_Pushed;
      elsif Started >= Next_Probe_Us then
         Next_Probe_Us := Started + Probe_Interval_Us;
         Probe;
      end if;
      Dash_Sync.Lock;
      Release_Retired;
      declare
         Generation : constant Natural := Layout_Generation;
      begin
      for P in 1 .. Board.Panels_Used loop
         exit when Layout_Generation /= Generation;
         if Has_Port (P) then
            declare
               --  The UI thread's viewport, as it was when the lock was
               --  taken (a later move bumps the tracks' generation, and an
               --  answer for this one is dropped).
               Port : constant Axis.Viewport := Ports (P);
               Item : constant Dash_Model.Panel := Panel_Of (P);
               First, Last : Natural;
            begin
               Tracks_Of (P, First, Last);
               for I in First .. Last loop
                  --  An edit (the UI thread) laid the dashboard out again
                  --  while a call was out: this step ends, the next follows it.
                  exit when Layout_Generation /= Generation;
                  declare
                     T : Dash_Tracks.Track renames Tracks (I).all;
                  begin
                     if T.Columns /= null then
                        Dash_Tracks.Resolve (T, Feed.all, Started);
                        Dash_Tracks.Refresh (T, Feed.all, Port, Feed.Now_Us, Budget_End);
                        if Item.Kind in Dash_Model.Stat | Dash_Model.Histogram then
                           Dash_Tracks.Refresh_Stats (T, Feed.all, Port, Started);
                        end if;
                     end if;
                  end;
               end loop;
            end;
         end if;
      end loop;
      end;
      Dash_Sync.Unlock;
      Last_Refresh_Cost := Feed.Now_Us - Started;
   end Data_Step;

   --  Panel P against the viewport it should show now: its tracks follow,
   --  what changed is marked. Under the lock.
   procedure Refresh_Panel (P : Dash_Model.Panel_Index; Damage : in out Rect) is
   begin
      declare
         Port : constant Axis.Viewport := Viewport_For (P);
         Item : constant Dash_Model.Panel := Panel_Of (P);
         First, Last : Natural;
         Moved : constant Boolean := not Has_Port (P) or else Ports (P) /= Port;
         Whole_Panel : Boolean := False;
      begin
         Tracks_Of (P, First, Last);
         for I in First .. Last loop
            declare
               T : Dash_Tracks.Track renames Tracks (I).all;
            begin
               if T.Columns = null then
                  Dash_Tracks.Configure (T, Port, Item.Kind = Dash_Model.Histogram, Item.Buckets);
               elsif Moved then
                  Dash_Tracks.Follow (T, Port);
               end if;
               if T.Stats_Changed then
                  T.Stats_Changed := False;
                  Whole_Panel := True;
               end if;
               if T.State /= Drawn_States (I) then
                  Drawn_States (I) := T.State;
                  Whole_Panel := True;
                  Mark (Damage, Status);
               end if;
            end;
         end loop;
         if Moved then
            if Item.Kind in Dash_Model.Time_Series | Dash_Model.Heatmap and then Has_Port (P) then
               Scroll_Plot (P, Ports (P), Port, Damage);
            elsif Item.Kind in Dash_Model.Time_Series | Dash_Model.Heatmap then
               Mark (Damage, Plot_Area (P));
            else
               Whole_Panel := True;
            end if;
            Ports (P) := Port;
            Has_Port (P) := True;
         end if;
         declare
            Top : constant Unsigned_64 := Panel_Top (P);
         begin
            if Top /= Tops (P) then
               Tops (P) := Top;
               Whole_Panel := True;
            end if;
         end;
         for I in First .. Last loop
            declare
               Any : Boolean;
               Lo, Hi : Column_Number;
            begin
               Dash_Tracks.Take_Dirty (Tracks (I).all, Any, Lo, Hi);
               if Any then
                  case Item.Kind is
                     when Dash_Model.Time_Series => Mark (Damage, Column_Strip (P, Lo, Hi));
                     when Dash_Model.Heatmap => Mark (Damage, Cell_Strip (P, Lo, Hi));
                     when Dash_Model.Stat => Mark (Damage, Panels (P).Plot);
                     when others => Whole_Panel := True;
                  end case;
               end if;
            end;
         end loop;
         if Whole_Panel then
            Mark (Damage, Panels (P).Outer);
         end if;
      end;
   end Refresh_Panel;

   procedure Take_Answers (Damage : in out Rect);

   procedure Refresh (V : in out View; Damage : in out Rect) is
      pragma Unreferenced (V);
   begin
      if Board = null or else Tracks = null then
         return;
      end if;
      Lock_UI;
      --  What the data thread answered (a dashboard opened replaces this one).
      Take_Answers (Damage);
      if Drawn_States = null then
         Drawn_States := new State_List'(Tracks'Range => Dash_Tracks.Unresolved);
         --  First frame: fit the span to the data while live.
         Selected_Span := Span;
         Auto_Fit := Live_Mode;
      end if;
      if Save_Changed then
         Save_Changed := False;
         Mark (Damage, Status);
      end if;
      if Auto_Fit then
         Fit_Span (Damage);
      end if;
      for P in 1 .. Board.Panels_Used loop
         Refresh_Panel (P, Damage);
      end loop;
      Dash_Sync.Unlock;
      --  The live dot's pulse.
      if Live_Mode and then Pulse_Phase /= Shown_Pulse then
         Mark (Damage, Live_Dot_Rect);
      end if;
   end Refresh;

   ---------------------------------------------------------------------------
   --  Painting
   ---------------------------------------------------------------------------
   --  Pixel columns of Plot that Damage touches: X0 .. X1 (none if X0 > X1).
   procedure Damaged_Columns (Plot, Damage : Rect; X0, X1 : out Natural);
   procedure Damaged_Columns (Plot, Damage : Rect; X0, X1 : out Natural) is
   begin
      X0 := 1;
      X1 := 0;
      if not Intersects (Plot, Damage) then
         return;
      end if;
      X0 := (if Damage.x > Plot.x then Damage.x - Plot.x else 0);
      X1 := Natural'Min (Plot.w - 1, Damage.x + Damage.w - 1 - Plot.x);
   end Damaged_Columns;

   procedure Draw_Time_Ticks (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; X0, X1 : Natural; Labels : Boolean);
   procedure Draw_Time_Ticks (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; X0, X1 : Natural; Labels : Boolean)
   is
      Port : constant Axis.Viewport := Ports (P);
      Plot : constant Rect := Panels (P).Plot;
      Axis_Box : constant Rect := Panels (P).Bottom_Axis;
      Step : constant Unsigned_64 := Axis.Tick_Step (Port.Width, Tick_Spacing_Pixels);
      T : Unsigned_64 := Axis.First_Tick (Axis.Left_Time (Port), Step);
      Right : constant Unsigned_64 := Axis.Right_Time (Port);
   begin
      while T <= Right loop
         declare
            Col : constant Column_Number := Axis.Column_Of (T, Port.Width);
         begin
            if Axis.Visible (Port, Col) then
               declare
                  X : constant Natural := Axis.X_Of (Port, Col);
               begin
                  if X in X0 .. X1 then
                     Fill_Rect (C, (Plot.x + X, Plot.y, 1, Plot.h), K.Grid_Line);
                  end if;
                  if Labels and then Axis_Box.h > 0 then
                     declare
                        Label : constant String := Format.Clock_Text (T, Step);
                        W : constant Natural := UI_Text_Width (Label);
                        Left : constant Natural := Plot.x + X - Natural'Min (W / 2, Plot.x + X);
                     begin
                        if Left >= Axis_Box.x and then Left + W <= Axis_Box.x + Axis_Box.w then
                           Dash_Labels.Draw (C, Left, Axis_Box.y + M.Space_XS, Label, K.Axis_Text, K.Card);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
         exit when T > Unsigned_64'Last - Step;
         T := T + Step;
      end loop;
   end Draw_Time_Ticks;

   procedure Draw_Value_Ticks
     (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; X0, X1 : Natural; U : Format.Unit; Labels : Boolean);
   procedure Draw_Value_Ticks
     (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; X0, X1 : Natural; U : Format.Unit; Labels : Boolean)
   is
      Plot : constant Rect := Panels (P).Plot;
      Left : constant Rect := Panels (P).Left_Axis;
      Top : constant Unsigned_64 := Unsigned_64'Max (1, Tops (P));
      Step : constant Unsigned_64 := Values.Tick_Step (Values.Clamp (Top), Ticks_For (P));
      V : Unsigned_64 := 0;
   begin
      while V <= Top loop
         declare
            Row : constant Natural := Values.Row_Of (V, Values.Clamp (Top), Natural'Max (1, Plot.h));
         begin
            if X1 >= X0 and then V > 0 then
               Fill_Rect (C, (Plot.x + X0, Plot.y + Row, X1 - X0 + 1, 1), K.Grid_Line);
            end if;
            if Labels and then Left.w > 0 then
               declare
                  Label : constant String := Format.Axis_Text (V, Top, U);
                  Y : constant Natural := Plot.y + Row;
                  Half : constant Natural := UI_Text_Height / 2;
                  Label_Y : constant Natural :=
                    (if Y < Plot.y + Half then Plot.y
                     else Natural'Min (Y - Half, Plot.y + Plot.h - UI_Text_Height));
                  W : constant Natural := UI_Text_Width (Label);
                  Right : constant Natural := Plot.x - M.Space_S;
               begin
                  Dash_Labels.Draw (C, (if Right > W then Right - W else 0), Label_Y, Label, K.Axis_Text, K.Card);
               end;
            end if;
         end;
         exit when V > Unsigned_64'Last - Step;
         V := V + Step;
      end loop;
   end Draw_Value_Ticks;

   --  Dashes start where X + Phase is a multiple of Dash_Period: a plot's
   --  phase follows its columns, so its dashes scroll with the data.
   procedure Draw_Dashed (C : Canvas; X0, X1, Y : Natural; Ink : Color; Phase : Natural := 0);
   procedure Draw_Dashed (C : Canvas; X0, X1, Y : Natural; Ink : Color; Phase : Natural := 0) is
      Offset : constant Natural := (X0 + Phase) mod Dash_Period;
      X : Integer := Integer (X0) - Integer (Offset);
   begin
      while X <= X1 loop
         declare
            A : constant Natural := Natural (Integer'Max (X, Integer (X0)));
            B : constant Natural := Natural (Integer'Max (0, Integer'Min (X + Dash_On - 1, Integer (X1))));
         begin
            if B >= A then
               Fill_Rect (C, (A, Y, B - A + 1, Line_Thickness / 2 + 1), Ink);
            end if;
         end;
         X := X + Dash_Period;
      end loop;
   end Draw_Dashed;

   function Line_Color (K : Colors; Index : Positive) return Color is
     (K.Series (((Index - 1) mod Palette.Series_Slots) + 1));

   procedure Draw_Message (C : Canvas; Area : Rect; K : Colors; Line_1, Line_2 : String);
   procedure Draw_Message (C : Canvas; Area : Rect; K : Colors; Line_1, Line_2 : String) is
      Y : constant Natural := Area.y + (if Area.h > 3 * UI_Text_Height then Area.h / 2 - UI_Text_Height else 0);
   begin
      Text (C, Area.x + (if Area.w > UI_Text_Width (Line_1) then (Area.w - UI_Text_Width (Line_1)) / 2 else 0), Y,
            Fit_UI_Text (Line_1, Area.w), K.Text);
      --  The second line only where it fits.
      if Area.h >= 2 * UI_Text_Height + M.Space_XS then
         Text (C, Area.x + (if Area.w > UI_Text_Width (Line_2) then (Area.w - UI_Text_Width (Line_2)) / 2 else 0),
               Y + UI_Text_Height + M.Space_XS, Fit_UI_Text (Line_2, Area.w), K.Muted);
      end if;
   end Draw_Message;

   --  Why panel P shows nothing, or "" when it has data.
   function Empty_Reason (P : Dash_Model.Panel_Index) return String is
      First, Last : Natural;
      Any_Resolved : Boolean := False;
   begin
      Tracks_Of (P, First, Last);
      for I in First .. Last loop
         if Tracks (I).State = Dash_Tracks.Resolved then
            Any_Resolved := True;
         end if;
      end loop;
      if Any_Resolved or else Last < First then
         return "";
      end if;
      case Tracks (First).State is
         when Dash_Tracks.Refused => return "refused";
         when Dash_Tracks.Unavailable => return "unavailable";
         when others => return "missing";
      end case;
   end Empty_Reason;

   procedure Draw_Empty (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; Reason : String);
   procedure Draw_Empty (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; Reason : String) is
      First, Last : Natural;
   begin
      Tracks_Of (P, First, Last);
      declare
         Name : constant String := (if Last >= First then Dash_Model.Image (Tracks (First).Spec.Name) else "");
      begin
         if Reason = "refused" then
            Draw_Message (C, Panels (P).Body_Area, K, "Not in this app's metrics scope", Name);
         elsif Reason = "unavailable" then
            Draw_Message (C, Panels (P).Body_Area, K, "metrics.svc is not answering", Name);
         else
            Draw_Message (C, Panels (P).Body_Area, K,
                          (if Panels (P).Body_Area.h >= 2 * UI_Text_Height + M.Space_XS
                           then "Waiting for the first sample" else "Waiting for " & Name), Name);
         end if;
      end;
   end Draw_Empty;

   --  The selection or drill-down slice behind the data.
   procedure Draw_Slices (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; X0, X1 : Natural;
                          Outline : Boolean := False);
   procedure Draw_Slices (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; X0, X1 : Natural;
                          Outline : Boolean := False) is
      Port : constant Axis.Viewport := Ports (P);
      Plot : constant Rect := Panels (P).Plot;
      procedure Band (From, To : Unsigned_64; Ink : Color) is
         A : constant Column_Number := Axis.Column_Of (Unsigned_64'Min (From, To), Port.Width);
         B : constant Column_Number := Axis.Column_Of (Unsigned_64'Max (From, To), Port.Width);
         Lo : constant Column_Number := Unsigned_64'Max (A, Axis.First (Port));
         Hi : constant Column_Number := Unsigned_64'Min (B, Port.Last);
      begin
         if Lo <= Hi and then Axis.Visible (Port, Lo) and then Axis.Visible (Port, Hi) then
            declare
               L : constant Natural := Natural'Max (Axis.X_Of (Port, Lo), X0);
               R : constant Natural := Natural'Min (Axis.X_Of (Port, Hi), X1);
            begin
               if R >= L and then not Outline then
                  Fill_Rect (C, (Plot.x + L, Plot.y, R - L + 1, Plot.h), Ink);
               elsif R >= L then
                  --  Over cells: the slice's edges only.
                  Fill_Rect (C, (Plot.x + L, Plot.y, 1, Plot.h), K.Accent);
                  Fill_Rect (C, (Plot.x + R, Plot.y, 1, Plot.h), K.Accent);
               end if;
            end;
         end if;
      end Band;
   begin
      if Selecting then
         Band (Select_From, Select_To, Palette.Mix (K.Selection, K.Card, Selection_Tint));
      end if;
      if Detail_Valid then
         Band (Detail_From, Detail_To, Palette.Mix (K.Accent, K.Card, Selection_Tint));
      end if;
   end Draw_Slices;

   procedure Draw_Crosshair (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; X0, X1 : Natural);
   procedure Draw_Crosshair (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; X0, X1 : Natural) is
      Port : constant Axis.Viewport := Ports (P);
      Plot : constant Rect := Panels (P).Plot;
      Col : constant Column_Number := Axis.Column_Of (Hover_Time, Port.Width);
   begin
      if Hover_Valid and then P = Hover_Panel and then not Dragged and then Axis.Visible (Port, Col) then
         declare
            X : constant Natural := Axis.X_Of (Port, Col);
         begin
            if X in X0 .. X1 then
               Fill_Rect (C, (Plot.x + X, Plot.y, 1, Plot.h), Palette.Mix (K.Text, K.Card, Crosshair_Tint));
            end if;
         end;
      end if;
   end Draw_Crosshair;

   procedure Draw_Time_Series (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; Damage : Rect);
   procedure Draw_Time_Series (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; Damage : Rect) is
      Item : constant Dash_Model.Panel := Panel_Of (P);
      Port : constant Axis.Viewport := Ports (P);
      Plot : constant Rect := Panels (P).Plot;
      Top : constant Values.Value := Values.Clamp (Unsigned_64'Max (1, Tops (P)));
      Rows : constant Values.Row_Count := Natural'Max (1, Natural'Min (Plot.h, Values.Maximum_Rows));
      Bin : constant Unsigned_64 := Unsigned_64 (Cell_Columns (P));
      First, Last : Natural;
      X0, X1 : Natural;
      Line_Index : Natural := 0;
      U : Format.Unit := Format.Microseconds;

      --  The bins (Bin columns, aligned to absolute columns) drawn: those
      --  overlapping X0 .. X1, and one more each side for the joins.
      Low_Bin, High_Bin : Column_Number := 1;
      type Bin_Summaries is array (Natural range <>) of Columns.Summary;

      procedure Gather (T : Dash_Tracks.Track; Into : out Bin_Summaries) is
         Run : Columns.Summary_Array (1 .. Into'Length * Natural (Bin));
      begin
         Columns.Copy_Run (T.Columns.all, Low_Bin * Bin, Run);
         for B in Into'Range loop
            declare
               S : Columns.Summary := Columns.Nothing;
               Base : constant Natural := (B - Into'First) * Natural (Bin);
            begin
               for K in 1 .. Natural (Bin) loop
                  if Run (Base + K).Origin /= Columns.Empty then
                     S := Columns.Merge (S, Run (Base + K));
                  end if;
               end loop;
               Into (B) := S;
            end;
         end loop;
      end Gather;

      --  The pixel column of bin B's centre (clamped into the plot).
      function Center_Of (B : Column_Number) return Integer is
         Middle : constant Column_Number := B * Bin + Bin / 2;
      begin
         return Integer (Plot.w - 1) - Integer (Long_Long_Integer (Port.Last) - Long_Long_Integer (Middle));
      end Center_Of;

      procedure Segment (Xa, Ya, Xb, Yb : Integer; Ink : Color) is
         Previous : Integer := Ya;
      begin
         for X in Xa .. Xb loop
            declare
               Y : constant Integer :=
                 (if Xb = Xa then Yb else Ya + (Yb - Ya) * (X - Xa) / (Xb - Xa));
               From : constant Integer := Integer'Min (Previous, Y);
               To : constant Integer := Integer'Max (Previous, Y);
            begin
               if X in X0 .. X1 and then X >= 0 then
                  Dash_Pixels.Span (C, Plot.x + X, Plot.y + Natural (From),
                                    Natural'Min (Natural (To - From) + Line_Thickness, Plot.h - Natural (From)), Ink);
               end if;
               Previous := Y;
            end;
         end loop;
      end Segment;
   begin
      Tracks_Of (P, First, Last);
      if Last >= First then
         U := Unit_Of (Tracks (First).all);
      end if;
      Damaged_Columns (Plot, Damage, X0, X1);
      Draw_Value_Ticks (C, P, K, X0, X1, U, Intersects (Panels (P).Left_Axis, Damage));
      if X1 < X0 then
         Draw_Time_Ticks (C, P, K, 1, 0, Intersects (Panels (P).Bottom_Axis, Damage));
         return;
      end if;
      Draw_Slices (C, P, K, X0, X1);
      Draw_Time_Ticks (C, P, K, X0, X1, Intersects (Panels (P).Bottom_Axis, Damage));
      Fill_Rect (C, (Plot.x + X0, Plot.y + Plot.h - 1, X1 - X0 + 1, 1), K.Edge);
      declare
         Left_Column : constant Column_Number :=
           (if Axis.Has_Column_At (Port, X0) then Axis.Column_At (Port, X0) else Axis.First (Port));
         Right_Column : constant Column_Number :=
           (if Axis.Has_Column_At (Port, X1) then Axis.Column_At (Port, X1) else Port.Last);
      begin
         Low_Bin := (if Left_Column / Bin > 0 then Left_Column / Bin - 1 else 0);
         High_Bin := Right_Column / Bin + 1;
      end;
      for I in First .. Last loop
         declare
            T : Dash_Tracks.Track renames Tracks (I).all;
            Bins : Bin_Summaries (0 .. Natural (High_Bin - Low_Bin));
         begin
            if T.State = Dash_Tracks.Resolved and then T.Columns /= null then
               Gather (T, Bins);
               --  The first series' min .. max band, per bin.
               if Item.Band and then I = First then
                  declare
                     Ink : constant Color := Palette.Mix (Line_Color (K, 1), K.Card, Band_Tint);
                  begin
                     for B in Bins'Range loop
                        if Bins (B).Origin /= Columns.Empty and then Bins (B).Maximum > Bins (B).Minimum then
                           declare
                              Y_High : constant Natural := Values.Row_Of (Bins (B).Maximum, Top, Rows);
                              Y_Low : constant Natural := Values.Row_Of (Bins (B).Minimum, Top, Rows);
                              Center : constant Integer := Center_Of (Low_Bin + Unsigned_64 (B));
                              Half : constant Integer := Integer (Bin / 2);
                           begin
                              for X in Integer'Max (Center - Half, Integer (X0)) ..
                                       Integer'Min (Center + Half, Integer (X1))
                              loop
                                 Dash_Pixels.Span (C, Plot.x + Natural (X), Plot.y + Y_High, Y_Low - Y_High + 1, Ink);
                              end loop;
                           end;
                        end if;
                     end loop;
                  end;
               end if;
               for Stat in Dash_Model.Statistic loop
                  if Item.Lines (Stat) then
                     Line_Index := Line_Index + 1;
                     declare
                        Ink : constant Color := Line_Color (K, Line_Index);
                        Has_Previous : Boolean := False;
                        Previous_X, Previous_Y : Integer := 0;
                     begin
                        for B in Bins'Range loop
                           if Bins (B).Origin /= Columns.Empty then
                              declare
                                 X : constant Integer := Center_Of (Low_Bin + Unsigned_64 (B));
                                 Y : constant Integer := Values.Row_Of (Value_Of (Bins (B), Stat, Port.Width * Bin),
                                                                        Top, Rows);
                                 Joined : constant Boolean := Has_Previous and then B > Bins'First
                                   and then Bins (B - 1).Origin /= Columns.Empty;
                              begin
                                 if Joined then
                                    Segment (Previous_X, Previous_Y, X, Y, Ink);
                                 elsif B = Bins'Last or else Bins (B + 1).Origin = Columns.Empty then
                                    --  Alone between gaps: a dot.
                                    for Dot_X in X - 1 .. X + 1 loop
                                       if Dot_X >= 0 and then Dot_X in Integer (X0) .. Integer (X1) then
                                          Dash_Pixels.Span (C, Plot.x + Natural (Dot_X), Plot.y + Natural'Max (Y, 1) - 1,
                                                            Natural'Min (Dot_Size, Plot.h - Natural'Max (Y, 1) + 1), Ink);
                                       end if;
                                    end loop;
                                 end if;
                                 Has_Previous := True;
                                 Previous_X := X;
                                 Previous_Y := Y;
                              end;
                           else
                              Has_Previous := False;
                           end if;
                        end loop;
                     end;
                  end if;
               end loop;
            end if;
         end;
      end loop;
      if Item.Budget > 0 and then Item.Budget <= Top then
         Draw_Dashed (C, Plot.x + X0, Plot.x + X1, Plot.y + Values.Row_Of (Item.Budget, Top, Rows), Palette.Critical,
                      Phase => Natural ((Port.Last + Unsigned_64 (Dash_Period) * Unsigned_64 (Plot.w)
                                         - Unsigned_64 (Plot.x) - Unsigned_64 (Plot.w - 1)) mod Dash_Period));
      end if;
      Draw_Crosshair (C, P, K, X0, X1);
   end Draw_Time_Series;

   --  A latency heatmap reads on a log scale: V's place between the
   --  smallest and the largest value in view, by octave (eighths of one).
   function Octave_Eighths (X : Unsigned_64) return Unsigned_64 is
      N : Unsigned_64 := 0;
      Y : Unsigned_64 := Unsigned_64'Max (X, 1);
      Eighths_Per_Octave : constant := 8;
      Mantissa_Shift : constant := 3;
   begin
      while Y >= 2 ** (Mantissa_Shift + 1) loop
         Y := Y / 2;
         N := N + 1;
      end loop;
      --  Y in 1 .. 15: its top bits give the fraction of the octave.
      return N * Eighths_Per_Octave + (if Y >= 2 ** Mantissa_Shift then Y - 2 ** Mantissa_Shift else 0);
   end Octave_Eighths;

   function Heat_Step (P : Dash_Model.Panel_Index; V : Unsigned_64) return Palette.Ramp_Index is
      Low : constant Unsigned_64 := Octave_Eighths (Unsigned_64'Min (Lows (P), Tops (P)));
      High : constant Unsigned_64 := Octave_Eighths (Tops (P));
      Here : constant Unsigned_64 := Octave_Eighths (V);
   begin
      if High <= Low then
         return Palette.Ramp_Steps;
      end if;
      return Palette.Step_For ((if Here > Low then Here - Low else 0), High - Low);
   end Heat_Step;

   procedure Draw_Heatmap (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; Damage : Rect);
   procedure Draw_Heatmap (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; Damage : Rect) is
      Item : constant Dash_Model.Panel := Panel_Of (P);
      Port : constant Axis.Viewport := Ports (P);
      Plot : constant Rect := Panels (P).Plot;
      Labels : constant Rect := Panels (P).Left_Axis;
      Cell : constant Unsigned_64 := Unsigned_64 (Cell_Columns (P));
      First, Last : Natural;
      X0, X1 : Natural;
      Row_Count : Natural := 0;
      Run : Columns.Summary_Array (1 .. Port.Columns);

      --  The statistic over a cell's columns, merged as the statistic merges.
      procedure Cell_Value (From, To : Natural; V : out Unsigned_64; Any : out Boolean) is
         Most : Unsigned_64 := 0;
      begin
         V := 0;
         Any := False;
         for X in From .. To loop
            declare
               S : Columns.Summary renames Run (X + 1);
            begin
               if S.Origin /= Columns.Empty then
                  declare
                     W : constant Unsigned_64 := Value_Of (S, Item.Shown, Port.Width);
                  begin
                     case Item.Shown is
                        when Dash_Model.P99 | Dash_Model.Maximum | Dash_Model.Last =>
                           V := (if Any then Unsigned_64'Max (V, W) else W);
                        when Dash_Model.Minimum =>
                           V := (if Any then Unsigned_64'Min (V, W) else W);
                        when Dash_Model.Count | Dash_Model.Rate =>
                           V := V + W;
                        when Dash_Model.P50 | Dash_Model.Mean =>
                           --  The value of the column with the most samples.
                           if S.Count >= Most then
                              Most := S.Count;
                              V := W;
                           end if;
                     end case;
                     Any := True;
                  end;
               end if;
            end;
         end loop;
      end Cell_Value;
   begin
      Tracks_Of (P, First, Last);
      for I in First .. Last loop
         if Tracks (I).State = Dash_Tracks.Resolved or else Tracks (I).Match_Index = 1 then
            Row_Count := Row_Count + 1;
         end if;
      end loop;
      Damaged_Columns (Plot, Damage, X0, X1);
      Draw_Time_Ticks (C, P, K, 1, 0, Intersects (Panels (P).Bottom_Axis, Damage));
      if Row_Count = 0 then
         return;
      end if;
      declare
         --  Every drawn row is a labelled line tall; rows that do not fit
         --  are left out and counted under the labels (never a row whose
         --  label is missing).
         Shown_Rows : constant Positive := Natural'Max (1, Natural'Min (Row_Count, Plot.h / UI_Text_Height));
         Row_H : constant Natural := Natural'Max (1, Plot.h / Shown_Rows);
         Row : Natural := 0;
         --  Whole cells, from the first damaged column's cell.
         Cell_X0 : Natural := X0;
      begin
         if X1 >= X0 and then Axis.Has_Column_At (Port, X0) then
            declare
               C0 : constant Column_Number := Axis.Column_At (Port, X0) / Cell * Cell;
            begin
               Cell_X0 := (if Axis.Visible (Port, C0) then Axis.X_Of (Port, C0) else 0);
            end;
         end if;
         for I in First .. Last loop
            declare
               T : Dash_Tracks.Track renames Tracks (I).all;
            begin
               if (T.State = Dash_Tracks.Resolved or else T.Match_Index = 1) and then Row < Shown_Rows then
                  declare
                     Y : constant Natural := Plot.y + Row * Row_H;
                     Cell_H : constant Natural := (if Row_H > 2 then Row_H - 1 else Row_H);
                  begin
                     if Intersects (Labels, Damage) then
                        Text_Right (C, Labels.x + Labels.w - M.Space_M,
                                    Y + (if Row_H > UI_Text_Height then (Row_H - UI_Text_Height) / 2 else 0),
                                    Fit_UI_Text (Series_Label (T), Labels.w - M.Space_M),
                                    (if T.State = Dash_Tracks.Resolved then K.Muted else K.Grid_Line));
                     end if;
                     if T.State = Dash_Tracks.Resolved and then T.Columns /= null and then X1 >= X0 then
                        Copy_View (T, Port, Run);
                        declare
                           X : Natural := Cell_X0;
                        begin
                           while X <= X1 loop
                              declare
                                 --  The cell ends where the next one starts.
                                 Width : Natural := 1;
                                 V : Unsigned_64;
                                 Any : Boolean;
                              begin
                                 while X + Width <= Plot.w - 1 and then
                                   (not Axis.Has_Column_At (Port, X + Width) or else
                                    Axis.Column_At (Port, X + Width) mod Cell /= 0)
                                 loop
                                    Width := Width + 1;
                                 end loop;
                                 Cell_Value (X, X + Width - 1, V, Any);
                                 if Any then
                                    Fill_Rect (C, (Plot.x + X, Y, Width, Cell_H), K.Ramp (Heat_Step (P, V)));
                                    if Item.Budget > 0 and then V > Item.Budget then
                                       --  Over budget: a red edge; the value keeps its colour.
                                       Fill_Rect (C, (Plot.x + X, Y, Width, Natural'Min (Over_Edge, Cell_H)),
                                                  Palette.Critical);
                                    end if;
                                 end if;
                                 X := X + Width;
                              end;
                           end loop;
                        end;
                     end if;
                  end;
                  Row := Row + 1;
               end if;
            end;
         end loop;
      end;
      if Row_Count > Natural'Max (1, Natural'Min (Row_Count, Plot.h / UI_Text_Height))
        and then Intersects (Labels, Damage)
      then
         Text (C, Labels.x, Plot.y + Plot.h + M.Space_XS,
               Fit_UI_Text ("+" & Format.Decimal (Unsigned_64 (Row_Count - Plot.h / UI_Text_Height))
                            & " rows", Labels.w), K.Muted);
      end if;
      Draw_Slices (C, P, K, X0, X1, Outline => True);
      Draw_Crosshair (C, P, K, X0, X1);
   end Draw_Heatmap;

   procedure Draw_Histogram (C : Canvas; P : Dash_Model.Panel_Index; K : Colors);
   procedure Draw_Histogram (C : Canvas; P : Dash_Model.Panel_Index; K : Colors) is
      Item : constant Dash_Model.Panel := Panel_Of (P);
      Port : constant Axis.Viewport := Ports (P);
      Plot : constant Rect := Panels (P).Plot;
      Below : constant Rect := Panels (P).Bottom_Axis;
      First, Last : Natural;
      Totals : Charts_Buckets.Totals := [others => 0];
      High : Unsigned_64 := 1;
      Count : constant Charts_Buckets.Bucket_Count := Item.Buckets.Count;
   begin
      Tracks_Of (P, First, Last);
      Fill_Rect (C, Add_Rect (Plot, Below), K.Card);
      for I in First .. Last loop
         if Tracks (I).Buckets /= null and then Tracks (I).State = Dash_Tracks.Resolved then
            declare
               Part : Charts_Buckets.Totals;
            begin
               Charts_Buckets.Sum (Tracks (I).Buckets.all, Axis.First (Port), Port.Last, Part);
               for B in 0 .. Count - 1 loop
                  Totals (B) := Columns.Saturating_Add (Totals (B), Part (B));
               end loop;
            end;
         end if;
      end loop;
      for B in 0 .. Count - 1 loop
         High := Unsigned_64'Max (High, Totals (B));
      end loop;
      declare
         Top : constant Values.Value := Values.Axis_Top (Values.Clamp (High), Ticks_For (P));
         Rows : constant Values.Row_Count := Natural'Max (1, Natural'Min (Plot.h, Values.Maximum_Rows));
         Slot_W : constant Natural := Plot.w / Count;
         Total : Unsigned_64 := 0;
      begin
         Tops (P) := Top;
         Draw_Value_Ticks (C, P, K, 0, Plot.w - 1, Format.Count, True);
         Fill_Rect (C, (Plot.x, Plot.y + Plot.h - 1, Plot.w, 1), K.Edge);
         for B in 0 .. Count - 1 loop
            Total := Total + Totals (B);
            declare
               X : constant Natural := Plot.x + B * Slot_W + Bar_Gap;
               W : constant Natural := (if Slot_W > 2 * Bar_Gap then Slot_W - 2 * Bar_Gap else 1);
               Y : constant Natural := Values.Row_Of (Totals (B), Top, Rows);
               Edge : constant Unsigned_64 := Charts_Buckets.Lower_Edge (Item.Buckets, B);
               Over : constant Boolean := Item.Budget > 0 and then Edge >= Item.Budget;
               Ink : constant Color :=
                 (if Over then Palette.Mix (Palette.Warning, Line_Color (K, 1), Late_Tint) else Line_Color (K, 1));
               Label : constant String :=
                 Format.Decimal (Edge) & (if B = Count - 1 then "+" else "");
               Count_Label : constant String := Format.Value_Text (Totals (B), Format.Count);
            begin
               if Totals (B) > 0 then
                  Fill_Rect (C, (X, Plot.y + Y, W, Plot.h - Y - 1), Ink);
                  if Y >= UI_Text_Height then
                     Text (C, X + (if W > UI_Text_Width (Count_Label) then (W - UI_Text_Width (Count_Label)) / 2
                                   else 0), Plot.y + Y - UI_Text_Height, Count_Label, K.Text);
                  end if;
               end if;
               Text (C, X + (if W > UI_Text_Width (Label) then (W - UI_Text_Width (Label)) / 2 else 0),
                     Below.y + M.Space_XS, Label, K.Axis_Text);
            end;
         end loop;
         if Total = 0 then
            Draw_Message (C, Plot, K, "No samples in view", "");
         end if;
      end;
   end Draw_Histogram;

   procedure Draw_Stat (C : Canvas; P : Dash_Model.Panel_Index; K : Colors);
   procedure Draw_Stat (C : Canvas; P : Dash_Model.Panel_Index; K : Colors) is
      Item : constant Dash_Model.Panel := Panel_Of (P);
      G : constant Panel_Geometry := Panels (P);
      Port : constant Axis.Viewport := Ports (P);
      First, Last : Natural;
      function Shown_Value (S : Dash_Feeds.Window_Stats; Counter : Boolean) return Unsigned_64 is
        (case Item.Shown is
           when Dash_Model.Minimum => S.Minimum, when Dash_Model.P50 => S.P50, when Dash_Model.P99 => S.P99,
           when Dash_Model.Maximum => S.Maximum,
           when Dash_Model.Mean => (if S.Count = 0 then 0 else S.Sum / S.Count),
           when Dash_Model.Count => (if Counter then S.Sum else S.Count),
           when Dash_Model.Rate => S.Rate_Milli / Microseconds_Per_Millisecond,
           when Dash_Model.Last => S.Last);
   begin
      Tracks_Of (P, First, Last);
      if Last < First then
         return;
      end if;
      declare
         T : Dash_Tracks.Track renames Tracks (First).all;
         S : constant Dash_Feeds.Window_Stats := T.Stats;
         Counter : constant Boolean := T.Info.Kind = Dash_Feeds.Counter;
         Counting : constant Boolean := Item.Shown in Dash_Model.Count | Dash_Model.Rate;
         V : constant Unsigned_64 := Shown_Value (S, Counter);
         U : constant Format.Unit := (if Counting then Format.Count else T.Info.Unit);
         Has_Value : constant Boolean := T.Stats_Valid and then (S.Count > 0 or else Counting);
         Shown : constant String := (if Has_Value then Format.Value_Text (V, U) else "--");
         Value_Y : constant Natural := G.Body_Area.y;
         Note_Y : constant Natural := Value_Y + Styled_Line_Height (Emphasis_Face, Heading_Size);
         Spark : constant Rect := G.Plot;
         Spark_Ink : constant Color := Line_Color (K, 1);
         --  Scaled to the window's p99, not its maximum: one spike is clipped
         --  (and marked) instead of flattening the rest.
         Top : constant Values.Value := Values.Clamp (Unsigned_64'Max (1,
           (if T.Stats_Valid and then not Counting and then T.Stats.P99 > 0 then T.Stats.P99 else Tops (P))));
         Text_Room : constant Natural :=
           (if Spark.x > G.Body_Area.x + M.Space_M then Spark.x - G.Body_Area.x - M.Space_M else G.Body_Area.w);
         Compact : constant Boolean := G.Body_Area.h < Styled_Line_Height (Emphasis_Face, Heading_Size);
      begin
         if Compact then
            Draw_Emphasis_Text_Transparent
              (C, G.Body_Area.x,
               G.Body_Area.y + (if G.Body_Area.h > UI_Text_Height then (G.Body_Area.h - UI_Text_Height) / 2 else 0),
               Shown, K.Text);
         else
            Draw_Styled_Text_Transparent (C, G.Body_Area.x, Value_Y, Shown, K.Text, Emphasis_Face, Heading_Size);
         end if;
         --  Against the window before: the change, muted (and which way is
         --  worse is the budget's to say, not a colour's).
         if not Compact and then Has_Value and then T.Previous_Valid and then (T.Previous.Count > 0 or else Counting)
         then
            declare
               Before : constant Unsigned_64 := Shown_Value (T.Previous, Counter);
               Percent : constant Unsigned_64 :=
                 (if Before = 0 then 0
                  else (if V >= Before then V - Before else Before - V) * Palette.Percent / Before);
               Change : constant String :=
                 (if Before = 0 then "new"
                  elsif Percent = 0 then "same"
                  else (if V >= Before then "+" else "-") & Format.Decimal (Percent) & "%");
               Value_W : constant Natural :=
                 Styled_Text_Width (Shown, Emphasis_Face, Heading_Size) + M.Space_M;
            begin
               if Text_Room > Value_W + UI_Text_Width (Change) then
                  Text (C, G.Body_Area.x + Value_W,
                        Value_Y + Styled_Baseline (Emphasis_Face, Heading_Size) - Styled_Baseline (Interface_Face),
                        Change, K.Muted);
               end if;
            end;
         end if;
         if Compact then
            null;
         elsif Item.Budget > 0 and then Has_Value then
            declare
               Over : constant Boolean := V > Item.Budget;
               Note : constant String :=
                 (if Over then "over the " else "within the ") & Format.Value_Text (Item.Budget, U) & " budget";
            begin
               Fill_Rect (C, (G.Body_Area.x, Note_Y + (UI_Text_Height - Status_Dot) / 2, Status_Dot, Status_Dot),
                          (if Over then Palette.Critical else Palette.Good));
               Text (C, G.Body_Area.x + Status_Dot + M.Space_S, Note_Y, Note, K.Muted);
            end;
         elsif T.State /= Dash_Tracks.Resolved then
            Text (C, G.Body_Area.x, Note_Y, Fit_UI_Text ("waiting for " & Dash_Model.Image (T.Spec.Name),
                                                          G.Body_Area.w), K.Muted);
         else
            Text (C, G.Body_Area.x, Note_Y, "in the last " & Format.Span_Text (Span), K.Muted);
         end if;
         --  The sparkline in its lane: the statistic per column, as an area.
         if T.State = Dash_Tracks.Resolved and then T.Columns /= null and then Spark.h > 1 then
            declare
               Rows : constant Values.Row_Count := Natural'Max (1, Natural'Min (Spark.h, Values.Maximum_Rows));
               Area_Ink : constant Color := Palette.Mix (Spark_Ink, K.Card, Area_Tint);
               Run : Columns.Summary_Array (1 .. Port.Columns);
               Bin : constant Positive := Cell_Columns (P);
               X : Natural := 0;
            begin
               Fill_Rect (C, (Spark.x, Spark.y + Spark.h - 1, Spark.w, 1), K.Grid_Line);
               Copy_View (T, Port, Run);
               --  Fewer than Minimum_Spark_Points bins with data read as a
               --  stray dot: the lane stays empty until there is a shape.
               declare
                  Filled : Natural := 0;
                  Bin_X : Natural := 0;
               begin
                  while Bin_X < Natural'Min (Spark.w, Port.Columns) loop
                     for K2 in Bin_X .. Natural'Min (Bin_X + Bin, Natural'Min (Spark.w, Port.Columns)) - 1 loop
                        if Run (K2 + 1).Origin /= Columns.Empty then
                           Filled := Filled + 1;
                           exit;
                        end if;
                     end loop;
                     Bin_X := Bin_X + Bin;
                  end loop;
                  if Filled < Minimum_Spark_Points then
                     X := Spark.w;
                  end if;
               end;
               --  Bins as the plots use, so a sparse series is a shape, not dust.
               while X < Natural'Min (Spark.w, Port.Columns) loop
                  declare
                     Width : constant Natural := Natural'Min (Bin, Natural'Min (Spark.w, Port.Columns) - X);
                     S : Columns.Summary := Columns.Nothing;
                  begin
                     for K2 in X .. X + Width - 1 loop
                        if Run (K2 + 1).Origin /= Columns.Empty then
                           S := Columns.Merge (S, Run (K2 + 1));
                        end if;
                     end loop;
                     if S.Origin /= Columns.Empty then
                        declare
                           Y : constant Natural := Values.Row_Of (Value_Of (S, Item.Shown, Port.Width), Top, Rows);
                        begin
                           for Column_X in X .. X + Width - 1 loop
                              Dash_Pixels.Span (C, Spark.x + Column_X, Spark.y + Y, Spark.h - Y, Area_Ink);
                              Dash_Pixels.Span (C, Spark.x + Column_X, Spark.y + Y,
                                                Natural'Min (Line_Thickness, Spark.h - Y), Spark_Ink);
                           end loop;
                        end;
                     end if;
                     X := X + Width;
                  end;
               end loop;
            end;
         end if;
      end;
   end Draw_Stat;

   procedure Draw_Legend (C : Canvas; P : Dash_Model.Panel_Index; K : Colors);
   procedure Draw_Legend (C : Canvas; P : Dash_Model.Panel_Index; K : Colors) is
      Item : constant Dash_Model.Panel := Panel_Of (P);
      H : constant Rect := Panels (P).Header;
      First, Last : Natural;
      --  Left of the source button.
      Right : Natural := (if H.w > Source_Button_Width + M.Space_S then H.x + H.w - Source_Button_Width - M.Space_S
                          else H.x);
      Line_Index : Natural := 0;
      Y : constant Natural := H.y + (if H.h > UI_Text_Height then (H.h - UI_Text_Height) / 2 else 0);
      type Entry_Text is record
         Label : String (1 .. Dash_Model.Maximum_Text + 8);
         Length : Natural;
         Ink : Color;
      end record;
      Entries : array (1 .. Palette.Series_Slots) of Entry_Text;
      Used : Natural := 0;
   begin
      Tracks_Of (P, First, Last);
      case Item.Kind is
         when Dash_Model.Time_Series =>
            for I in First .. Last loop
               for Stat in Dash_Model.Statistic loop
                  if Item.Lines (Stat) then
                     Line_Index := Line_Index + 1;
                     if Used < Entries'Last then
                        declare
                           L : constant String := Series_Label (Tracks (I).all) & " " & Statistic_Name (Stat);
                           N : constant Natural := Natural'Min (L'Length, Entries (1).Label'Length);
                        begin
                           Used := Used + 1;
                           Entries (Used).Label (1 .. N) := L (L'First .. L'First + N - 1);
                           Entries (Used).Length := N;
                           Entries (Used).Ink := Line_Color (K, Line_Index);
                        end;
                     end if;
                  end if;
               end loop;
            end loop;
            for E in reverse 1 .. Used loop
               declare
                  L : constant String := Entries (E).Label (1 .. Entries (E).Length);
                  W : constant Natural := UI_Text_Width (L);
               begin
                  exit when Right < H.x + W + Legend_Swatch + 2 * Legend_Gap + UI_Text_Width (Dash_Model.Image
                                                                                             (Item.Title));
                  Right := Right - W;
                  Text (C, Right, Y, L, K.Muted);
                  Right := Right - M.Space_S - Legend_Swatch;
                  Fill_Rect (C, (Right, Y + UI_Text_Height / 2 - 1, Legend_Swatch, Line_Thickness + 1), Entries (E).Ink);
                  Right := Right - Legend_Gap;
               end;
            end loop;
            if Item.Budget > 0 then
               declare
                  L : constant String := "budget " & Format.Value_Text (Item.Budget, Format.Microseconds);
                  W : constant Natural := UI_Text_Width (L);
               begin
                  if Right > H.x + W + Legend_Swatch + Legend_Gap + UI_Text_Width (Dash_Model.Image (Item.Title)) then
                     Right := Right - W;
                     Text (C, Right, Y, L, K.Muted);
                     Right := Right - M.Space_S - Legend_Swatch;
                     Draw_Dashed (C, Right, Right + Legend_Swatch - 1, Y + UI_Text_Height / 2 - 1, Palette.Critical);
                  end if;
               end;
            end if;
         when Dash_Model.Heatmap =>
            --  The scale: the ramp, low to high, and the over-budget colour.
            declare
               Step_W : constant := M.Space_M + M.Space_XS;
               Strip : constant Natural := Palette.Ramp_Steps * Step_W;
               Top_Label : constant String := Format.Value_Text (Tops (P), (if Last >= First then
                                                                  Unit_Of (Tracks (First).all) else Format.Microseconds));
               Over_Label : constant String := "over " & Format.Value_Text (Item.Budget, Format.Microseconds);
               X : Natural;
            begin
               if Item.Budget > 0 then
                  Right := Right - UI_Text_Width (Over_Label);
                  Text (C, Right, Y, Over_Label, K.Muted);
                  Right := Right - M.Space_S - Step_W;
                  Fill_Rect (C, (Right, Y + 3, Step_W, UI_Text_Height - 6), Palette.Critical);
                  Right := Right - Legend_Gap;
               end if;
               Right := Right - UI_Text_Width (Top_Label);
               Text (C, Right, Y, Top_Label, K.Muted);
               Right := Right - M.Space_S - Strip;
               X := Right;
               for I in Palette.Ramp_Index loop
                  Fill_Rect (C, (X, Y + 3, Step_W, UI_Text_Height - 6), K.Ramp (I));
                  X := X + Step_W;
               end loop;
               declare
                  Low_Label : constant String :=
                    Format.Value_Text (Unsigned_64'Min (Lows (P), Tops (P)), (if Last >= First then Unit_Of (Tracks (First).all)
                                                                               else Format.Microseconds));
               begin
                  Right := Right - M.Space_S - UI_Text_Width (Low_Label);
                  Text (C, Right, Y, Low_Label, K.Muted);
               end;
            end;
         when Dash_Model.Histogram =>
            if Last >= First and then Tracks (First).Stats_Valid then
               declare
                  L : constant String := "n = " & Format.Value_Text (Tracks (First).Stats.Count, Format.Count);
               begin
                  Right := Right - UI_Text_Width (L);
                  Text (C, Right, Y, L, K.Muted);
                  Right := Right - Legend_Gap;
               end;
            end if;
            if Item.Budget > 0 then
               declare
                  L : constant String := Format.Decimal (Item.Budget) & " or more";
               begin
                  if Right > H.x + UI_Text_Width (L) + Legend_Swatch + Legend_Gap
                    + Emphasis_Text_Width (Dash_Model.Image (Item.Title))
                  then
                     Right := Right - UI_Text_Width (L);
                     Text (C, Right, Y, L, K.Muted);
                     Right := Right - M.Space_S - Legend_Swatch;
                     Fill_Rect (C, (Right, Y + 3, Legend_Swatch, UI_Text_Height - 6),
                                Palette.Mix (Palette.Warning, Line_Color (K, 1), Late_Tint));
                  end if;
               end;
            end if;
         when others => null;
      end case;
   end Draw_Legend;

   --  The source button: a chevron at the header's right end (the
   --  accent while its picker is open).
   function Source_Button (P : Dash_Model.Panel_Index) return Rect is
      H : constant Rect := Panels (P).Header;
      W : constant Natural := Natural'Min (Source_Button_Width, H.w);
   begin
      return (H.x + H.w - W, H.y, W, H.h);
   end Source_Button;

   procedure Draw_Source_Button (C : Canvas; P : Dash_Model.Panel_Index; K : Colors);
   procedure Draw_Source_Button (C : Canvas; P : Dash_Model.Panel_Index; K : Colors) is
      B : constant Rect := Source_Button (P);
      Open : constant Boolean := Picker_On and then Picker_Target = P;
      Rows : constant Natural := Chevron_Width / 2;
      X : constant Natural := B.x + (if B.w > Chevron_Width then (B.w - Chevron_Width) / 2 else 0);
      Y : constant Natural := B.y + (if B.h > Rows then (B.h - Rows) / 2 else 0);
      Ink : constant Color := (if Open then K.Accent else K.Muted);
   begin
      if Open then
         Fill_Rect (C, B, Palette.Mix (K.Accent, K.Card, Selection_Tint));
      end if;
      --  A downward triangle, a row at a time.
      for Row in 0 .. Rows - 1 loop
         Fill_Rect (C, (X + Row, Y + Row, Chevron_Width - 2 * Row, 1), Ink);
      end loop;
   end Draw_Source_Button;

   procedure Draw_Panel (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; Damage : Rect);
   procedure Draw_Panel (C : Canvas; P : Dash_Model.Panel_Index; K : Colors; Damage : Rect) is
      Item : constant Dash_Model.Panel := Panel_Of (P);
      G : constant Panel_Geometry := Panels (P);
      Reason : constant String := Empty_Reason (P);
   begin
      if not Intersects (G.Outer, Damage) then
         return;
      end if;
      --  The card once (the canvas clips it to the damage); everything else
      --  is drawn over it, never under another fill.
      Fill_Rect (C, G.Outer, K.Card);
      Stroke_Rect (C, G.Outer, K.Edge, K.Edge);
      if Intersects (Damage, G.Header) then
         Strong (C, G.Header.x, G.Header.y + (if G.Header.h > UI_Text_Height then (G.Header.h - UI_Text_Height) / 2
                                              else 0),
                 Fit_UI_Text (Dash_Model.Image (Item.Title),
                              (if G.Header.w > Source_Button_Width then G.Header.w - Source_Button_Width else 0)),
                 K.Text);
         Draw_Legend (C, P, K);
         Draw_Source_Button (C, P, K);
      end if;
      if Item.Series_Used = 0 and then Item.Kind /= Dash_Model.Logs then
         Draw_Message (C, G.Body_Area, K, "No series yet",
                       "Pick one: the arrow at the top right, or Enter while editing");
         return;
      end if;
      if Reason /= "" and then Item.Kind /= Dash_Model.Logs then
         Draw_Empty (C, P, K, Reason);
         return;
      end if;
      if not Has_Port (P) then
         return;
      end if;
      case Item.Kind is
         when Dash_Model.Time_Series => Draw_Time_Series (C, P, K, Damage);
         when Dash_Model.Heatmap => Draw_Heatmap (C, P, K, Damage);
         when Dash_Model.Histogram => Draw_Histogram (C, P, K);
         when Dash_Model.Stat => Draw_Stat (C, P, K);
         when Dash_Model.Logs =>
            Draw_Message (C, G.Body_Area, K, "Logs of " & Dash_Model.Image (Item.Logs_Service),
                          "Click a plot: the slice's logs.between expression appears below (Ctrl+C copies)");
      end case;
   end Draw_Panel;

   function Preset_Rect (I : Preset_Index) return Rect;
   function Live_Rect return Rect;

   function Segment_Width return Natural is (UI_Text_Width ("24h") + 2 * Segment_Padding);
   function Live_Width return Natural is (UI_Text_Width (Live_Label) + Status_Dot + M.Space_S + 2 * Segment_Padding);
   --  The pulse never fades below this share of its colour.
   Pulse_Floor : constant := 35;

   function Live_Dot_Rect return Rect is
      R : constant Rect := Live_Rect;
   begin
      return (R.x, R.y, Status_Dot + Segment_Padding + M.Space_S, R.h);
   end Live_Dot_Rect;

   function Preset_Rect (I : Preset_Index) return Rect is
      W : constant Natural := Segment_Width;
      Total : constant Natural := W * Natural (Preset_Index'Last) + M.Space_M + Live_Width;
      Left : constant Natural := (if Toolbar.w > Total + Margin then Toolbar.x + Toolbar.w - Total - Margin else 0);
   begin
      return (Left + Natural (I - 1) * W, Toolbar.y + (Toolbar.h - Segment_Height) / 2, W, Segment_Height);
   end Preset_Rect;

   function Live_Rect return Rect is
      Last_Preset : constant Rect := Preset_Rect (Preset_Index'Last);
   begin
      return (Last_Preset.x + Last_Preset.w + M.Space_M, Last_Preset.y, Live_Width, Segment_Height);
   end Live_Rect;

   --  Edit (Done while editing), left of the range presets.
   function Edit_Rect return Rect is
      First_Preset : constant Rect := Preset_Rect (Preset_Index'First);
      W : constant Natural :=
        Natural'Max (UI_Text_Width (Edit_Label), UI_Text_Width (Done_Label)) + 2 * Segment_Padding;
   begin
      return ((if First_Preset.x > W + M.Space_M then First_Preset.x - W - M.Space_M else 0), First_Preset.y, W,
              Segment_Height);
   end Edit_Rect;

   procedure Draw_Toolbar (C : Canvas; K : Colors);
   procedure Draw_Toolbar (C : Canvas; K : Colors) is
      Text_Y : constant Natural := Toolbar.y + (Toolbar.h - UI_Text_Height) / 2;
      procedure Segment (R : Rect; Label : String; On : Boolean; Dot : Boolean) is
         Label_X : constant Natural :=
           R.x + (R.w - UI_Text_Width (Label) - (if Dot then Status_Dot + M.Space_S else 0)) / 2;
      begin
         Fill_Rect (C, R, (if On then K.Selection else K.Card));
         Stroke_Rect (C, R, (if On then K.Selection else K.Edge), (if On then K.Selection else K.Edge));
         if Dot then
            declare
               --  A triangle wave over the second: dim, bright, dim.
               Phase : constant Natural := Pulse_Phase;
               Level : constant Natural := (if Phase < Pulse_Steps / 2 then Phase else Pulse_Steps - Phase);
               Strength : constant Palette.Percentage :=
                 Pulse_Floor + (Palette.Percent - Pulse_Floor) * Level / (Pulse_Steps / 2);
               Under : constant Color := (if On then K.Selection else K.Card);
            begin
               Fill_Rect (C, (Label_X, R.y + (R.h - Status_Dot) / 2, Status_Dot, Status_Dot),
                          (if On then Palette.Mix (Palette.Good, Under, Strength) else K.Muted));
               Shown_Pulse := Phase;
            end;
         end if;
         Text (C, Label_X + (if Dot then Status_Dot + M.Space_S else 0), R.y + (R.h - UI_Text_Height) / 2, Label,
               (if On then K.Selection_Text else K.Text));
      end Segment;
   begin
      Fill_Rect (C, Toolbar, K.Bar);
      Fill_Rect (C, (Toolbar.x, Toolbar.y + Toolbar.h - 1, Toolbar.w, 1), K.Rule);
      if Board /= null then
         Strong (C, Toolbar.x + Margin, Text_Y, Dash_Model.Image (Board.Title), K.Text);
         Text (C, Toolbar.x + Margin + Emphasis_Text_Width (Dash_Model.Image (Board.Title)) + M.Space_L, Text_Y,
               (if Live_Mode then "last " & Format.Span_Text (Span)
                else Format.Span_Text (Span) & " ending " & Format.Clock_Text (End_Us, Axis.Microseconds_Per_Second)),
               K.Muted);
      end if;
      --  The range presets, a segmented control, and Live in the same style.
      for I in Preset_Index loop
         declare
            Label : constant String := Preset_Names (I);
         begin
            Segment (Preset_Rect (I), (if Label (Label'First) = ' ' then Label (Label'First + 1 .. Label'Last)
                                       else Label), Span = Presets (I), False);
         end;
      end loop;
      Segment (Live_Rect, Live_Label, Live_Mode, True);
      Segment (Edit_Rect, (if Edit_Mode then Done_Label else Edit_Label), Edit_Mode, False);
   end Draw_Toolbar;

   function Detail_Text return String;

   procedure Draw_Status (C : Canvas; K : Colors);
   procedure Draw_Status (C : Canvas; K : Colors) is
      Save_Text : constant String :=
        (case Shown_Save is
           when Unedited => "",
           when Edited => ", edited",
           when Saving => ", saving",
           when Saved => ", saved",
           when Not_Saved => ", " & Dash_Model.Image (Save_Reason));
      Right_Text : constant String :=
        (if Edit_Mode then "panel " & Format.Decimal (Unsigned_64 (Selected_Panel)) & " of "
           & Format.Decimal (Unsigned_64 (Board.Panels_Used)) & Save_Text
         elsif Shown_Save = Not_Saved then Save_Text (Save_Text'First + 2 .. Save_Text'Last)
         else Format.Decimal (Unsigned_64 (Resolved_Tracks (View'(null record)))) & " of "
           & Format.Decimal (Unsigned_64 (Track_Total (View'(null record)))) & " series live");
      Left_Text : constant String :=
        (if Notice_Length > 0 then Notice_Text (1 .. Notice_Length)
         elsif Picker_On then "Picking the panel's source: arrows choose, Tab the other field, Esc closes"
         elsif Edit_Mode then "Editing: arrows move, Shift+arrows size, Enter picks a source, A adds a panel, ? for keys"
         elsif Detail_Valid then Detail_Text
         elsif Live_Mode then "Drag to pan, wheel to zoom, E to edit, ? for keys"
         else "Paused: L or End returns to live");
      Y : constant Natural := Status.y + (Status.h - UI_Text_Height) / 2;
   begin
      Fill_Rect (C, Status, K.Bar);
      Fill_Rect (C, (Status.x, Status.y, Status.w, 1), K.Rule);
      Text_Right (C, Status.x + Status.w - Margin, Y, Right_Text, K.Muted);
      Text (C, Status.x + Margin, Y,
            Fit_UI_Text (Left_Text, (if Status.w > UI_Text_Width (Right_Text) + 3 * Margin
                                     then Status.w - UI_Text_Width (Right_Text) - 3 * Margin else 0)),
            (if Detail_Valid or else Notice_Length > 0 then K.Text else K.Muted));
   end Draw_Status;

   procedure Draw_Tooltip (C : Canvas; K : Colors);

   --  The page between the cards, and only there: band by band (split at
   --  every card's top and bottom), the gaps between the cards in it.
   procedure Fill_Page (C : Canvas; K : Colors; Area : Rect);
   procedure Fill_Page (C : Canvas; K : Colors; Area : Rect) is
      Count : constant Natural := (if Board = null then 0 else Board.Panels_Used);
      type Edge_List is array (1 .. 2 * Dash_Model.Maximum_Panels + 2) of Natural;
      Edges : Edge_List := [others => 0];
      Used : Natural := 0;
      procedure Add_Edge (Y : Natural) is
      begin
         if Y > Area.y and then Y < Area.y + Area.h then
            for I in 1 .. Used loop
               if Edges (I) = Y then
                  return;
               end if;
            end loop;
            Used := Used + 1;
            Edges (Used) := Y;
         end if;
      end Add_Edge;
   begin
      Used := 1;
      Edges (1) := Area.y;
      for P in 1 .. Count loop
         Add_Edge (Panels (P).Outer.y);
         Add_Edge (Panels (P).Outer.y + Panels (P).Outer.h);
      end loop;
      Used := Used + 1;
      Edges (Used) := Area.y + Area.h;
      --  Sorted (a handful of edges: insertion).
      for I in 2 .. Used loop
         declare
            E : constant Natural := Edges (I);
            J : Natural := I;
         begin
            while J > 1 and then Edges (J - 1) > E loop
               Edges (J) := Edges (J - 1);
               J := J - 1;
            end loop;
            Edges (J) := E;
         end;
      end loop;
      for B in 1 .. Used - 1 loop
         declare
            Top : constant Natural := Edges (B);
            Height : constant Natural := Edges (B + 1) - Edges (B);
            X : Natural := Area.x;
         begin
            if Height > 0 then
               --  Left to right: the cards that cover this band, by x.
               loop
                  declare
                     Next_Card : Natural := 0;
                     Next_X : Natural := Area.x + Area.w;
                  begin
                     for P in 1 .. Count loop
                        declare
                           O : constant Rect := Panels (P).Outer;
                        begin
                           if O.y <= Top and then O.y + O.h >= Top + Height and then O.x + O.w > X
                             and then O.x < Next_X
                           then
                              Next_Card := P;
                              Next_X := Natural'Max (O.x, X);
                           end if;
                        end;
                     end loop;
                     if Next_X > X then
                        Fill_Rect (C, (X, Top, Next_X - X, Height), K.Page);
                     end if;
                     exit when Next_Card = 0;
                     X := Panels (Next_Card).Outer.x + Panels (Next_Card).Outer.w;
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Fill_Page;

   --  The selected panel while editing: an outline and the resize grip.
   function Grip_Rect (P : Dash_Model.Panel_Index) return Rect is
      O : constant Rect := Panels (P).Outer;
   begin
      return ((if O.w > Grip_Size then O.x + O.w - Grip_Size else O.x),
              (if O.h > Grip_Size then O.y + O.h - Grip_Size else O.y),
              Natural'Min (Grip_Size, O.w), Natural'Min (Grip_Size, O.h));
   end Grip_Rect;

   procedure Draw_Selection (C : Canvas; K : Colors; P : Dash_Model.Panel_Index) is
      O : constant Rect := Panels (P).Outer;
      G : constant Rect := Grip_Rect (P);
   begin
      for I in 0 .. Selection_Stroke - 1 loop
         if O.w > 2 * I and then O.h > 2 * I then
            Stroke_Rect (C, (O.x + I, O.y + I, O.w - 2 * I, O.h - 2 * I), K.Accent, K.Accent);
         end if;
      end loop;
      --  The grip: a triangle in the corner.
      for Row in 0 .. G.h - 1 loop
         declare
            Side : constant Natural := (Row + 1) * G.w / Natural'Max (1, G.h);
         begin
            Fill_Rect (C, (G.x + G.w - Side, G.y + Row, Side, 1), K.Accent);
         end;
      end loop;
   end Draw_Selection;

   --  The keys, as a card in the overlay's place.
   type Key_Line is record
      Keys, Action : access constant String;
   end record;
   K_Edit : aliased constant String := "E";
   A_Edit : aliased constant String := "Edit the layout, or finish (also Esc)";
   K_Tab : aliased constant String := "Tab, Shift+Tab";
   A_Tab : aliased constant String := "Select the next or previous panel";
   K_Move : aliased constant String := "Arrows";
   A_Move : aliased constant String := "Move the panel earlier or later";
   K_Size : aliased constant String := "Shift+arrows";
   A_Size : aliased constant String := "Make it narrower, wider, shorter, taller";
   K_Remove : aliased constant String := "Delete";
   A_Remove : aliased constant String := "Remove the panel";
   K_Copy : aliased constant String := "Insert";
   A_Copy : aliased constant String := "Copy the panel";
   K_Undo : aliased constant String := "Ctrl+Z, Ctrl+Y";
   A_Undo : aliased constant String := "Undo, redo";
   K_Source : aliased constant String := "C";
   A_Source : aliased constant String := "Show the dashboard as CCL (Ctrl+C copies it)";
   K_Range : aliased constant String := "1 .. 6, +, -";
   A_Range : aliased constant String := "Time range: presets, zoom in and out";
   K_Pan : aliased constant String := "Left, Right, Home, L";
   A_Pan : aliased constant String := "Pan back and forth, to the start, live";
   K_Pointer : aliased constant String := "Drag, wheel, Shift+drag";
   A_Pointer : aliased constant String := "Pan, zoom, select a range (editing: move, grip sizes)";
   K_Add : aliased constant String := "A";
   A_Add : aliased constant String := "Add a panel (its source picker opens)";
   K_Pick : aliased constant String := "Enter";
   A_Pick : aliased constant String := "Pick the selected panel's series and view";
   K_Menus : aliased constant String := "F10, Alt+F, Alt+E, Alt+V";
   A_Menus : aliased constant String := "The menus";
   K_Files : aliased constant String := "Ctrl+N, Ctrl+O, Ctrl+Shift+S";
   A_Files : aliased constant String := "New, open, save the dashboard as";
   K_Help : aliased constant String := "? or F1";
   A_Help : aliased constant String := "These keys (again, or Esc, closes)";
   Key_Lines : constant array (Positive range <>) of Key_Line :=
     [(K_Edit'Access, A_Edit'Access), (K_Tab'Access, A_Tab'Access), (K_Move'Access, A_Move'Access),
      (K_Size'Access, A_Size'Access), (K_Remove'Access, A_Remove'Access), (K_Copy'Access, A_Copy'Access),
      (K_Add'Access, A_Add'Access), (K_Pick'Access, A_Pick'Access),
      (K_Undo'Access, A_Undo'Access), (K_Source'Access, A_Source'Access), (K_Range'Access, A_Range'Access),
      (K_Pan'Access, A_Pan'Access), (K_Pointer'Access, A_Pointer'Access), (K_Menus'Access, A_Menus'Access),
      (K_Files'Access, A_Files'Access), (K_Help'Access, A_Help'Access)];

   procedure Draw_Keys (C : Canvas; K : Colors);
   function Overlay_Box return Rect is
      Area : constant Rect := Source_Box;
   begin
      if Shown_Overlay = Source_Overlay then
         return Area;
      end if;
      if Keys_W = 0 then
         declare
            Key_Width, Action_Width : Natural := 0;
         begin
            for L of Key_Lines loop
               Key_Width := Natural'Max (Key_Width, Emphasis_Text_Width (L.Keys.all));
               Action_Width := Natural'Max (Action_Width, UI_Text_Width (L.Action.all));
            end loop;
            Keys_W := Key_Width + Action_Width + 3 * Margin;
            Keys_H := (Key_Lines'Length + 1) * (UI_Text_Height + M.Space_S) + M.Space_S + 2 * Margin;
         end;
      end if;
      declare
         W : constant Natural := Natural'Min (Keys_W, Area.w);
         H : constant Natural := Natural'Min (Keys_H, Area.h);
      begin
         return (Area.x + (Area.w - W) / 2, Area.y + (Area.h - H) / 2, W, H);
      end;
   end Overlay_Box;

   procedure Draw_Keys (C : Canvas; K : Colors) is
      Box : constant Rect := Overlay_Box;
      Row : constant Natural := UI_Text_Height + M.Space_S;
      Key_Width : Natural := 0;
      Y : Natural := Box.y + Margin;
   begin
      for L of Key_Lines loop
         Key_Width := Natural'Max (Key_Width, Emphasis_Text_Width (L.Keys.all));
      end loop;
      Fill_Rect (C, Box, K.Card);
      Stroke_Rect (C, Box, K.Accent, K.Accent);
      Strong (C, Box.x + Margin, Y, "Keys", K.Text);
      Y := Y + Row + M.Space_S;
      for L of Key_Lines loop
         exit when Y + Row > Box.y + Box.h;
         Strong (C, Box.x + Margin, Y, L.Keys.all, K.Accent);
         Text (C, Box.x + 2 * Margin + Key_Width, Y,
               Fit_UI_Text (L.Action.all, (if Box.w > 3 * Margin + Key_Width then Box.w - 3 * Margin - Key_Width
                                           else 0)), K.Text);
         Y := Y + Row;
      end loop;
   end Draw_Keys;

   procedure Draw_Source (C : Canvas; K : Colors);
   procedure Draw_Source (C : Canvas; K : Colors) is
      Box : constant Rect := Source_Box;
      Line_H : constant Natural := Code_Text_Height + M.Space_XS;
      Left : constant Natural := Box.x + Margin;
      Width : constant Natural := (if Box.w > 2 * Margin then Box.w - 2 * Margin else 0);
      Bottom : constant Natural := Box.y + Box.h - Margin;
      Y : Natural := Box.y + Margin;
      Indent : constant String (1 .. Source_Indent) := [others => ' '];
      D : Dash_Model.Dashboard renames Board.all;
      Header : constant String := Dash_Model.To_Source ((D with delta Panels_Used => 0));
      --  The header without its closing "[])": the panels follow it.
      Opening : constant String :=
        (if Header'Length > 3 then Header (Header'First .. Header'Last - 2) else Header);
      procedure Line (S : String; Ink : Color; Continued : Natural := 0) is
         Rest_First : Natural := S'First;
      begin
         --  Long lines go on, indented, as many lines as they need.
         loop
            exit when Y + Line_H > Bottom;
            declare
               Lead : constant String := (if Rest_First = S'First then "" else Indent & Indent);
               Prefix : constant String := (1 .. Continued => ' ') & Lead;
               Room : constant Natural :=
                 (if Width > Code_Text_Width (Prefix) then Width - Code_Text_Width (Prefix) else 0);
               Rest : constant String := S (Rest_First .. S'Last);
               Kept : constant Natural := Natural'Max (1, Kept_Code_Text (Rest, Room));
               Shown : Natural := (if Code_Text_Width (Rest) <= Room then Rest'Length else Kept);
            begin
               --  A break at the last space that fits, when there is one.
               if Shown < Rest'Length then
                  for K in reverse Rest'First + 1 .. Rest'First + Shown - 1 loop
                     if Rest (K) = ' ' then
                        Shown := K - Rest'First + 1;
                        exit;
                     end if;
                  end loop;
               end if;
               Draw_Code_Text_Transparent
                 (C, Left + Code_Text_Width (Prefix), Y, Rest (Rest'First .. Rest'First + Shown - 1), Ink);
               Y := Y + Line_H;
               Rest_First := Rest_First + Shown;
               exit when Rest_First > S'Last;
            end;
         end loop;
      end Line;
   begin
      if Shown_Overlay = Keys_Overlay then
         Draw_Keys (C, K);
         return;
      end if;
      Fill_Rect (C, Box, K.Card);
      Stroke_Rect (C, Box, K.Accent, K.Accent);
      Strong (C, Left, Y, "The dashboard as CCL", K.Text);
      Text_Right (C, Box.x + Box.w - Margin, Y, "Ctrl+C copies it, C or Esc closes", K.Muted);
      Y := Y + UI_Text_Height + M.Space_M;
      Line (Opening, K.Muted);
      for P in 1 .. D.Panels_Used loop
         Line (Dash_Model.Panel_Source (D.Panels (P)), (if P = Selected_Panel then K.Accent else K.Text),
               Source_Indent);
      end loop;
      Line ("])", K.Muted);
   end Draw_Source;

   ---------------------------------------------------------------------------
   --  The layers' drawing
   ---------------------------------------------------------------------------
   --  The picker's card: under its panel's source button, inside the window.
   Picker_Height : constant Natural :=
     2 * Picker_Padding + UI_Text_Height + M.Space_M + 2 * (M.Control_Height + Picker_Row_Gap) + UI_Text_Height;
   function Picker_Box return Rect is
   begin
      if not Picker_On or else Board = null or else Picker_Target not in 1 .. Board.Panels_Used then
         return (others => 0);
      end if;
      declare
         B : constant Rect := Source_Button (Picker_Target);
         W : constant Natural := Natural'Min (Picker_Width, (if Whole.w > 2 * Margin then Whole.w - 2 * Margin else 0));
         H : constant Natural := Picker_Height;
         Left_Limit : constant Natural := Whole.x + Margin;
         Right_Limit : constant Natural := Whole.x + Whole.w - Margin;
         X : constant Natural :=
           (if B.x + B.w >= Left_Limit + W then Natural'Min (B.x + B.w, Right_Limit) - W else Left_Limit);
         Below : constant Natural := B.y + B.h + M.Space_XS;
         Y : constant Natural :=
           (if Below + H <= Status.y or else B.y < H + M.Space_XS + Toolbar.y + Toolbar.h then Below
            else B.y - H - M.Space_XS);
      begin
         return (X, Y, W, H);
      end;
   end Picker_Box;

   function Field_Box (Field : Picker_Field) return Rect is
      Card : constant Rect := Picker_Box;
      Top : constant Natural := Card.y + Picker_Padding + UI_Text_Height + M.Space_M;
      Left : constant Natural := Card.x + Picker_Padding + Picker_Label_Width;
   begin
      if Is_Empty (Card) then
         return (others => 0);
      end if;
      return (Left, Top + Picker_Field'Pos (Field) * (M.Control_Height + Picker_Row_Gap),
              (if Card.w > 2 * Picker_Padding + Picker_Label_Width then Card.w - 2 * Picker_Padding - Picker_Label_Width
               else 1),
              M.Control_Height);
   end Field_Box;

   --  The closed field first, the open one last (its list over the other).
   procedure Draw_Fields (C : Canvas; Into : in out Controls.Control_Map);
   procedure Draw_Fields (C : Canvas; Into : in out Controls.Control_Map) is
      T : constant Theme := Current_Theme;
      Series_Last : constant Boolean := CB.Is_Open (Series_Combo);
      procedure Series is
      begin
         CB.Draw (C, Into, Series_Combo, Series_Model, Series_Base, Field_Box (Series_Field), T,
                  Focused => Picker_Focus = Series_Field, Visible_Rows => Picker_Rows);
      end Series;
      procedure Views is
      begin
         CB.Draw (C, Into, View_Combo, View_Model, View_Base, Field_Box (View_Field), T,
                  Focused => Picker_Focus = View_Field, Visible_Rows => Picker_Rows);
      end Views;
   begin
      if Series_Last then
         Views;
         Series;
      else
         Series;
         Views;
      end if;
   end Draw_Fields;

   procedure Draw_Picker (C : Canvas; K : Colors);
   procedure Draw_Picker (C : Canvas; K : Colors) is
      Card : constant Rect := Picker_Box;
      Inner : constant Natural := (if Card.w > 2 * Picker_Padding then Card.w - 2 * Picker_Padding else 0);
      Title : constant String :=
        (if Picker_Target in 1 .. Board.Panels_Used then Dash_Model.Image (Board.Panels (Picker_Target).Title) else "");
      procedure Label (Field : Picker_Field; Caption : String) is
         F : constant Rect := Field_Box (Field);
      begin
         Text (C, Card.x + Picker_Padding, F.y + (if F.h > UI_Text_Height then (F.h - UI_Text_Height) / 2 else 0),
               Caption, K.Muted);
      end Label;
   begin
      if Is_Empty (Card) then
         return;
      end if;
      if Intersects (C.clip, Card) or else not C.clipEnabled then
         Fill_Rect (C, Card, K.Card);
         Stroke_Rect (C, Card, K.Accent, K.Accent);
         Strong (C, Card.x + Picker_Padding, Card.y + Picker_Padding, Fit_UI_Text (Title, Inner), K.Text);
         Label (Series_Field, "Series");
         Label (View_Field, "View");
         Text (C, Card.x + Picker_Padding, Card.y + Card.h - Picker_Padding - UI_Text_Height,
               Fit_UI_Text ("Applies at once; Ctrl+Z undoes, Esc closes", Inner), K.Muted);
      end if;
      Controls.Clear (Paint_Map);
      Draw_Fields (C, Paint_Map);
   end Draw_Picker;

   --  The menu bar and its open menu; their controls cover the window
   --  whatever this frame's clip.
   procedure Draw_Menus (C : Canvas; Into : in out Controls.Control_Map);
   procedure Draw_Menus (C : Canvas; Into : in out Controls.Control_Map) is
      Whole_Input : Canvas := C;
   begin
      Whole_Input.inputClipEnabled := True;
      Whole_Input.inputClip := Whole;
      Menus.Draw (Whole_Input, Into, Menu_State, Menu_Model, Menu_Base, Menu_Area, Current_Theme, Menu_Width,
                  Menu_Row);
   end Draw_Menus;

   --  What hit testing sees, again: the menu bar and the picker's fields
   --  as they stand (drawn with nothing visible: only the registrations).
   procedure Register is
      Ghost : Canvas := Dash_Layers.Canvas;
   begin
      Ghost.clipEnabled := True;
      Ghost.clip := (others => 0);
      Ghost.inputClipEnabled := True;
      Ghost.inputClip := Whole;
      Controls.Clear (Map);
      if Picker_On then
         Draw_Fields (Ghost, Map);
      end if;
      Draw_Menus (Ghost, Map);
   end Register;

   procedure Paint (C : Canvas; Damage : Rect) is
      K : constant Colors := Current_Colors;
      Above : constant Natural := Menu_Area.h + Toolbar.h;
      Content : constant Rect :=
        (Whole.x, Toolbar.y + Toolbar.h, Whole.w,
         (if Whole.h > Above + Status.h then Whole.h - Above - Status.h else 0));
      --  Wholly under a layer: only the layers are drawn.
      Hidden : constant Boolean := Under_Layer (Damage);
   begin
      if Hidden then
         null;
      elsif Board /= null and then Source_Shown and then Inside (Overlay_Box, Damage) then
         --  All under the opaque overlay.
         Draw_Source (With_Clip (C, Overlay_Box), K);
      else
         if Intersects (Damage, Content) then
            Fill_Page (C, K, Content);
         end if;
         if Intersects (Damage, Toolbar) then
            Draw_Toolbar (C, K);
         end if;
         if Board /= null then
            for P in 1 .. Board.Panels_Used loop
               --  Nothing a panel draws leaves its card.
               if Intersects (Panels (P).Outer, Damage) then
                  Draw_Panel (With_Clip (C, Panels (P).Outer), P, K, Damage);
               end if;
            end loop;
         end if;
         if Board /= null and then Edit_Mode and then Selected_Panel in 1 .. Board.Panels_Used
           and then Intersects (Panels (Selected_Panel).Outer, Damage)
         then
            Draw_Selection (With_Clip (C, Panels (Selected_Panel).Outer), K, Selected_Panel);
         end if;
         if Intersects (Damage, Status) then
            Draw_Status (C, K);
         end if;
         if Board /= null and then Source_Shown and then Intersects (Damage, Overlay_Box) then
            Draw_Source (With_Clip (C, Overlay_Box), K);
         elsif Hover_Valid and then not Is_Empty (Tooltip) and then Intersects (Damage, Tooltip) then
            Draw_Tooltip (C, K);
         end if;
      end if;
      if Picker_On and then Intersects (Damage, Picker_Cover) then
         Draw_Picker (C, K);
      end if;
      if Intersects (Damage, Menu_Area) or else Intersects (Damage, Menu_Popup) then
         Controls.Clear (Paint_Map);
         Draw_Menus (C, Paint_Map);
      end if;
      if Dialogs.Is_Open (Dialog) and then Intersects (Damage, Dialog_Box) then
         Dialogs.Draw (C, Dialog, Current_Theme);
      end if;
   end Paint;

   procedure Render (V : in out View; C : Canvas; Damage : Rect) is
      pragma Unreferenced (V);
   begin
      if not Dash_Layers.Ready then
         Paint (C, Damage);
         return;
      end if;
      --  What changed, exactly, into the retained frame; then the frame
      --  the toolkit asked for is a copy.
      Last_Paint_Us := 0;
      if Dash_Layers.Dirty_Count > 0 then
         --  The columns are read under the lock; the copy out needs none.
         Lock_UI;
         declare
            Started : constant Unsigned_64 := Feed.Now_Us;
         begin
            for I in 1 .. Dash_Layers.Dirty_Count loop
               declare
                  R : constant Rect := Dash_Layers.Dirty (I);
               begin
                  Paint (Dash_Layers.Canvas (R), R);
               end;
            end loop;
            Last_Paint_Us := Feed.Now_Us - Started;
         end;
         Dash_Sync.Unlock;
      end if;
      Dash_Layers.Clear_Dirty;
      declare
         Started : constant Unsigned_64 := Feed.Now_Us;
         Union : constant Unsigned_64 := Unsigned_64 (Damage.w) * Unsigned_64 (Damage.h);
      begin
         Dash_Layers.Copy_Out (C, Damage);
         Last_Copy_Us := Feed.Now_Us - Started;
         Last_Union_Pixels := Union;
         --  The toolkit's damage also repairs the other buffer; what this
         --  frame changed is at most the union.
         Last_Exact_Pixels := Unsigned_64'Min (Marked_Pixels, Union);
         Marked_Pixels := 0;
      end;
   end Render;

   procedure Probe_Full_Redraw (V : in out View; Took_Us : out Unsigned_64) is
      pragma Unreferenced (V);
      Started : Unsigned_64;
   begin
      Took_Us := 0;
      if Board = null or else not Dash_Layers.Ready then
         return;
      end if;
      Lock_UI;
      Started := Feed.Now_Us;
      Paint (Dash_Layers.Canvas (Whole), Whole);
      Took_Us := Feed.Now_Us - Started;
      Dash_Sync.Unlock;
   end Probe_Full_Redraw;

   procedure Take_Copy_Stats (V : View; Paint_Us, Copy_Us, Union_Pixels, Exact_Pixels : out Unsigned_64) is
      pragma Unreferenced (V);
   begin
      Paint_Us := Last_Paint_Us;
      Copy_Us := Last_Copy_Us;
      Union_Pixels := Last_Union_Pixels;
      Exact_Pixels := Last_Exact_Pixels;
   end Take_Copy_Stats;

   procedure Invalidate (V : in out View; R : Rect; Damage : in out Rect) is
      pragma Unreferenced (V);
   begin
      Mark (Damage, R);
   end Invalidate;

   ---------------------------------------------------------------------------
   --  Hover and tooltip
   ---------------------------------------------------------------------------
   function Panel_At (X, Y : Natural) return Natural is
   begin
      if Board = null then
         return 0;
      end if;
      for P in 1 .. Board.Panels_Used loop
         if Point_In_Rect (X, Y, Panels (P).Plot) and then Has_Port (P) and then Time_Based (Panels_Kind (P)) then
            return P;
         end if;
      end loop;
      return 0;
   end Panel_At;

   --  The card under a point, any kind (the editor's).
   function Card_At (X, Y : Natural) return Natural is
   begin
      if Board /= null then
         for P in 1 .. Board.Panels_Used loop
            if Point_In_Rect (X, Y, Panels (P).Outer) then
               return P;
            end if;
         end loop;
      end if;
      return 0;
   end Card_At;

   Tooltip_Lines : constant := 9;

   function Tooltip_Box return Rect is
      P : constant Natural := Hover_Panel;
      First, Last : Natural;
      Lines : Natural := 1;
      Widest : Natural := 0;
   begin
      if P = 0 or else not Hover_Valid then
         return (others => 0);
      end if;
      Tracks_Of (P, First, Last);
      for I in First .. Last loop
         if Tracks (I).State = Dash_Tracks.Resolved and then Lines < Tooltip_Lines then
            Lines := Lines + 1;
            Widest := Natural'Max (Widest, UI_Text_Width (Series_Label (Tracks (I).all)));
         end if;
      end loop;
      declare
         W : constant Natural := Widest + 22 * M.Mono_Advance + 2 * Tooltip_Padding + Tooltip_Swatch;
         H : constant Natural := Lines * Tooltip_Row + 2 * Tooltip_Padding;
         --  Inside the hovered card: right of the pointer, flipped left
         --  near the card's edge; below it, flipped above near the bottom.
         Card : constant Rect := Panels (P).Outer;
         Inner_Right : constant Natural := Card.x + Card.w - M.Space_S;
         Inner_Bottom : constant Natural := Card.y + Card.h - M.Space_S;
         X : constant Natural :=
           (if Hover_X + Tooltip_Offset + W <= Inner_Right then Hover_X + Tooltip_Offset
            elsif Hover_X >= Card.x + W + Tooltip_Offset then Hover_X - W - Tooltip_Offset
            else Card.x + M.Space_S);
         Y : constant Natural :=
           (if Hover_Y + Tooltip_Offset + H <= Inner_Bottom then Hover_Y + Tooltip_Offset
            elsif Hover_Y >= Card.y + H + Tooltip_Offset then Hover_Y - H - Tooltip_Offset
            else Card.y + M.Space_S);
      begin
         return (X, Y, Natural'Min (W, Card.w - M.Space_S), Natural'Min (H, Card.h - M.Space_S));
      end;
   end Tooltip_Box;

   --  The summary of the bin (Cell_Columns) holding column Col: what the
   --  lines and cells show there.
   function Bin_Summary (T : Dash_Tracks.Track; P : Dash_Model.Panel_Index; Col : Column_Number)
     return Columns.Summary
   is
      Bin : constant Unsigned_64 := Unsigned_64 (Cell_Columns (P));
      S : Columns.Summary := Columns.Nothing;
   begin
      for C in Col / Bin * Bin .. Col / Bin * Bin + Bin - 1 loop
         S := Columns.Merge (S, Columns.Get (T.Columns.all, C));
      end loop;
      return S;
   end Bin_Summary;

   procedure Draw_Tooltip (C : Canvas; K : Colors) is
      P : constant Natural := Hover_Panel;
      First, Last : Natural;
      Box : constant Rect := Tooltip;
      Y : Natural := Box.y + Tooltip_Padding;
      Port : constant Axis.Viewport := Ports (P);
      Col : constant Column_Number := Axis.Column_Of (Hover_Time, Port.Width);
      Item : constant Dash_Model.Panel := Panel_Of (P);
      Line_Index : Natural := 0;
   begin
      Fill_Rect (C, (Box.x + 2, Box.y + 2, Box.w, Box.h), Palette.Mix (0, K.Page, 30));
      Fill_Rect (C, Box, K.Card);
      Stroke_Rect (C, Box, K.Edge, K.Edge);
      Strong (C, Box.x + Tooltip_Padding, Y,
              Format.Clock_Text (Axis.Start_Of (Col / Unsigned_64 (Cell_Columns (P)) * Unsigned_64 (Cell_Columns (P)),
                                                Port.Width), 0), K.Text);
      Text_Right (C, Box.x + Box.w - Tooltip_Padding, Y,
                  Format.Span_Text (Port.Width * Unsigned_64 (Cell_Columns (P))) & " slice", K.Muted);
      Y := Y + Tooltip_Row;
      Tracks_Of (P, First, Last);
      for I in First .. Last loop
         declare
            T : Dash_Tracks.Track renames Tracks (I).all;
         begin
            if T.State = Dash_Tracks.Resolved and then T.Columns /= null and then Y + Tooltip_Row <= Box.y + Box.h then
               declare
                  S : constant Columns.Summary := Bin_Summary (T, P, Col);
                  U : constant Format.Unit := Unit_Of (T);
                  Values_Text : constant String :=
                    (if S.Origin = Columns.Empty then "no samples"
                     else "p50 " & Format.Value_Text (S.P50, U) & "  p99 " & Format.Value_Text (S.P99, U)
                       & "  n " & Format.Value_Text (S.Count, Format.Count));
                  Ink : Color := Line_Color (K, (if Item.Kind = Dash_Model.Time_Series then Line_Index + 1 else 1));
               begin
                  if Item.Kind = Dash_Model.Heatmap and then S.Origin /= Columns.Empty then
                     declare
                        V : constant Unsigned_64 := Value_Of (S, Item.Shown, Port.Width);
                     begin
                        Ink := (if Item.Budget > 0 and then V > Item.Budget then Palette.Critical
                                else K.Ramp (Heat_Step (P, V)));
                     end;
                  end if;
                  Fill_Rect (C, (Box.x + Tooltip_Padding, Y + (Tooltip_Row - Tooltip_Swatch) / 2, Tooltip_Swatch,
                                 Tooltip_Swatch), Ink);
                  Text (C, Box.x + Tooltip_Padding + Tooltip_Swatch + M.Space_S, Y, Series_Label (T), K.Muted);
                  Text_Right (C, Box.x + Box.w - Tooltip_Padding, Y, Values_Text, K.Text);
                  Y := Y + Tooltip_Row;
               end;
            end if;
            for Stat in Dash_Model.Statistic loop
               if Item.Lines (Stat) then
                  Line_Index := Line_Index + 1;
               end if;
            end loop;
         end;
      end loop;
   end Draw_Tooltip;

   function Time_At (P : Dash_Model.Panel_Index; X : Natural) return Unsigned_64 is
      Port : constant Axis.Viewport := Ports (P);
      Plot : constant Rect := Panels (P).Plot;
      Offset : constant Natural := (if X > Plot.x then Natural'Min (X - Plot.x, Plot.w - 1) else 0);
   begin
      if Axis.Has_Column_At (Port, Offset) then
         return Axis.Start_Of (Axis.Column_At (Port, Offset), Port.Width) + Port.Width / 2;
      end if;
      return 0;
   end Time_At;

   --  Every time-based panel's crosshair strip at Time (the crosshair is linked).
   --  The hovered panel's crosshair strip at Time (others show none: a
   --  hover repaints two columns and the tooltip, not every plot).
   function Crosshair_Damage (P : Natural; Time : Unsigned_64) return Rect is
   begin
      if P = 0 or else not Has_Port (P) then
         return (others => 0);
      end if;
      declare
         Port : constant Axis.Viewport := Ports (P);
         Plot : constant Rect := Panels (P).Plot;
         Col : constant Column_Number := Axis.Column_Of (Time, Port.Width);
      begin
         if not Axis.Visible (Port, Col) then
            return (others => 0);
         end if;
         return (Plot.x + Axis.X_Of (Port, Col), Plot.y, 1, Plot.h);
      end;
   end Crosshair_Damage;

   --  Every panel whose picture follows the viewport: plots with their
   --  axes, histograms whole, stat sparklines (their numbers follow when the
   --  store answers for the new window, in Refresh).
   procedure Mark_Plots (Damage : in out Rect);
   procedure Mark_Plots (Damage : in out Rect) is
   begin
      if Board /= null then
         for P in 1 .. Board.Panels_Used loop
            Mark (Damage, (case Panels_Kind (P) is
                             when Dash_Model.Time_Series | Dash_Model.Heatmap => Plot_Area (P),
                             when Dash_Model.Stat => Panels (P).Plot,
                             when others => Panels (P).Outer));
         end loop;
      end if;
   end Mark_Plots;

   --  The plots' pixels follow a viewport moved by whole columns (a pan, the
   --  live edge's step): shifted in the retained frame, and only the strip
   --  that scrolls in (with the joins reaching into it), the axes and any
   --  overlay are redrawn. Another width, a new top or too far: whole.
   procedure Scroll_Plot (P : Dash_Model.Panel_Index; Old, New_Port : Axis.Viewport; Damage : in out Rect) is
      Plot : constant Rect := Panels (P).Plot;
      Reach : constant Natural := Bridge_Of (P) + Cell_Columns (P);
      Overlay : constant Boolean :=
        (Hover_Valid and then Hover_Panel = P) or else Selecting or else (Detail_Valid and then Detail_Panel = P)
        or else Touches_Layer (Plot) or else (Source_Shown and then Intersects (Plot, Overlay_Box));
   begin
      Mark (Damage, Panels (P).Bottom_Axis);
      if Old.Width /= New_Port.Width or else Old.Columns /= New_Port.Columns or else Overlay
        or else (if New_Port.Last > Old.Last then New_Port.Last - Old.Last else Old.Last - New_Port.Last)
                >= Unsigned_64 (Plot.w / 2)
      then
         Mark (Damage, Plot_Area (P));
         return;
      end if;
      declare
         Moved : constant Natural :=
           Natural (if New_Port.Last > Old.Last then New_Port.Last - Old.Last else Old.Last - New_Port.Last);
         Strip : constant Natural := Natural'Min (Plot.w, Moved + Reach);
      begin
         if Moved = 0 then
            return;
         end if;
         --  Later: the picture moves left and the right edge scrolls in.
         Dash_Layers.Shift (Plot, (if New_Port.Last > Old.Last then -Moved else Moved));
         --  The toolkit copies the moved picture; nothing in it is redrawn.
         Damage := Add_Rect (Damage, Plot);
         if New_Port.Last > Old.Last then
            Dash_Layers.Invalidate ((Plot.x + Plot.w - Strip, Plot.y, Strip, Plot.h));
         else
            Dash_Layers.Invalidate ((Plot.x, Plot.y, Strip, Plot.h));
         end if;
         --  Both edges: a bin cut by an edge merges other columns now.
         Dash_Layers.Invalidate ((Plot.x, Plot.y, Natural'Min (Reach, Plot.w), Plot.h));
         Dash_Layers.Invalidate ((Plot.x + Plot.w - Natural'Min (Reach, Plot.w), Plot.y, Natural'Min (Reach, Plot.w),
                                  Plot.h));
      end;
   end Scroll_Plot;

   function Is_Preset (S : Unsigned_64) return Boolean is (for some P of Presets => P = S);

   --  The panel input works in: the hovered one, the pressed one, else the
   --  first time-based one.
   function Focus_Panel return Natural is
   begin
      if Hover_Panel /= 0 then
         return Hover_Panel;
      elsif Press_Panel /= 0 then
         return Press_Panel;
      end if;
      for P in 1 .. Board.Panels_Used loop
         if Panels_Kind (P) in Dash_Model.Time_Series | Dash_Model.Heatmap then
            return P;
         end if;
      end loop;
      return 1;
   end Focus_Panel;

   procedure Set_Span (New_Span : Unsigned_64; Anchor : Unsigned_64; Damage : in out Rect);
   procedure Set_Span (New_Span : Unsigned_64; Anchor : Unsigned_64; Damage : in out Rect) is
      Clamped : constant Axis.Span :=
        Unsigned_64'Max (Axis.Minimum_Span_Us, Unsigned_64'Min (New_Span, Axis.Maximum_Span_Us));
      Old_End : constant Unsigned_64 := Current_End;
   begin
      if not Live_Mode and then Anchor > 0 and then Old_End >= Anchor then
         --  Keep the anchor's time under the pointer.
         End_Us := Anchor + (Old_End - Anchor) * (Clamped / Axis.Microseconds_Per_Millisecond)
           / Unsigned_64'Max (1, Span / Axis.Microseconds_Per_Millisecond);
         End_Us := Unsigned_64'Min (End_Us, Feed.Now_Us);
      end if;
      --  The toolbar's span text; the presets only when one turns on or off.
      Mark (Damage, (Toolbar.x, Toolbar.y, Natural'Max (Preset_Rect (1).x, Toolbar.x) - Toolbar.x, Toolbar.h));
      if Is_Preset (Span) or else Is_Preset (Clamped) then
         Mark (Damage, (Preset_Rect (1).x, Toolbar.y,
                        Natural'Max (Live_Rect.x + Live_Rect.w, Preset_Rect (1).x) - Preset_Rect (1).x, Toolbar.h));
      end if;
      Span := Clamped;
      Update_Quantum;
      --  Every track resamples at once (the preview the store's answer
      --  refines); each plot and its time axis are redrawn, its value axis
      --  only when its top changed, a stat's number when the store answers.
      --  The panel the person works in follows in this frame; the others in
      --  the next (Refresh sees their viewports moved), so an input's own
      --  frame redraws one panel, not all of them.
      if Board /= null then
         for P in 1 .. Board.Panels_Used loop
            if P = Focus_Panel then
            declare
               Port : constant Axis.Viewport := Viewport_For (P);
               Old_Top : constant Unsigned_64 := Tops (P);
               First, Last : Natural;
            begin
               Tracks_Of (P, First, Last);
               for I in First .. Last loop
                  Dash_Tracks.Follow (Tracks (I).all, Port);
               end loop;
               Ports (P) := Port;
               Has_Port (P) := Tracks /= null and then Last >= First;
               Tops (P) := Panel_Top (P);
               case Panels_Kind (P) is
                  when Dash_Model.Time_Series | Dash_Model.Heatmap =>
                     Mark (Damage, Panels (P).Plot);
                     Mark (Damage, Panels (P).Bottom_Axis);
                     if Tops (P) /= Old_Top then
                        Mark (Damage, Panels (P).Left_Axis);
                        Mark (Damage, Panels (P).Header);
                     end if;
                  when Dash_Model.Stat => Mark (Damage, Panels (P).Plot);
                  when others => Mark (Damage, Panels (P).Outer);
               end case;
            end;
            end if;
         end loop;
      end if;
   end Set_Span;

   procedure Pan_To (New_End : Unsigned_64; Damage : in out Rect);
   procedure Pan_To (New_End : Unsigned_64; Damage : in out Rect) is
      Now : constant Unsigned_64 := Feed.Now_Us;
   begin
      if Live_Mode then
         Mark (Damage, Live_Rect);
         Mark (Damage, Status);
      end if;
      Live_Mode := False;
      End_Us := Unsigned_64'Min (Unsigned_64'Max (New_End, Span), Now);
      --  The title's "ending ..." only, not the whole bar.
      Mark (Damage, (Toolbar.x, Toolbar.y, Natural'Max (Preset_Rect (1).x, Toolbar.x) - Toolbar.x, Toolbar.h));
      if Board /= null then
         for P in 1 .. Board.Panels_Used loop
            --  As Set_Span: the focus now, the rest at the next frame.
            if P = Focus_Panel then
            declare
               Port : constant Axis.Viewport := Viewport_For (P);
               Old : constant Axis.Viewport := Ports (P);
               First, Last : Natural;
            begin
               Tracks_Of (P, First, Last);
               for I in First .. Last loop
                  Dash_Tracks.Follow (Tracks (I).all, Port);
               end loop;
               Ports (P) := Port;
               --  The value axis follows at the next frame (Refresh), not
               --  per pointer event: a pan only shifts pixels.
               case Panels_Kind (P) is
                  when Dash_Model.Time_Series | Dash_Model.Heatmap =>
                     Scroll_Plot (P, Old, Port, Damage);
                  when Dash_Model.Stat => Mark (Damage, Panels (P).Plot);
                  when others => Mark (Damage, Panels (P).Outer);
               end case;
            end;
            end if;
         end loop;
      end if;
   end Pan_To;

   procedure Go_Live (Damage : in out Rect);
   procedure Go_Live (Damage : in out Rect) is
   begin
      Live_Mode := True;
      Mark_Plots (Damage);
      Mark (Damage, Toolbar);
      Mark (Damage, Status);
      Set_Span (Span, 0, Damage);
   end Go_Live;

   procedure Set_Hover (X, Y : Natural; Damage : in out Rect);
   procedure Set_Hover (X, Y : Natural; Damage : in out Rect) is
      P : constant Natural := Panel_At (X, Y);
      Old_Box : constant Rect := Tooltip;
      Old_Time : constant Unsigned_64 := Hover_Time;
      Old_Panel : constant Natural := Hover_Panel;
      Was_Valid : constant Boolean := Hover_Valid;
   begin
      Hover_X := X;
      Hover_Y := Y;
      if P = 0 then
         Hover_Valid := False;
         Hover_Panel := 0;
      else
         Hover_Panel := P;
         Hover_Valid := True;
         Hover_Time := Time_At (P, X);
      end if;
      Tooltip := Tooltip_Box;
      if Was_Valid then
         Mark (Damage, Old_Box);
      Mark (Damage, Crosshair_Damage (Old_Panel, Old_Time));
      end if;
      if Hover_Valid then
         Mark (Damage, Tooltip);
      Mark (Damage, Crosshair_Damage (Hover_Panel, Hover_Time));
      end if;
   end Set_Hover;

   --  The editor's pointer: a press on a panel selects it; a drag moves it
   --  (it takes the place of the panel under the pointer) or, from the
   --  grip, sizes it by whole grid units. One drag undoes as one edit.
   Edit_Drag : Boolean := False;
   Edit_Resize : Boolean := False;
   Drag_Width, Drag_Height : Natural := 0;

   procedure Select_Panel (P : Natural; Damage : in out Rect);
   procedure Select_Panel (P : Natural; Damage : in out Rect) is
      Was : constant Natural := Selected_Panel;
   begin
      Selected_Panel := P;
      Mark_Outline (Was, Damage);
      Mark_Outline (P, Damage);
      Mark (Damage, Status);
      if Source_Shown then
         Mark_Exactly (Damage, Overlay_Box);
      end if;
   end Select_Panel;

   procedure Set_Editing (On : Boolean; Damage : in out Rect);
   procedure Set_Editing (On : Boolean; Damage : in out Rect) is
      Hovered : constant Natural := (if Hover_Valid then Hover_Panel else 0);
   begin
      Edit_Mode := On;
      if Hover_Valid then
         --  No tooltip or crosshair while editing.
         Mark (Damage, Tooltip);
         Mark (Damage, Crosshair_Damage (Hover_Panel, Hover_Time));
         Hover_Valid := False;
      end if;
      if On and then Selected_Panel not in 1 .. Board.Panels_Used then
         Selected_Panel := (if Hovered /= 0 then Hovered else 1);
      end if;
      if not On then
         if Source_Shown then
            Source_Shown := False;
            Mark (Damage, Whole);
         end if;
         Dash_Edit.End_Group;
         Edit_Drag := False;
      end if;
      Mark_Outline (Selected_Panel, Damage);
      Mark (Damage, Edit_Rect);
      Mark (Damage, Status);
   end Set_Editing;

   procedure Edit_Drag_To (X, Y : Natural; Damage : in out Rect);
   procedure Edit_Drag_To (X, Y : Natural; Damage : in out Rect) is
      function Units (Moved, Unit : Integer) return Integer is
        (if Unit = 0 then 0 elsif Moved >= 0 then (Moved + Unit / 2) / Unit else -((Unit / 2 - Moved) / Unit));
   begin
      if Selected_Panel not in 1 .. Board.Panels_Used then
         return;
      end if;
      if Edit_Resize then
         declare
            Want_W : constant Integer := Drag_Width + Units (X - Press_X, Grid_Unit_W);
            Want_H : constant Integer := Drag_Height + Units (Y - Press_Y, Grid_Unit_H);
         begin
            while Board.Panels (Selected_Panel).Width < Integer'Min (Want_W, Dash_Model.Grid_Columns) loop
               Edit (Dash_Edit.Wider, Damage);
            end loop;
            while Board.Panels (Selected_Panel).Width > Integer'Max (Want_W, 1) loop
               Edit (Dash_Edit.Narrower, Damage);
            end loop;
            while Board.Panels (Selected_Panel).Height < Integer'Min (Want_H, Dash_Model.Maximum_Grid_Height) loop
               Edit (Dash_Edit.Taller, Damage);
            end loop;
            while Board.Panels (Selected_Panel).Height > Integer'Max (Want_H, 1) loop
               Edit (Dash_Edit.Shorter, Damage);
            end loop;
         end;
      else
         declare
            Target : constant Natural := Card_At (X, Y);
         begin
            if Target /= 0 then
               while Selected_Panel > Target loop
                  Edit (Dash_Edit.Move_Earlier, Damage);
               end loop;
               while Selected_Panel < Target loop
                  Edit (Dash_Edit.Move_Later, Damage);
               end loop;
            end if;
         end;
      end if;
   end Edit_Drag_To;

   --  A key while editing; False: not an editor key (the view's own then).
   procedure Edit_Key (E : Event; Damage : in out Rect; Taken : out Boolean);
   procedure Edit_Key (E : Event; Damage : in out Rect; Taken : out Boolean) is
   begin
      Taken := True;
      case E.Pressed is
         when Left => Edit ((if E.Shift then Dash_Edit.Narrower else Dash_Edit.Move_Earlier), Damage);
         when Right => Edit ((if E.Shift then Dash_Edit.Wider else Dash_Edit.Move_Later), Damage);
         when Up => Edit ((if E.Shift then Dash_Edit.Shorter else Dash_Edit.Move_Earlier), Damage);
         when Down => Edit ((if E.Shift then Dash_Edit.Taller else Dash_Edit.Move_Later), Damage);
         when Next_Panel =>
            Select_Panel ((if E.Shift then (if Selected_Panel <= 1 then Board.Panels_Used else Selected_Panel - 1)
                           else (if Selected_Panel >= Board.Panels_Used then 1 else Selected_Panel + 1)), Damage);
         when Remove => Edit (Dash_Edit.Remove, Damage);
         when Duplicate => Edit (Dash_Edit.Duplicate, Damage);
         when Escape | Edit =>
            if Source_Shown and then E.Pressed = Escape then
               Source_Shown := False;
               Mark (Damage, Whole);
            else
               Set_Editing (False, Damage);
            end if;
         when others => Taken := False;
      end case;
   end Edit_Key;

   ---------------------------------------------------------------------------
   --  The menu bar
   ---------------------------------------------------------------------------
   function Shown_Range return Natural is
   begin
      for I in Preset_Index loop
         if Span = Presets (I) then
            return Natural (I);
         end if;
      end loop;
      return 0;
   end Shown_Range;

   procedure Rebuild_Menus is
      Flags : Commands.State;
   begin
      if Board = null then
         return;
      end if;
      Flags :=
        (Can_Undo => Dash_Edit.Can_Undo, Can_Redo => Dash_Edit.Can_Redo, Editing => Edit_Mode,
         Has_Selection => Selected_Panel in 1 .. Board.Panels_Used,
         Can_Add => Board.Panels_Used < Dash_Model.Maximum_Panels, Live => Live_Mode,
         Source_Shown => Source_Shown and then Shown_Overlay = Source_Overlay,
         Keys_Shown => Source_Shown and then Shown_Overlay = Keys_Overlay,
         Folder_Ready => Folder.Ready, Shown_Range => Shown_Range, Recent => [others => null],
         Recent_Used => Recent_Used);
      for R in 1 .. Recent_Used loop
         Flags.Recent (R) := Menus.Text (Recent_Captions (R));
      end loop;
      Menu_Model := Commands.Menu_Bar (Flags);
      Menu_Width := Commands.Popup_Width (Menu_Model);
   end Rebuild_Menus;

   --  How the menu bar looks: what changed is drawn again.
   type Menu_Look is record
      Open, Selected, Hot : Natural := 0;
      Cues : Boolean := False;
      Popup : Rect := (others => 0);
   end record;
   function Look return Menu_Look is
     ((Menus.Open_Menu (Menu_State), Menus.Selected_Item (Menu_State), Menus.Hot_Title (Menu_State),
       Menus.Cues_Shown (Menu_State), Menu_Popup));

   procedure Close_Picker (Damage : in out Rect);

   procedure Menu_Moved (Before : Menu_Look; Damage : in out Rect);
   procedure Menu_Moved (Before : Menu_Look; Damage : in out Rect) is
      After : constant Menu_Look := Look;
   begin
      if After = Before then
         return;
      end if;
      if After.Open /= 0 and then Picker_On then
         Close_Picker (Damage);
      end if;
      if After.Open /= Before.Open or else After.Hot /= Before.Hot or else After.Cues /= Before.Cues then
         Mark_Exactly (Damage, Menu_Area);
      end if;
      if After.Popup /= Before.Popup then
         --  What the old menu covered comes back, then the new one.
         Mark_Exactly (Damage, Before.Popup);
         Mark_Exactly (Damage, After.Popup);
      elsif After.Selected /= Before.Selected or else After.Cues /= Before.Cues then
         Mark_Exactly (Damage, After.Popup);
      end if;
      if After.Open /= Before.Open or else After.Popup /= Before.Popup then
         Register;
      end if;
   end Menu_Moved;

   procedure Dismiss_Menu (Damage : in out Rect);
   procedure Dismiss_Menu (Damage : in out Rect) is
      Before : constant Menu_Look := Look;
   begin
      Menus.Dismiss (Menu_State);
      Menu_Moved (Before, Damage);
   end Dismiss_Menu;

   procedure Set_Notice (S : String; Damage : in out Rect);
   procedure Set_Notice (S : String; Damage : in out Rect) is
   begin
      Set_Notice_Text (S);
      Mark (Damage, Status);
   end Set_Notice;

   ---------------------------------------------------------------------------
   --  The picker
   ---------------------------------------------------------------------------
   function Name_Listed (Name : String) return Boolean is
     (for some K in 1 .. Catalog_Used => Dash_Model.Image (Catalog (K).Name) = Name);
   function Group_Size (Group : String) return Natural is
      Count : Natural := 0;
   begin
      for K in 1 .. Catalog_Used loop
         declare
            Name : constant String := Dash_Model.Image (Catalog (K).Name);
         begin
            if Name'Length > Group'Length and then Name (Name'First .. Name'First + Group'Length - 1) = Group then
               Count := Count + 1;
            end if;
         end;
      end loop;
      return Count;
   end Group_Size;

   procedure Build_Picker;
   procedure Build_Picker is
      P : Dash_Model.Panel renames Board.Panels (Picker_Target);
      Current : Dash_Choices.Source_Choice;
      One : Boolean;
      Selected_Series, Selected_View : Natural := 0;
      Shown : Dash_Choices.View_Choice;
      Shown_Found : Boolean;
      Last_Group : Dash_Model.Text;
      procedure Add_Series (Source : Dash_Choices.Source_Choice; Caption : String) is
      begin
         if Series_Model.Count < CB.Max_Choices then
            Series_Model.Count := Series_Model.Count + 1;
            Series_Captions (Series_Model.Count) := new String'(Caption);
            Series_Model.Choices (Series_Model.Count) := (Caption => CB.Text (Series_Captions (Series_Model.Count)),
                                                          Enabled => True);
            Series_Sources (Series_Model.Count) := Source;
            if One and then Dash_Model.Image (Source.Name) = Dash_Model.Image (Current.Name)
              and then Source.Match = Current.Match
            then
               Selected_Series := Series_Model.Count;
            end if;
         end if;
      end Add_Series;
   begin
      for C of Series_Captions loop
         if C /= null then
            Free (C);
         end if;
      end loop;
      Series_Model := (others => <>);
      Dash_Choices.Source_Of (P, Current, One);
      Series_Keeps := not One
        or else (if Current.Match = Dash_Model.Exact then not Name_Listed (Dash_Model.Image (Current.Name))
                 else Group_Size (Dash_Model.Image (Current.Name)) = 0);
      if Series_Keeps then
         --  What the panel reads now, kept as it is.
         Add_Series (Current, (if P.Series_Used = 0 then "Choose a series"
                               elsif One then Dash_Choices.Caption (Current) & " (not listed)"
                               else "Its" & Natural'Image (P.Series_Used) & " series"));
         Selected_Series := 1;
      end if;
      for K in 1 .. Catalog_Used loop
         declare
            Name : constant String := Dash_Model.Image (Catalog (K).Name);
            Group : constant String := Dash_Choices.Group_Of (Name);
         begin
            --  A group of two or more, before its first member.
            if Group'Length > 0 and then Dash_Model.Image (Last_Group) /= Group and then Group_Size (Group) > 1 then
               Last_Group := Dash_Model.To_Text (Group);
               Add_Series ((Name => Dash_Model.To_Text (Group), Match => Dash_Model.Prefix),
                           Dash_Choices.Caption (Dash_Choices.Source_Choice'(Dash_Model.To_Text (Group),
                                                                             Dash_Model.Prefix))
                           & "  (" & Format.Decimal (Unsigned_64 (Group_Size (Group))) & " series)");
            end if;
            Add_Series ((Name => Catalog (K).Name, Match => Dash_Model.Exact), Name);
         end;
      end loop;
      --  The views.
      View_Model := (others => <>);
      Dash_Choices.View_Of (P, Shown, Shown_Found);
      View_Keeps := not Shown_Found;
      if View_Keeps then
         View_Model.Count := 1;
         View_Model.Choices (1) := (Caption => Keep_View_Caption'Access, Enabled => True);
         Selected_View := 1;
      end if;
      for Choice in Dash_Choices.View_Choice loop
         if View_Captions (Choice) = null then
            View_Captions (Choice) := new String'(Dash_Choices.Caption (Choice));
         end if;
         View_Model.Count := View_Model.Count + 1;
         View_Model.Choices (View_Model.Count) := (Caption => CB.Text (View_Captions (Choice)), Enabled => True);
         View_Choices (View_Model.Count) := Choice;
         if Shown_Found and then Dash_Choices."=" (Choice, Shown) then
            Selected_View := View_Model.Count;
         end if;
      end loop;
      declare
         Series_Was_Open : constant Boolean := CB.Is_Open (Series_Combo);
         View_Was_Open : constant Boolean := CB.Is_Open (View_Combo);
      begin
         CB.Set_Selection (Series_Combo, Series_Model, Selected_Series);
         CB.Set_Selection (View_Combo, View_Model, Selected_View);
         --  A list stays open across a change it made.
         if Series_Was_Open then
            declare
               Changed, Handled : Boolean;
            begin
               CB.Handle_Key (Series_Combo, Series_Model, CB.Toggle, Changed, Handled);
            end;
         end if;
         if View_Was_Open then
            declare
               Changed, Handled : Boolean;
            begin
               CB.Handle_Key (View_Combo, View_Model, CB.Toggle, Changed, Handled);
            end;
         end if;
      end;
      Built_Generation := Catalog_Generation;
   end Build_Picker;

   procedure Open_Picker (P : Natural; Damage : in out Rect);
   procedure Open_Picker (P : Natural; Damage : in out Rect) is
   begin
      if Board = null or else P not in 1 .. Board.Panels_Used then
         return;
      end if;
      if Menus.Is_Open (Menu_State) then
         Dismiss_Menu (Damage);
      end if;
      if Picker_On then
         Close_Picker (Damage);
      end if;
      if Hover_Valid then
         Mark (Damage, Tooltip);
         Mark (Damage, Crosshair_Damage (Hover_Panel, Hover_Time));
         Hover_Valid := False;
      end if;
      Picker_On := True;
      Picker_Target := P;
      Picker_Focus := Series_Field;
      CB.Dismiss (Series_Combo);
      CB.Dismiss (View_Combo);
      Build_Picker;
      --  The store's newest catalog, read by the data thread meanwhile.
      Catalog_Wanted := True;
      Next_Catalog_Us := Feed.Now_Us + Catalog_Refresh_Us;
      Mark (Damage, Source_Button (P));
      Mark_Exactly (Damage, Picker_Cover);
      Mark (Damage, Status);
      Register;
   end Open_Picker;

   procedure Close_Picker (Damage : in out Rect) is
      Cover : constant Rect := Picker_Cover;
      P : constant Natural := Picker_Target;
   begin
      if not Picker_On then
         return;
      end if;
      Picker_On := False;
      CB.Dismiss (Series_Combo);
      CB.Dismiss (View_Combo);
      Mark_Exactly (Damage, Cover);
      if Board /= null and then P in 1 .. Board.Panels_Used then
         Mark (Damage, Source_Button (P));
      end if;
      Mark (Damage, Status);
      Register;
   end Close_Picker;

   --  The lists' places before an input (Note_Lists), for what a list
   --  that closed covered.
   Before_Series_List, Before_View_List : Rect := (others => 0);
   procedure Note_Lists;
   procedure Note_Lists is
   begin
      Before_Series_List :=
        (if Picker_On and then CB.Is_Open (Series_Combo) then Picker_List else (others => 0));
      Before_View_List :=
        (if Picker_On and then CB.Is_Open (View_Combo) then Picker_List else (others => 0));
   end Note_Lists;

   --  The picker's look changed: its card and its list (and what a list
   --  that closed covered).
   procedure Picker_Moved (Before : Rect; Damage : in out Rect);
   procedure Picker_Moved (Before : Rect; Damage : in out Rect) is
      After : constant Rect := Picker_Cover;
   begin
      if After /= Before then
         --  A list opened or closed: the card, the list, and what a list
         --  that closed covered (each on its own: wholly under a layer,
         --  nothing beneath is drawn).
         Mark_Exactly (Damage, Picker_Box);
         Mark_Exactly (Damage, Picker_List);
         for Field in Picker_Field loop
            declare
               Old_List : constant Rect :=
                 (if Field = Series_Field then Before_Series_List else Before_View_List);
            begin
               Mark_Exactly (Damage, Old_List);
            end;
         end loop;
         Register;
      else
         --  The fields and the open list; the card's text stays.
         Mark_Exactly (Damage, Field_Box (Series_Field));
         Mark_Exactly (Damage, Field_Box (View_Field));
         Mark_Exactly (Damage, Picker_List);
      end if;
   end Picker_Moved;

   --  The panel picked: Item replaces it (one edit).
   procedure Apply_Pick (Item : Dash_Model.Panel; Damage : in out Rect);
   procedure Apply_Pick (Item : Dash_Model.Panel; Damage : in out Rect) is
      D : Dash_Model.Dashboard := Board.all;
      Done : Boolean;
      Was : constant Natural :=
        (if Selected_Panel in 1 .. Board.Panels_Used then Board.Panels (Selected_Panel).Id else 0);
   begin
      Dash_Edit.Replace (D, Picker_Target, Item, Done);
      if Done then
         Apply_Dashboard (D, Damage, Was);
         Build_Picker;
         --  The card names the panel (its title may follow the pick).
         Mark_Exactly (Damage, Picker_Box);
      end if;
   end Apply_Pick;

   procedure Pick (Field : Picker_Field; Damage : in out Rect);
   procedure Pick (Field : Picker_Field; Damage : in out Rect) is
      P : constant Dash_Model.Panel := Board.Panels (Picker_Target);
   begin
      case Field is
         when Series_Field =>
            declare
               I : constant Natural := CB.Selection (Series_Combo);
            begin
               if I in 1 .. Series_Model.Count and then not (Series_Keeps and then I = 1) then
                  Apply_Pick (Dash_Choices.With_Source (P, Series_Sources (I)), Damage);
               end if;
            end;
         when View_Field =>
            declare
               I : constant Natural := CB.Selection (View_Combo);
            begin
               if I in 1 .. View_Model.Count and then not (View_Keeps and then I = 1) then
                  Apply_Pick (Dash_Choices.With_View (P, View_Choices (I), Tops (Picker_Target)), Damage);
               end if;
            end;
      end case;
   end Pick;

   --  A key while the picker is open: it takes them all (Ctrl+Z and Ctrl+Y
   --  undo and redo its edits).
   procedure Picker_Key (E : Event; Damage : in out Rect);
   procedure Picker_Key (E : Event; Damage : in out Rect) is
      Before : constant Rect := Picker_Cover;
      Series_Was : constant CB.Combo_State := Series_Combo;
      View_Was : constant CB.Combo_State := View_Combo;
      Focus_Was : constant Picker_Field := Picker_Focus;
      Changed, Handled : Boolean := False;
      procedure Send (Key : CB.Key; Letter : Character := ' ') is
      begin
         case Picker_Focus is
            when Series_Field => CB.Handle_Key (Series_Combo, Series_Model, Key, Changed, Handled, Letter);
            when View_Field => CB.Handle_Key (View_Combo, View_Model, Key, Changed, Handled, Letter);
         end case;
      end Send;
      function Focused_Open return Boolean is
        (case Picker_Focus is
           when Series_Field => CB.Is_Open (Series_Combo),
           when View_Field => CB.Is_Open (View_Combo));
   begin
      Note_Lists;
      case E.Pressed is
         when Escape =>
            if Focused_Open then
               Send (CB.Cancel);
            else
               Close_Picker (Damage);
               return;
            end if;
         when Next_Panel =>
            Send (CB.Tab_Key);
            Picker_Focus := (if Picker_Focus = Series_Field then View_Field else Series_Field);
         when Up => Send (CB.Up);
         when Down => Send ((if E.Alt then CB.Toggle else CB.Down));
         when Home => Send (CB.Home);
         when End_Key => Send (CB.End_Key);
         when Enter | Space => Send ((if Focused_Open then CB.Commit else CB.Toggle));
         when Undo | Redo =>
            Undo_Redo (E.Pressed = Redo, Damage);
            if Picker_Target in 1 .. Board.Panels_Used then
               Build_Picker;
               Mark_Exactly (Damage, Picker_Box);
            else
               Close_Picker (Damage);
               return;
            end if;
         when others =>
            if E.Letter /= ' ' and then not E.Control and then not E.Alt then
               Send (CB.Type_Character, E.Letter);
            end if;
      end case;
      if Changed then
         Pick (Picker_Focus, Damage);
      end if;
      if Picker_On and then (Changed or else Series_Combo /= Series_Was or else View_Combo /= View_Was
                             or else Picker_Focus /= Focus_Was or else Picker_Cover /= Before)
      then
         Picker_Moved (Before, Damage);
      end if;
   end Picker_Key;

   ---------------------------------------------------------------------------
   --  Dashboards: new, open, save as, duplicate
   ---------------------------------------------------------------------------
   function Listed (Leaf : String) return Boolean is
     (for some K in 1 .. Folder.Count => Files.Value (Folder.Entries (K).Leaf) = Leaf);

   --  Base, or Base and a number, whose file is not in the folder.
   function Unique_Title (Base : String) return String is
      Kept : constant String :=
        Base (Base'First .. Base'First + Natural'Min (Base'Length, Dash_Model.Maximum_Text / 2) - 1);
   begin
      if not Listed (Dash_Model.File_Leaf (Kept)) then
         return Kept;
      end if;
      for N in 2 .. Folder.Count + 2 loop
         declare
            Candidate : constant String := Kept & " " & Format.Decimal (Unsigned_64 (N));
         begin
            if not Listed (Dash_Model.File_Leaf (Candidate)) then
               return Candidate;
            end if;
         end;
      end loop;
      return Kept;
   end Unique_Title;

   function Stem (Leaf : String) return String is
     (if Leaf'Length > Dash_Model.Dashboard_Suffix'Length
        and then Leaf (Leaf'Last - Dash_Model.Dashboard_Suffix'Length + 1 .. Leaf'Last) = Dash_Model.Dashboard_Suffix
      then Leaf (Leaf'First .. Leaf'Last - Dash_Model.Dashboard_Suffix'Length) else Leaf);

   --  D replaces the dashboard shown (opened, new, a copy): everything is
   --  laid out and drawn again, its tracks are new, its edits start over.
   --  Edits of the one left that are not yet written are saved first.
   procedure Replace_Board (D : Dash_Model.Dashboard; Leaf : String; Damage : in out Rect);
   procedure Replace_Board (D : Dash_Model.Dashboard; Leaf : String; Damage : in out Rect) is
      Count : Natural := 0;
      Accepted : Boolean;
   begin
      if Edit_Count /= Saved_Generation and then not (Save_Pending and then In_Flight_Generation = Edit_Count)
        and then Parting_Source = null
      then
         Parting_Source := new String'(Dash_Model.To_Source (Board.all) & ASCII.LF);
         Parting_Name := Current_Leaf;
      end if;
      Close_Picker (Damage);
      if Menus.Is_Open (Menu_State) then
         Dismiss_Menu (Damage);
      end if;
      for T of Tracks.all loop
         Retire ((Track => T, List => null));
      end loop;
      Retire ((Track => null, List => Tracks));
      for P in 1 .. D.Panels_Used loop
         for S in 1 .. D.Panels (P).Series_Used loop
            Count := Count + (if D.Panels (P).Series (S).Match = Dash_Model.Prefix then Prefix_Tracks else 1);
         end loop;
      end loop;
      Tracks := new Track_Array'(1 .. Count => null);
      Count := 0;
      for P in 1 .. D.Panels_Used loop
         for S in 1 .. D.Panels (P).Series_Used loop
            for Match in 1 .. (if D.Panels (P).Series (S).Match = Dash_Model.Prefix then Prefix_Tracks else 1) loop
               Count := Count + 1;
               Tracks (Count) := new Dash_Tracks.Track;
               Tracks (Count).Panel := P;
               Tracks (Count).Panel_Id := D.Panels (P).Id;
               Tracks (Count).Spec_Index := S;
               Tracks (Count).Match_Index := Match;
               Tracks (Count).Spec := D.Panels (P).Series (S);
            end loop;
         end loop;
      end loop;
      if Drawn_States /= null then
         Free (Drawn_States);
      end if;
      Board.all := D;
      Layout_Generation := Layout_Generation + 1;
      Span := Unsigned_64'Max (Axis.Minimum_Span_Us,
                               Unsigned_64'Min (D.Span * Microseconds_Per_Millisecond, Axis.Maximum_Span_Us));
      Live_Mode := D.Live;
      End_Us := Feed.Now_Us;
      Has_Port := [others => False];
      Tops := [others => 1];
      Lows := [others => 1];
      Edit_Mode := False;
      Selected_Panel := 0;
      Source_Shown := False;
      Hover_Valid := False;
      Detail_Valid := False;
      Pressed := False;
      Selecting := False;
      Edit_Drag := False;
      Dash_Edit.End_Group;
      Rows_Basis := 0;
      Lay_Out;
      Update_Quantum;
      Dash_Edit.Start (D);
      Board_First_Generation := Edit_Count + 1;
      Saved_Generation := Edit_Count;
      Shown_Save := Unedited;
      Files.Set (Current_Leaf, Leaf, Accepted);
      Rebuild_Menus;
      Register;
      Mark_Exactly (Damage, Whole);
   end Replace_Board;

   --  The dashboard counts as edited (a copy not yet written).
   procedure Note_Edited;
   procedure Note_Edited is
   begin
      Edit_Count := Edit_Count + 1;
      Last_Edit_Us := Feed.Now_Us;
      Shown_Save := Edited;
   end Note_Edited;

   --  A new panel in the first free place, selected, its picker open.
   procedure Add_Panel_Now (Damage : in out Rect);
   procedure Add_Panel_Now (Damage : in out Rect) is
      D : Dash_Model.Dashboard := Board.all;
      At_Index : Dash_Model.Panel_Index;
      Where : Dash_Grid.Placement;
      Done : Boolean;
      Was : constant Natural :=
        (if Selected_Panel in 1 .. Board.Panels_Used then Board.Panels (Selected_Panel).Id else 0);
   begin
      if D.Panels_Used = Dash_Model.Maximum_Panels then
         Set_Notice ("A dashboard holds" & Natural'Image (Dash_Model.Maximum_Panels) & " panels at most", Damage);
         return;
      end if;
      Close_Picker (Damage);
      Dash_Grid.First_Free (D, Dash_Choices.New_Width, Dash_Choices.New_Height, At_Index, Where);
      Dash_Edit.Insert (D, Dash_Choices.New_Panel, At_Index, Done);
      if not Done then
         return;
      end if;
      if not Edit_Mode then
         Set_Editing (True, Damage);
      end if;
      Selected_Panel := At_Index;
      Apply_Dashboard (D, Damage, Was);
      Mark_Outline (At_Index, Damage);
      Open_Picker (At_Index, Damage);
   end Add_Panel_Now;

   --  The file dialog over the window.
   procedure Show_Dialog (Mode : Dialogs.Dialog_Mode; Damage : in out Rect);
   procedure Show_Dialog (Mode : Dialogs.Dialog_Mode; Damage : in out Rect) is
      Names : Files.File_List;
      Accepted : Boolean;
      Suggested : constant String :=
        (if Mode = Dialogs.Save_New_File then Stem (Files.Value (Current_Leaf)) & "-copy" else "");
      function Free_Name (Base : String) return String is
      begin
         if not Listed (Base & Dash_Model.Dashboard_Suffix) then
            return Base & Dash_Model.Dashboard_Suffix;
         end if;
         for N in 2 .. Folder.Count + 2 loop
            declare
               Candidate : constant String := Base & "-" & Format.Decimal (Unsigned_64 (N)) & Dash_Model.Dashboard_Suffix;
            begin
               if not Listed (Candidate) then
                  return Candidate;
               end if;
            end;
         end loop;
         return Base & Dash_Model.Dashboard_Suffix;
      end Free_Name;
   begin
      Close_Picker (Damage);
      if Menus.Is_Open (Menu_State) then
         Dismiss_Menu (Damage);
      end if;
      for K in 1 .. Folder.Count loop
         Files.Append (Names, Files.Value (Folder.Entries (K).Leaf), Accepted);
      end loop;
      Dialogs.Show
        (Dialog, Mode, Names, Folder_Label,
         Suggested_Name => (if Mode = Dialogs.Save_New_File then Free_Name (Suggested) else ""),
         Title => (if Mode = Dialogs.Open_File then "Open a dashboard" else "Save the dashboard as a new file"),
         Kind => Dashboard_Kind,
         Empty => "No saved dashboards yet");
      Dialog_Down := False;
      if Hover_Valid then
         Mark (Damage, Tooltip);
         Hover_Valid := False;
      end if;
      Mark_Exactly (Damage, Dialog_Box);
   end Show_Dialog;

   procedure Close_Dialog (Damage : in out Rect);
   procedure Close_Dialog (Damage : in out Rect) is
      Box : constant Rect := Dialog_Box;
   begin
      Dialogs.Close (Dialog);
      Dialog_Down := False;
      Mark_Exactly (Damage, Box);
   end Close_Dialog;

   --  The dialog's name, as a dashboard's file: ".ccl" added when missing.
   function Dashboard_Leaf (Name : String) return String is
     (if Stem (Name) = Name then Name & Dash_Model.Dashboard_Suffix else Name);

   procedure Submit_Dialog (Damage : in out Rect);
   procedure Submit_Dialog (Damage : in out Rect) is
      Leaf : constant String := Dashboard_Leaf (Dialogs.Filename (Dialog));
      Accepted : Boolean;
   begin
      if not Files.Valid_Leaf (Leaf) then
         Dialogs.Set_Error (Dialog, "Use letters, digits, spaces, . _ and - (no folders), at most"
                            & Natural'Image (Files.Maximum_Name_Length) & " characters");
      elsif Dialogs.Mode (Dialog) = Dialogs.Open_File then
         Files.Set (Want_Open, Leaf, Accepted);
         Dialogs.Set_Error (Dialog, "Opening " & Leaf & "...");
      elsif Listed (Leaf) then
         Dialogs.Set_Error (Dialog, "There is a " & Leaf & " already: choose another name");
      else
         Files.Set (Want_Save, Leaf, Accepted);
         Dialogs.Set_Error (Dialog, "Saving " & Leaf & "...");
      end if;
      Mark_Exactly (Damage, Dialog_Box);
   end Submit_Dialog;

   --  A command that needs the folder's listing, once it is in.
   procedure Run_Deferred (Which : Commands.Command; Damage : in out Rect);
   procedure Run_Deferred (Which : Commands.Command; Damage : in out Rect) is
   begin
      case Which is
         when Commands.Open_Dashboard | Commands.Save_As =>
            if not Folder.Ready then
               Set_Notice ("Can't " & (if Which = Commands.Save_As then "save" else "open")
                           & ": this app has no folder for dashboards", Damage);
            else
               Show_Dialog ((if Which = Commands.Save_As then Dialogs.Save_New_File else Dialogs.Open_File), Damage);
            end if;
         when Commands.New_Dashboard =>
            declare
               Title : constant String := Unique_Title (Dash_Choices.New_Dashboard_Title);
            begin
               Replace_Board (Dash_Choices.New_Dashboard (Title), Dash_Model.File_Leaf (Title), Damage);
               Set_Notice ("A new dashboard: pick its first panel's series", Damage);
               Set_Editing (True, Damage);
               Select_Panel (1, Damage);
               Open_Picker (1, Damage);
            end;
         when Commands.Duplicate_Dashboard =>
            declare
               Copy : Dash_Model.Dashboard := Board.all;
               Title : constant String := Unique_Title (Dash_Model.Image (Board.Title) & " copy");
            begin
               Copy.Title := Dash_Model.To_Text (Title);
               Replace_Board (Copy, Dash_Model.File_Leaf (Title), Damage);
               --  A copy is a new file: written at once.
               Note_Edited;
               Set_Notice ("A copy: " & Title & " (" & Dash_Model.File_Leaf (Title) & ")", Damage);
            end;
         when others =>
            null;
      end case;
   end Run_Deferred;

   procedure Toggle_Overlay (Wanted : Overlay_Kind; Damage : in out Rect);
   procedure Toggle_Overlay (Wanted : Overlay_Kind; Damage : in out Rect) is
   begin
      --  The same key again closes it; the other one switches.
      if Source_Shown and then Shown_Overlay /= Wanted then
         --  What the other one covered comes back first.
         Source_Shown := False;
         Mark (Damage, Whole);
         Source_Shown := True;
      else
         Source_Shown := not Source_Shown;
      end if;
      Shown_Overlay := Wanted;
      if Source_Shown then
         Hover_Valid := False;
         Mark_Exactly (Damage, Overlay_Box);
      else
         Mark (Damage, Whole);
      end if;
   end Toggle_Overlay;

   --  The panel a command without a selection acts on: the one under the
   --  pointer, else the first.
   function Target_Panel return Natural is
     (if Edit_Mode and then Selected_Panel in 1 .. Board.Panels_Used then Selected_Panel
      elsif Card_At (Hover_X, Hover_Y) /= 0 then Card_At (Hover_X, Hover_Y)
      elsif Board.Panels_Used > 0 then 1 else 0);

   procedure Run_Command (Item : Natural; Damage : in out Rect);
   procedure Run_Command (Item : Natural; Damage : in out Rect) is
      Which : Commands.Command;
      Recent : Commands.Recent_Count;
   begin
      Commands.Decode (Item, Which, Recent);
      if Recent /= 0 then
         if Recent <= Recent_Used then
            Want_Open := Recent_Leaves (Recent);
            Set_Notice ("Opening " & Files.Value (Recent_Leaves (Recent)) & "...", Damage);
         end if;
         return;
      end if;
      case Which is
         when Commands.No_Command =>
            null;
         when Commands.New_Dashboard | Commands.Open_Dashboard | Commands.Save_As | Commands.Duplicate_Dashboard =>
            --  Each needs the folder as it is now.
            Deferred := Which;
            Want_List := True;
         when Commands.Undo | Commands.Redo =>
            Undo_Redo (Which = Commands.Redo, Damage);
         when Commands.Toggle_Editor =>
            Set_Editing (not Edit_Mode, Damage);
         when Commands.Add_Panel =>
            Add_Panel_Now (Damage);
         when Commands.Panel_Source =>
            Open_Picker (Target_Panel, Damage);
         when Commands.Duplicate_Panel =>
            Edit (Dash_Edit.Duplicate, Damage);
         when Commands.Remove_Panel =>
            Edit (Dash_Edit.Remove, Damage);
         when Commands.Copy_Source =>
            Copy_Wanted := True;
         when Commands.Zoom_In =>
            Set_Span (Span * Zoom_Denominator / Zoom_Numerator, 0, Damage);
         when Commands.Zoom_Out =>
            Set_Span (Span * Zoom_Numerator / Zoom_Denominator, 0, Damage);
         when Commands.Range_Command =>
            Set_Span (Presets (Preset_Index (Commands.Command'Pos (Which) - Commands.Command'Pos (Commands.Range_1)
                                             + 1)), 0, Damage);
         when Commands.Go_Live =>
            Go_Live (Damage);
         when Commands.Show_Source =>
            Toggle_Overlay (Source_Overlay, Damage);
         when Commands.Show_Keys =>
            Toggle_Overlay (Keys_Overlay, Damage);
      end case;
   end Run_Command;

   --  The data thread's answers, taken at the UI thread's refresh.
   procedure Take_Answers (Damage : in out Rect) is
   begin
      if List_Answered then
         List_Answered := False;
         Folder := List_Answer;
         --  The dialog lists by name; recent is by change, newest first.
         for I in 2 .. Folder.Count loop
            declare
               Item : constant Folder_Entry := Folder.Entries (I);
               J : Natural := I;
            begin
               while J > 1 and then Files.Value (Folder.Entries (J - 1).Leaf) > Files.Value (Item.Leaf) loop
                  Folder.Entries (J) := Folder.Entries (J - 1);
                  J := J - 1;
               end loop;
               Folder.Entries (J) := Item;
            end;
         end loop;
         for C of Recent_Captions loop
            if C /= null then
               Free (C);
            end if;
         end loop;
         Recent_Used := 0;
         declare
            Taken : array (1 .. Files.Maximum_Files) of Boolean := [others => False];
         begin
            while Recent_Used < Commands.Maximum_Recent and then Recent_Used < Folder.Count loop
               declare
                  Best : Natural := 0;
               begin
                  for K in 1 .. Folder.Count loop
                     if not Taken (K) and then (Best = 0 or else Folder.Entries (K).Modified > Folder.Entries (Best).Modified)
                     then
                        Best := K;
                     end if;
                  end loop;
                  Taken (Best) := True;
                  Recent_Used := Recent_Used + 1;
                  Recent_Leaves (Recent_Used) := Folder.Entries (Best).Leaf;
                  Recent_Captions (Recent_Used) := new String'(Stem (Files.Value (Folder.Entries (Best).Leaf)));
               end;
            end loop;
         end;
         declare
            Before : constant Menu_Look := Look;
         begin
            Rebuild_Menus;
            if Menus.Is_Open (Menu_State) then
               Mark_Exactly (Damage, Before.Popup);
               Menu_Moved (Before, Damage);
               Mark_Exactly (Damage, Menu_Popup);
               Register;
            end if;
         end;
         if Deferred /= Commands.No_Command then
            declare
               Which : constant Commands.Command := Deferred;
            begin
               Deferred := Commands.No_Command;
               Run_Deferred (Which, Damage);
            end;
         end if;
      end if;
      if Open_Answered then
         Open_Answered := False;
         if Open_Done then
            if Dialogs.Is_Open (Dialog) then
               Close_Dialog (Damage);
            end if;
            Replace_Board (Open_Board.all, Files.Value (Open_Leaf), Damage);
            Set_Notice ("Opened " & Files.Value (Open_Leaf), Damage);
            Want_List := True;
         else
            declare
               Why : constant String := Files.Value (Open_Leaf) & ": " & Answer_Reason (1 .. Answer_Reason_Length);
            begin
               if Dialogs.Is_Open (Dialog) then
                  Dialogs.Set_Error (Dialog, Why);
                  Mark_Exactly (Damage, Dialog_Box);
               end if;
               Set_Notice (Why, Damage);
            end;
         end if;
      end if;
      if Save_Answered then
         Save_Answered := False;
         if Save_Done then
            if Dialogs.Is_Open (Dialog) then
               Close_Dialog (Damage);
            end if;
            declare
               Accepted : Boolean;
            begin
               Files.Set (Current_Leaf, Files.Value (Save_Leaf), Accepted);
            end;
            Saved_Generation := Edit_Count;
            Shown_Save := Saved;
            Set_Notice ("Saved as " & Files.Value (Save_Leaf), Damage);
            Want_List := True;
         else
            declare
               Why : constant String := Files.Value (Save_Leaf) & ": " & Answer_Reason (1 .. Answer_Reason_Length);
            begin
               if Dialogs.Is_Open (Dialog) then
                  Dialogs.Set_Error (Dialog, Why);
                  Mark_Exactly (Damage, Dialog_Box);
               end if;
               Set_Notice (Why, Damage);
            end;
         end if;
      end if;
      if Notice_Length > 0 and then Feed.Now_Us >= Notice_Until_Us then
         Notice_Length := 0;
         Mark (Damage, Status);
      end if;
      if Picker_On then
         if Catalog_Generation /= Built_Generation and then not CB.Is_Open (Series_Combo)
           and then not CB.Is_Open (View_Combo) and then Picker_Target in 1 .. Board.Panels_Used
         then
            Build_Picker;
            Mark_Exactly (Damage, Picker_Cover);
            Register;
         end if;
         if Feed.Now_Us >= Next_Catalog_Us then
            Catalog_Wanted := True;
            Next_Catalog_Us := Feed.Now_Us + Catalog_Refresh_Us;
         end if;
      end if;
   end Take_Answers;

   ---------------------------------------------------------------------------
   --  Input to the menu bar, the picker and the dialog
   ---------------------------------------------------------------------------
   --  The menu bar's keys: F10 opens and closes it, Alt+letter opens a
   --  menu; while one is open, arrows, Enter, Esc, Tab and letters move in
   --  it. Taken: the key was the menu's. Other keys close it and go on.
   procedure Menu_Key (E : Event; Damage : in out Rect; Taken : out Boolean);
   procedure Menu_Key (E : Event; Damage : in out Rect; Taken : out Boolean) is
      Was_Open : constant Boolean := Menus.Is_Open (Menu_State);
      Before : constant Menu_Look := Look;
      Key : Menus.Key := Menus.Mnemonic;
      Chosen : Natural := 0;
      Handled : Boolean;
   begin
      Taken := False;
      if Was_Open then
         case E.Pressed is
            when Menu_Bar => Key := Menus.Activate;
            when Left => Key := Menus.Left;
            when Right => Key := Menus.Right;
            when Up => Key := Menus.Up;
            when Down => Key := Menus.Down;
            when Home => Key := Menus.Home;
            when End_Key => Key := Menus.End_Key;
            when Enter => Key := Menus.Enter;
            when Space => Key := Menus.Space;
            when Escape => Key := Menus.Escape;
            when Next_Panel => Key := Menus.Tab_Key;
            when others =>
               if E.Letter /= ' ' and then not E.Control then
                  Key := Menus.Mnemonic;
               else
                  Dismiss_Menu (Damage);
                  return;
               end if;
         end case;
      elsif E.Pressed = Menu_Bar and then not E.Control and then not E.Shift then
         Key := Menus.Activate;
      elsif E.Alt and then not E.Control and then E.Letter /= ' '
        and then (for some I in 1 .. Menu_Model.Menu_Count => Menu_Model.Menus (I).Mnemonic = E.Letter)
      then
         Key := Menus.Mnemonic;
      else
         return;
      end if;
      Rebuild_Menus;
      Menus.Handle_Key (Menu_State, Menu_Model, Key, Chosen, Handled, E.Letter);
      Taken := Handled or else Was_Open;
      Menu_Moved (Before, Damage);
      if Chosen /= 0 then
         Run_Command (Chosen, Damage);
      end if;
      if Menus.Is_Open (Menu_State) and then not Was_Open then
         --  The recent dashboards as the folder is now.
         Want_List := True;
      end if;
   end Menu_Key;

   --  The menu bar's and the picker's share of a pointer event (the
   --  toolkit's retained routing: Controls first, then the widget). A
   --  press outside an open menu or the picker closes it and is taken,
   --  its release too (no click-through).
   procedure Control_Pointer (E : Event; Damage : in out Rect; Taken : out Boolean);
   procedure Control_Pointer (E : Event; Damage : in out Rect; Taken : out Boolean) is
      Target : constant Controls.Control_ID := Controls.Hit (Map, E.X, E.Y);
      Action : constant Controls.Pointer_Action :=
        (case E.Kind is
           when Pointer_Press => Controls.Pointer_Press,
           when Pointer_Release => Controls.Pointer_Release,
           when others => Controls.Pointer_Move);
      Menu_Open : constant Boolean := Menus.Is_Open (Menu_State);
      Ours : constant Boolean := Menus.Is_Menu_Control (Menu_Base, Target);
      In_Series : constant Boolean := Picker_On and then CB.Is_Combo_Control (Series_Base, Target);
      In_View : constant Boolean := Picker_On and then CB.Is_Combo_Control (View_Base, Target);
      Inside_Picker : constant Boolean := Picker_On and then Point_In_Rect (E.X, E.Y, Picker_Cover);
      Before : constant Menu_Look := Look;
      Before_Picker : constant Rect := Picker_Cover;
      Series_Was : constant CB.Combo_State := Series_Combo;
      View_Was : constant CB.Combo_State := View_Combo;
      Chosen : Natural := 0;
      Changed, Dispatched, Handled : Boolean;
   begin
      Taken := False;
      Note_Lists;
      if Action = Controls.Pointer_Release and then Block_Release then
         Block_Release := False;
         Taken := True;
         return;
      end if;
      if not Menu_Open and then not Ours and then not Picker_On
        and then (Action /= Controls.Pointer_Move or else Menus.Hot_Title (Menu_State) = 0)
      then
         return;
      end if;
      if Action = Controls.Pointer_Press
        and then ((Menu_Open and then not Ours) or else (Picker_On and then not Inside_Picker and then not Ours))
      then
         --  Outside: closed, and taken.
         if Menu_Open then
            Dismiss_Menu (Damage);
         end if;
         if Picker_On then
            Close_Picker (Damage);
         end if;
         Block_Release := True;
         Taken := True;
         return;
      end if;
      Controls.Dispatch_Pointer (Map, Target, Action, E.X, E.Y, Changed, Dispatched);
      if Menu_Open or else Ours or else Menus.Hot_Title (Menu_State) /= 0 then
         Rebuild_Menus;
         Menus.Handle_Pointer (Menu_State, Menu_Model, Map, Menu_Base, Target, Action, Chosen, Handled);
         Taken := Handled or else Menu_Open or else (Ours and then Action /= Controls.Pointer_Move);
         Menu_Moved (Before, Damage);
         if Menus.Is_Open (Menu_State) and then not Menu_Open then
            Want_List := True;
         end if;
         if Chosen /= 0 then
            Run_Command (Chosen, Damage);
         end if;
         if Taken or else not Picker_On then
            return;
         end if;
      end if;
      if Picker_On then
         if In_Series or else (CB.Is_Open (Series_Combo) and then not In_View) then
            CB.Handle_Pointer (Series_Combo, Series_Model, Map, Series_Base, Target, Action, Changed, Handled);
            if Action = Controls.Pointer_Press then
               Picker_Focus := Series_Field;
            end if;
            if Changed then
               Pick (Series_Field, Damage);
            end if;
         elsif In_View or else CB.Is_Open (View_Combo) then
            CB.Handle_Pointer (View_Combo, View_Model, Map, View_Base, Target, Action, Changed, Handled);
            if Action = Controls.Pointer_Press then
               Picker_Focus := View_Field;
            end if;
            if Changed then
               Pick (View_Field, Damage);
            end if;
         end if;
         --  Only what changed (a move over the list that stays on its
         --  row draws nothing).
         if Picker_On and then (Series_Combo /= Series_Was or else View_Combo /= View_Was
                                or else Picker_Cover /= Before_Picker)
         then
            Picker_Moved (Before_Picker, Damage);
         end if;
         Taken := Inside_Picker;
      end if;
   end Control_Pointer;

   function Dialog_Event_Of (E : Event) return Dialogs.Dialog_Event is
      Result : Dialogs.Dialog_Event :=
        (Kind => Dialogs.No_Event, X => E.X, Y => E.Y, Shift => E.Shift, Control => E.Control, others => <>);
   begin
      case E.Kind is
         when Key_Press =>
            Result.Kind :=
              (case E.Pressed is
                 when Left => Dialogs.Left, when Right => Dialogs.Right, when Home => Dialogs.Home,
                 when End_Key => Dialogs.End_Key, when Up => Dialogs.Up, when Down => Dialogs.Down,
                 when Page_Up => Dialogs.Page_Up, when Page_Down => Dialogs.Page_Down, when Enter => Dialogs.Enter,
                 when Escape => Dialogs.Escape, when Backspace => Dialogs.Backspace, when Remove => Dialogs.Delete,
                 when Next_Panel => Dialogs.Tab, when Select_All => Dialogs.Select_All,
                 when others => Dialogs.No_Event);
         when Text_Input =>
            Result.Kind := Dialogs.Text_Input;
            Result.Character_Value := E.Character_Value;
         when Pointer_Press =>
            Result.Kind := Dialogs.Pointer_Down;
         when Pointer_Move =>
            Result.Kind := (if Dialog_Down then Dialogs.Pointer_Drag else Dialogs.No_Event);
         when Pointer_Release =>
            Result.Kind := Dialogs.Pointer_Up;
         when Wheel =>
            Result.Kind := (if E.Steps > 0 then Dialogs.Wheel_Up elsif E.Steps < 0 then Dialogs.Wheel_Down
                            else Dialogs.No_Event);
         when Pointer_Leave | Modifier_Change =>
            null;
      end case;
      return Result;
   end Dialog_Event_Of;

   --  Everything goes to the open dialog (it is modal).
   procedure Dialog_Input (E : Event; Damage : in out Rect);
   procedure Dialog_Input (E : Event; Damage : in out Rect) is
      Item : Dialogs.Dialog_Event := Dialog_Event_Of (E);
      Action : Dialogs.Dialog_Action;
      Now : constant Unsigned_64 := Feed.Now_Us;
   begin
      if Item.Kind = Dialogs.No_Event then
         return;
      end if;
      if E.Kind = Pointer_Press then
         if Now - Last_Press_Us <= Double_Click_Us and then abs (E.X - Last_Press_X) <= Double_Click_Slop
           and then abs (E.Y - Last_Press_Y) <= Double_Click_Slop
         then
            Item.Kind := Dialogs.Double_Click;
            Last_Press_Us := 0;
         else
            Last_Press_Us := Now;
         end if;
         Last_Press_X := E.X;
         Last_Press_Y := E.Y;
         Dialog_Down := True;
      elsif E.Kind = Pointer_Release then
         Dialog_Down := False;
      end if;
      Dialogs.Handle (Dialog, Item, Whole.x + Whole.w, Whole.y + Whole.h, Action);
      case Action is
         when Dialogs.Submit =>
            Submit_Dialog (Damage);
         when Dialogs.Cancelled =>
            Mark_Exactly (Damage, Dialogs.Bounds (Whole.x + Whole.w, Whole.y + Whole.h));
            Dialog_Down := False;
         when others =>
            if Dialogs.Is_Open (Dialog) then
               Mark_Exactly (Damage, Dialog_Box);
            end if;
      end case;
   end Dialog_Input;

   ---------------------------------------------------------------------------
   --  The data thread's side of the dashboards folder
   ---------------------------------------------------------------------------
   procedure Take_Job (V : in out View; J : out Job) is
      pragma Unreferenced (V);
   begin
      J := (others => <>);
      if Want_Save.Length > 0 then
         J := (Save_New_File, Want_Save);
         Want_Save := (others => <>);
      elsif Want_Open.Length > 0 then
         J := (Open_File, Want_Open);
         Want_Open := (others => <>);
      elsif Want_List then
         J.Kind := List_Folder;
         Want_List := False;
      end if;
   end Take_Job;

   procedure Keep_Reason (Reason : String);
   procedure Keep_Reason (Reason : String) is
   begin
      Answer_Reason_Length := Natural'Min (Reason'Length, Maximum_Notice);
      Answer_Reason (1 .. Answer_Reason_Length) := Reason (Reason'First .. Reason'First + Answer_Reason_Length - 1);
   end Keep_Reason;

   procedure End_List (V : in out View; Listing : Folder_Listing) is
      pragma Unreferenced (V);
   begin
      List_Answer := Listing;
      List_Answered := True;
   end End_List;

   procedure End_Open (V : in out View; Leaf : String; D : Dash_Model.Dashboard; Opened : Boolean; Reason : String)
   is
      pragma Unreferenced (V);
      Accepted : Boolean;
   begin
      Files.Set (Open_Leaf, Leaf, Accepted);
      Open_Done := Opened;
      if Opened then
         Open_Board.all := D;
      end if;
      Keep_Reason (Reason);
      Open_Answered := True;
   end End_Open;

   procedure End_Save_New (V : in out View; Leaf : String; Saved : Boolean; Reason : String) is
      pragma Unreferenced (V);
      Accepted : Boolean;
   begin
      Files.Set (Save_Leaf, Leaf, Accepted);
      Save_Done := Saved;
      Keep_Reason (Reason);
      Save_Answered := True;
   end End_Save_New;

   function Take_Copy_Request (V : in out View) return Boolean is
      pragma Unreferenced (V);
      Wanted : constant Boolean := Copy_Wanted;
   begin
      Copy_Wanted := False;
      return Wanted;
   end Take_Copy_Request;

   function Open_Menu (V : View) return Natural is
      pragma Unreferenced (V);
   begin
      return Menus.Open_Menu (Menu_State);
   end Open_Menu;

   function Menu_Bar_Bounds (V : View) return Rect is
      pragma Unreferenced (V);
   begin
      return Menu_Area;
   end Menu_Bar_Bounds;

   function Menu_Popup_Bounds (V : View) return Rect is
      pragma Unreferenced (V);
   begin
      return Menu_Popup;
   end Menu_Popup_Bounds;

   function Picker_Panel (V : View) return Natural is
      pragma Unreferenced (V);
   begin
      return (if Picker_On then Picker_Target else 0);
   end Picker_Panel;

   function Picker_Bounds (V : View) return Rect is
      pragma Unreferenced (V);
   begin
      return Picker_Cover;
   end Picker_Bounds;

   function Source_Button_Bounds (V : View; P : Positive) return Rect is
      pragma Unreferenced (V);
   begin
      return (if Board /= null and then P <= Board.Panels_Used then Source_Button (P) else (others => 0));
   end Source_Button_Bounds;

   function Dialog_Open (V : View) return Boolean is
      pragma Unreferenced (V);
   begin
      return Dialogs.Is_Open (Dialog);
   end Dialog_Open;

   function Notice (V : View) return String is
      pragma Unreferenced (V);
   begin
      return Notice_Text (1 .. Notice_Length);
   end Notice;

   function Picker_Choices (V : View; Field : Picker_Field) return Natural is
      pragma Unreferenced (V);
   begin
      return (if not Picker_On then 0 elsif Field = Series_Field then Series_Model.Count else View_Model.Count);
   end Picker_Choices;

   function Picker_Choice (V : View; Field : Picker_Field; Index : Positive) return String is
      pragma Unreferenced (V);
      M_Of : constant CB.Model := (if Field = Series_Field then Series_Model else View_Model);
   begin
      return (if Picker_On and then Index <= M_Of.Count and then M_Of.Choices (Index).Caption /= null
              then M_Of.Choices (Index).Caption.all else "");
   end Picker_Choice;

   function Picker_Selection (V : View; Field : Picker_Field) return Natural is
      pragma Unreferenced (V);
   begin
      return (if not Picker_On then 0
              elsif Field = Series_Field then CB.Selection (Series_Combo) else CB.Selection (View_Combo));
   end Picker_Selection;

   function Panel_View (V : View; P : Positive; Choice : out Dash_Choices.View_Choice) return Boolean is
      pragma Unreferenced (V);
      Found : Boolean := False;
   begin
      Choice := Dash_Choices.View_Choice'First;
      if Board /= null and then P <= Board.Panels_Used then
         Dash_Choices.View_Of (Board.Panels (P), Choice, Found);
      end if;
      return Found;
   end Panel_View;

   function Selected (V : View) return Natural is
      pragma Unreferenced (V);
   begin
      return Selected_Panel;
   end Selected;

   function Panel_Source (V : View; P : Positive) return String is
      pragma Unreferenced (V);
   begin
      return (if Board /= null and then P <= Board.Panels_Used then Dash_Model.Panel_Source (Board.Panels (P)) else "");
   end Panel_Source;

   procedure Handle_Locked (E : Event; Damage : in out Rect);

   procedure Handle (V : in out View; E : Event; Damage : in out Rect) is
      pragma Unreferenced (V);
   begin
      Lock_UI;
      if E.Kind in Wheel | Key_Press | Pointer_Press then
         --  The person's own range from here on.
         Auto_Fit := False;
      end if;
      Handle_Locked (E, Damage);
      Dash_Sync.Unlock;
   end Handle;

   procedure Handle_Locked (E : Event; Damage : in out Rect) is
   begin
      if Board = null then
         return;
      end if;
      if E.Kind in Modifier_Change | Key_Press then
         --  Alt held: the menu bar's mnemonics show.
         declare
            Before : constant Menu_Look := Look;
         begin
            Menus.Set_Alt_Held (Menu_State, E.Alt);
            Menu_Moved (Before, Damage);
         end;
         if E.Kind = Modifier_Change then
            return;
         end if;
      end if;
      if Dialogs.Is_Open (Dialog) then
         Dialog_Input (E, Damage);
         return;
      elsif E.Kind = Text_Input then
         return;
      end if;
      if E.Kind = Key_Press then
         declare
            Taken : Boolean;
         begin
            Menu_Key (E, Damage, Taken);
            if Taken then
               return;
            end if;
         end;
         if Picker_On then
            Picker_Key (E, Damage);
            return;
         end if;
         case E.Pressed is
            when New_Dashboard => Run_Command (Commands.Command'Pos (Commands.New_Dashboard), Damage);
            when Open_Dashboard => Run_Command (Commands.Command'Pos (Commands.Open_Dashboard), Damage);
            when Save_As => Run_Command (Commands.Command'Pos (Commands.Save_As), Damage);
            when Add_Panel => Add_Panel_Now (Damage);
            when Panel_Source => Open_Picker (Target_Panel, Damage);
            when Enter =>
               if Edit_Mode then
                  Open_Picker (Target_Panel, Damage);
               end if;
            when others =>
               --  Alt and a letter that names no menu: nothing.
               if not E.Alt then
                  goto Keys;
               end if;
         end case;
         return;
      elsif E.Kind in Pointer_Move | Pointer_Press | Pointer_Release then
         declare
            Taken : Boolean;
         begin
            Control_Pointer (E, Damage, Taken);
            if Taken then
               return;
            end if;
         end;
         if E.Kind = Pointer_Press then
            for P in 1 .. Board.Panels_Used loop
               if Point_In_Rect (E.X, E.Y, Source_Button (P)) then
                  if Picker_On and then Picker_Target = P then
                     Close_Picker (Damage);
                  else
                     Open_Picker (P, Damage);
                  end if;
                  Block_Release := True;
                  return;
               end if;
            end loop;
         end if;
      elsif E.Kind = Wheel and then Picker_On then
         declare
            Before : constant Rect := Picker_Cover;
            Handled : Boolean := False;
         begin
            Note_Lists;
            if CB.Is_Open (Series_Combo) then
               CB.Handle_Wheel (Series_Combo, Series_Model, E.Steps, Handled);
            elsif CB.Is_Open (View_Combo) then
               CB.Handle_Wheel (View_Combo, View_Model, E.Steps, Handled);
            end if;
            if Handled then
               Picker_Moved (Before, Damage);
               return;
            elsif Point_In_Rect (E.X, E.Y, Before) then
               return;
            end if;
         end;
      end if;
      <<Keys>>
      case E.Kind is
         when Pointer_Leave =>
            Set_Hover (0, 0, Damage);
            Hover_Valid := False;
         when Pointer_Move =>
            if Edit_Mode then
               if Edit_Drag then
                  Edit_Drag_To (E.X, E.Y, Damage);
               end if;
               return;
            end if;
            if Pressed and then Press_Panel /= 0 then
               declare
                  Port : constant Axis.Viewport := Ports (Press_Panel);
                  Moved : constant Integer := E.X - Press_X;
               begin
                  if abs Moved > Click_Slop or else abs (E.Y - Press_Y) > Click_Slop then
                     Dragged := True;
                  end if;
                  if Dragged and then Selecting then
                     Select_To := Time_At (Press_Panel, E.X);
                     Mark_Plots (Damage);
                  elsif Dragged then
                     declare
                        Shift : constant Unsigned_64 := Unsigned_64 (abs Moved) * Port.Width;
                     begin
                        Pan_To ((if Moved > 0 then (if Press_End > Shift then Press_End - Shift else 0)
                                 else Press_End + Shift), Damage);
                     end;
                  end if;
               end;
            end if;
            Set_Hover (E.X, E.Y, Damage);
         when Pointer_Press =>
            --  The toolbar.
            for I in Preset_Index loop
               if Point_In_Rect (E.X, E.Y, Preset_Rect (I)) then
                  Set_Span (Presets (I), 0, Damage);
                  return;
               end if;
            end loop;
            if Point_In_Rect (E.X, E.Y, Edit_Rect) then
               Set_Editing (not Edit_Mode, Damage);
               return;
            end if;
            if Edit_Mode then
               declare
                  P : constant Natural := Card_At (E.X, E.Y);
               begin
                  if P /= 0 then
                     Select_Panel (P, Damage);
                     Edit_Drag := True;
                     Edit_Resize := Point_In_Rect (E.X, E.Y, Grip_Rect (P));
                     Drag_Width := Board.Panels (P).Width;
                     Drag_Height := Board.Panels (P).Height;
                     Press_X := E.X;
                     Press_Y := E.Y;
                     Dash_Edit.Begin_Group;
                  end if;
               end;
               return;
            end if;
            if Point_In_Rect (E.X, E.Y, Live_Rect) then
               if Live_Mode then
                  Pan_To (Current_End, Damage);
               else
                  Go_Live (Damage);
               end if;
               return;
            end if;
            Press_Panel := Panel_At (E.X, E.Y);
            Pressed := Press_Panel /= 0;
            Press_X := E.X;
            Press_Y := E.Y;
            Press_End := Current_End;
            Dragged := False;
            Selecting := E.Shift and then Pressed;
            if Selecting then
               Select_From := Time_At (Press_Panel, E.X);
               Select_To := Select_From;
            end if;
         when Pointer_Release =>
            if Edit_Drag then
               Edit_Drag := False;
               Dash_Edit.End_Group;
               return;
            end if;
            if Pressed and then not Dragged and then Press_Panel /= 0 then
               --  A click: the drill-down slice (the column clicked).
               declare
                  Port : constant Axis.Viewport := Ports (Press_Panel);
                  Col : constant Column_Number := Axis.Column_Of (Time_At (Press_Panel, E.X), Port.Width);
               begin
                  Detail_Valid := True;
                  Detail_Panel := Press_Panel;
                  Detail_From := Axis.Start_Of (Col, Port.Width);
                  Detail_To := Axis.End_Of (Col, Port.Width);
                  Mark_Plots (Damage);
      Mark (Damage, Status);
               end;
            elsif Pressed and then Selecting and then Dragged then
               declare
                  Lo : constant Unsigned_64 := Unsigned_64'Min (Select_From, Select_To);
                  Hi : constant Unsigned_64 := Unsigned_64'Max (Select_From, Select_To);
               begin
                  Selecting := False;
                  if Hi > Lo then
                     Live_Mode := False;
                     End_Us := Hi;
                     Set_Span (Hi - Lo, 0, Damage);
                  end if;
               end;
            end if;
            Pressed := False;
            Dragged := False;
            Selecting := False;
            Mark_Plots (Damage);
         when Wheel =>
            declare
               Anchor : constant Unsigned_64 := (if Hover_Valid then Hover_Time else 0);
               New_Span : Unsigned_64 := Span;
            begin
               for I in 1 .. abs E.Steps loop
                  New_Span := (if E.Steps > 0 then New_Span * Zoom_Denominator / Zoom_Numerator
                               else New_Span * Zoom_Numerator / Zoom_Denominator);
               end loop;
               Set_Span (New_Span, Anchor, Damage);
               Set_Hover (E.X, E.Y, Damage);
            end;
         when Key_Press =>
            if Edit_Mode then
               declare
                  Taken : Boolean;
               begin
                  Edit_Key (E, Damage, Taken);
                  if Taken then
                     return;
                  end if;
               end;
            end if;
            case E.Pressed is
               when Edit => Set_Editing (True, Damage);
               when Undo | Redo => Undo_Redo (E.Pressed = Redo, Damage);
               when Show_Source | Show_Keys =>
                  Toggle_Overlay ((if E.Pressed = Show_Source then Source_Overlay else Keys_Overlay), Damage);
               when Left | Right =>
                  declare
                     Step : constant Unsigned_64 := Span / Pan_Fraction;
                     Now_End : constant Unsigned_64 := Current_End;
                  begin
                     Pan_To ((if E.Pressed = Left then (if Now_End > Step then Now_End - Step else 0)
                              else Now_End + Step), Damage);
                  end;
               when Zoom_In => Set_Span (Span * Zoom_Denominator / Zoom_Numerator, 0, Damage);
               when Zoom_Out => Set_Span (Span * Zoom_Numerator / Zoom_Denominator, 0, Damage);
               when Live | End_Key => Go_Live (Damage);
               when Home => Pan_To (Span, Damage);
               when Escape =>
                  if Source_Shown then
                     Source_Shown := False;
                     Mark (Damage, Whole);
                  end if;
                  Detail_Valid := False;
                  Selecting := False;
                  Mark_Plots (Damage);
      Mark (Damage, Status);
               when Range_1 => Set_Span (Presets (1), 0, Damage);
               when Range_2 => Set_Span (Presets (2), 0, Damage);
               when Range_3 => Set_Span (Presets (3), 0, Damage);
               when Range_4 => Set_Span (Presets (4), 0, Damage);
               when Range_5 => Set_Span (Presets (5), 0, Damage);
               when Range_6 => Set_Span (Presets (6), 0, Damage);
               when Up | Down | Next_Panel | Remove | Duplicate | Copy | No_Key | Menu_Bar | Panel_Source | Add_Panel
                  | New_Dashboard | Open_Dashboard | Save_As | Enter | Space | Backspace | Page_Up | Page_Down
                  | Select_All | Letter_Key => null;
            end case;
         when Text_Input | Modifier_Change => null;
      end case;
   end Handle_Locked;

   ---------------------------------------------------------------------------
   --  Drill-down
   ---------------------------------------------------------------------------
   function Detail_Text return String is
      P : constant Natural := Detail_Panel;
      First, Last : Natural;
   begin
      if not Detail_Valid or else P = 0 then
         return "";
      end if;
      Tracks_Of (P, First, Last);
      if Last < First or else Tracks (First).Columns = null then
         return Drill_Down_Expression (View'(null record));
      end if;
      declare
         T : Dash_Tracks.Track renames Tracks (First).all;
         S : constant Columns.Summary :=
           Columns.Get (T.Columns.all, Axis.Column_Of (Detail_From, T.Width_Us));
         U : constant Format.Unit := T.Info.Unit;
      begin
         return Format.Clock_Text (Detail_From, 0) & "  " & Series_Label (T) & ": "
           & (if S.Origin = Columns.Empty then "no samples"
              else "p50 " & Format.Value_Text (S.P50, U) & ", p99 " & Format.Value_Text (S.P99, U) & ", max "
                & Format.Value_Text (S.Maximum, U) & ", n " & Format.Decimal (S.Count))
           & "   logs: " & Drill_Down_Expression (View'(null record)) & "  (Ctrl+C)";
      end;
   end Detail_Text;

   function Drill_Down_Expression (V : View) return String is
      pragma Unreferenced (V);
      First, Last : Natural;
   begin
      if not Detail_Valid or else Detail_Panel = 0 then
         return "";
      end if;
      Tracks_Of (Detail_Panel, First, Last);
      declare
         Service : constant String :=
           (if Last >= First and then Tracks (First).State = Dash_Tracks.Resolved
            then Dash_Model.Image (Tracks (First).Info.Source) else "desktop");
      begin
         return "(logs.between (LogWindow """ & Service & """ "
           & Format.Decimal (Detail_From / Microseconds_Per_Millisecond) & " "
           & Format.Decimal (Detail_To / Microseconds_Per_Millisecond + 1) & "))";
      end;
   end Drill_Down_Expression;

   ---------------------------------------------------------------------------
   --  Observation
   ---------------------------------------------------------------------------
   function Is_Live (V : View) return Boolean is
      pragma Unreferenced (V);
   begin
      return Live_Mode;
   end Is_Live;

   function Span_Us (V : View) return Unsigned_64 is
      pragma Unreferenced (V);
   begin
      return Span;
   end Span_Us;

   function Resolved_Tracks (V : View) return Natural is
      pragma Unreferenced (V);
      N : Natural := 0;
   begin
      if Tracks /= null then
         for T of Tracks.all loop
            if T.State = Dash_Tracks.Resolved then
               N := N + 1;
            end if;
         end loop;
      end if;
      return N;
   end Resolved_Tracks;

   function Track_Total (V : View) return Natural is
      pragma Unreferenced (V);
   begin
      return (if Tracks = null then 0 else Tracks'Length);
   end Track_Total;

   function Columns_With_Data (V : View) return Natural is
      pragma Unreferenced (V);
      N : Natural := 0;
   begin
      if Tracks /= null then
         for T of Tracks.all loop
            if T.Columns /= null then
               for S of T.Columns.Items loop
                  if S.Origin /= Columns.Empty then
                     N := N + 1;
                  end if;
               end loop;
            end if;
         end loop;
      end if;
      return N;
   end Columns_With_Data;

   procedure Probe
     (V : View; Title : String; Samples, Newest_P99, From_Us, To_Us : out Unsigned_64; Found : out Boolean) is
      pragma Unreferenced (V);
      First, Last : Natural;
   begin
      Samples := 0;
      Newest_P99 := 0;
      From_Us := 0;
      To_Us := 0;
      Found := False;
      if Board = null then
         return;
      end if;
      for P in 1 .. Board.Panels_Used loop
         if Dash_Model.Image (Board.Panels (P).Title) = Title and then Has_Port (P) then
            Tracks_Of (P, First, Last);
            if Last >= First and then Tracks (First).Columns /= null
              and then Tracks (First).State = Dash_Tracks.Resolved
            then
               Found := True;
               declare
                  Port : constant Axis.Viewport := Ports (P);
               begin
                  From_Us := Axis.Left_Time (Port);
                  To_Us := Axis.Right_Time (Port);
                  for C in Axis.First (Port) .. Port.Last loop
                     declare
                        S : constant Columns.Summary := Columns.Get (Tracks (First).Columns.all, C);
                     begin
                        if S.Origin /= Columns.Empty then
                           Samples := Samples + S.Count;
                           Newest_P99 := S.P99;
                        end if;
                     end;
                  end loop;
               end;
            end if;
            return;
         end if;
      end loop;
   end Probe;

   function Title (V : View) return String is
      pragma Unreferenced (V);
   begin
      return (if Board = null then "" else Dash_Model.Image (Board.Title));
   end Title;
   function Pushed_Samples (V : View) return Unsigned_64 is
      pragma Unreferenced (V);
   begin
      return Pushed_Total;
   end Pushed_Samples;

   function Push_Active (V : View) return Boolean is
      pragma Unreferenced (V);
   begin
      return Feed /= null and then Feed.Pushing;
   end Push_Active;

   function Panel_Count (V : View) return Natural is
      pragma Unreferenced (V);
   begin
      return (if Board = null then 0 else Board.Panels_Used);
   end Panel_Count;

   function Panel_Bounds (V : View; P : Positive) return Rect is
      pragma Unreferenced (V);
   begin
      return Panels (P).Outer;
   end Panel_Bounds;

   function Plot_Bounds (V : View; P : Positive) return Rect is
      pragma Unreferenced (V);
   begin
      return Panels (P).Plot;
   end Plot_Bounds;
end Dash_View;

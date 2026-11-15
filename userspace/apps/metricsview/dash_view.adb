pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with CuBit.UI_Metrics;
with Charts_Buckets;
with Charts_Columns;
with Charts_Format;
with Charts_Palette;
with Charts_Time_Axis;
with Charts_Value_Axis;
with Dash_Edit;
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
   Whole, Toolbar, Status, Grid : Rect := (others => 0);
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
   --  Where the overlay is.
   function Source_Box return Rect is
     ((Grid.x + Natural'Min (Source_Inset, Grid.w / 4), Grid.y + Natural'Min (Source_Inset, Grid.h / 4),
       (if Grid.w > 2 * Source_Inset then Grid.w - 2 * Source_Inset else Grid.w),
       (if Grid.h > 2 * Source_Inset then Grid.h - 2 * Source_Inset else Grid.h)));


   function Inside (Outer, R : Rect) return Boolean is
     (R.x >= Outer.x and then R.y >= Outer.y and then R.x + R.w <= Outer.x + Outer.w
      and then R.y + R.h <= Outer.y + Outer.h);

   --  R changed: the retained frame redraws exactly R (Dash_Layers), the
   --  toolkit's frame grows by it.
   procedure Mark_Exactly (Damage : in out Rect; R : Rect);
   procedure Mark_Exactly (Damage : in out Rect; R : Rect) is
   begin
      if not Is_Empty (R) then
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
      if Source_Shown and then Intersects (R, Source_Box) then
         if not Inside (Source_Box, R) then
            Mark_Exactly (Damage, Add_Rect (R, Source_Box));
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

   procedure Lay_Out;
   procedure Lay_Out is
      --  Skyline packing on the 12-column grid: each panel, in order, at the
      --  lowest place its width fits, leftmost on a tie (panels stack in a
      --  column beside a taller one).
      type Skyline is array (0 .. Dash_Model.Grid_Columns - 1) of Natural;
      type Placement is record
         Column, Row : Natural := 0;
      end record;
      type Placements is array (Dash_Model.Panel_Index) of Placement;
      Heights : Skyline := [others => 0];
      Placed : Placements := [others => <>];
      Total_Rows : Natural := 0;
      Unit_W, Unit_H : Natural;
   begin
      Grid_Unit_W := 0;
      Grid_Unit_H := 0;
      Toolbar := (Whole.x, Whole.y, Whole.w, Natural'Min (Toolbar_Height, Whole.h));
      Status := (Whole.x, (if Whole.h > Status_Height then Whole.y + Whole.h - Status_Height else Whole.y),
                 Whole.w, Natural'Min (Status_Height, Whole.h));
      Grid := (Whole.x + Margin, Toolbar.y + Toolbar.h + Margin,
               (if Whole.w > 2 * Margin then Whole.w - 2 * Margin else 0),
               (if Whole.h > Toolbar.h + Status.h + 2 * Margin then Whole.h - Toolbar.h - Status.h - 2 * Margin
                else 0));
      if Board = null then
         return;
      end if;
      for P in 1 .. Board.Panels_Used loop
         declare
            W : constant Dash_Model.Grid_Width := Board.Panels (P).Width;
            Best_Column : Natural := 0;
            Best_Row : Natural := Natural'Last;
         begin
            for Column in 0 .. Dash_Model.Grid_Columns - W loop
               declare
                  Row : Natural := 0;
               begin
                  for K in Column .. Column + W - 1 loop
                     Row := Natural'Max (Row, Heights (K));
                  end loop;
                  if Row < Best_Row then
                     Best_Row := Row;
                     Best_Column := Column;
                  end if;
               end;
            end loop;
            Placed (P) := (Best_Column, Best_Row);
            for K in Best_Column .. Best_Column + W - 1 loop
               Heights (K) := Best_Row + Board.Panels (P).Height;
            end loop;
            Total_Rows := Natural'Max (Total_Rows, Best_Row + Board.Panels (P).Height);
         end;
      end loop;
      --  A grid unit is a cell plus one gap; cards are a unit less the gap.
      Unit_W := (Grid.w + Gap) / Dash_Model.Grid_Columns;
      Unit_H := Natural'Max (Minimum_Row_Unit, (if Total_Rows > 0 then (Grid.h + Gap) / Total_Rows else 0));
      Grid_Unit_W := Unit_W;
      Grid_Unit_H := Unit_H;
      for P in 1 .. Board.Panels_Used loop
         declare
            Item : constant Dash_Model.Panel := Board.Panels (P);
            G : Panel_Geometry renames Panels (P);
            X : constant Natural := Grid.x + Placed (P).Column * Unit_W;
            Y : constant Natural := Grid.y + Placed (P).Row * Unit_H;
            W : constant Natural :=
              (if Placed (P).Column + Item.Width = Dash_Model.Grid_Columns then Grid.x + Grid.w - X
               else Item.Width * Unit_W - Gap);
            H : constant Natural :=
              (if Placed (P).Row + Item.Height = Total_Rows then Natural'Max (Grid.y + Grid.h, Y + 1) - Y
               else Item.Height * Unit_H - Gap);
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
   end Initialize;

   procedure Set_Bounds (V : in out View; Bounds : Rect; Numerator, Denominator : Positive := 1) is
      pragma Unreferenced (V);
   begin
      Whole := Bounds;
      Dash_Layers.Resize (Bounds.x + Bounds.w, Bounds.y + Bounds.h, Numerator, Denominator);
      Lay_Out;
      Update_Quantum;
      --  Plot widths changed: every track follows on the next refresh.
      Has_Port := [others => False];
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

   --  D replaces the dashboard (an edit): panels are laid out again and each
   --  keeps the tracks it had (by panel identity), so the data stays;
   --  removed panels' tracks are retired. UI thread, under the lock.
   procedure Apply_Dashboard (D : Dash_Model.Dashboard; Damage : in out Rect);
   procedure Apply_Dashboard (D : Dash_Model.Dashboard; Damage : in out Rect) is
      Count : Natural := 0;
      Old : Track_Array_Access := Tracks;
   begin
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
               for I in Old'Range loop
                  if Old (I) /= null and then Old (I).Panel_Id = D.Panels (P).Id and then Old (I).Spec_Index = S
                    and then Old (I).Match_Index = Match
                  then
                     Tracks (Count) := Old (I);
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
      Board.all := D;
      Layout_Generation := Layout_Generation + 1;
      if Drawn_States /= null then
         Free (Drawn_States);
         Drawn_States := new State_List'(Tracks'Range => Dash_Tracks.Unresolved);
      end if;
      Lay_Out;
      Update_Quantum;
      Has_Port := [others => False];
      Edit_Count := Edit_Count + 1;
      Last_Edit_Us := Feed.Now_Us;
      Shown_Save := Edited;
      Mark (Damage, Whole);
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
         Selected_Panel := Selected;
         Apply_Dashboard (D, Damage);
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
         Selected_Panel := Natural'Min (Selected_Panel, D.Panels_Used);
         Apply_Dashboard (D, Damage);
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
      Due := Board /= null and then not Save_Pending and then Edit_Count /= Saved_Generation
        and then (not Edit_Mode or else Feed.Now_Us - Last_Edit_Us >= Autosave_Delay_Ms * Microseconds_Per_Millisecond);
      Generation := Edit_Count;
      if Due then
         Save_Pending := True;
         Shown_Save := Saving;
         Save_Changed := True;
      end if;
   end Take_Save;

   procedure End_Save (V : in out View; Generation : Natural; Saved : Boolean; Reason : String := "") is
      pragma Unreferenced (V);
   begin
      Save_Pending := False;
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
      return (if Board = null then "" else Dash_Model.File_Name (Board.all));
   end Save_Name;

   function Saving_State (V : View) return Save_State is
      pragma Unreferenced (V);
   begin
      return Shown_Save;
   end Saving_State;

   function Source_Showing (V : View) return Boolean is
      pragma Unreferenced (V);
   begin
      return Source_Shown;
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

   procedure Data_Step (V : in out View) is
      pragma Unreferenced (V);
      Started : constant Unsigned_64 := Feed.Now_Us;
      Budget_End : constant Unsigned_64 := Started + Fetch_Budget_Us;
   begin
      if Board = null or else Tracks = null then
         return;
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

   procedure Refresh (V : in out View; Damage : in out Rect) is
      pragma Unreferenced (V);
   begin
      if Board = null or else Tracks = null then
         return;
      end if;
      if Drawn_States = null then
         Drawn_States := new State_List'(Tracks'Range => Dash_Tracks.Unresolved);
         --  First frame: fit the span to the data while live.
         Selected_Span := Span;
         Auto_Fit := Live_Mode;
      end if;
      Lock_UI;
      if Save_Changed then
         Save_Changed := False;
         Mark (Damage, Status);
      end if;
      if Auto_Fit then
         Fit_Span (Damage);
      end if;
      for P in 1 .. Board.Panels_Used loop
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
      Right : Natural := H.x + H.w;
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
                                              else 0), Fit_UI_Text (Dash_Model.Image (Item.Title), G.Header.w), K.Text);
         Draw_Legend (C, P, K);
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
           when Saved => ", saved as " & Dash_Model.File_Name (Board.all),
           when Not_Saved => ", not saved: " & Dash_Model.Image (Save_Reason));
      Right_Text : constant String :=
        (if Edit_Mode then "panel " & Format.Decimal (Unsigned_64 (Selected_Panel)) & " of "
           & Format.Decimal (Unsigned_64 (Board.Panels_Used)) & Save_Text
         elsif Shown_Save in Saving | Not_Saved then Save_Text (Save_Text'First + 2 .. Save_Text'Last)
         else Format.Decimal (Unsigned_64 (Resolved_Tracks (View'(null record)))) & " of "
           & Format.Decimal (Unsigned_64 (Track_Total (View'(null record)))) & " series live");
      Left_Text : constant String :=
        (if Edit_Mode then "Editing: Tab selects, arrows move, Shift+arrows size, Delete removes, Insert copies, "
           & "Ctrl+Z undoes, C shows the CCL, E or Esc is done"
         elsif Detail_Valid then Detail_Text
         elsif Live_Mode then "Drag to pan, wheel to zoom, Shift+drag to select, click for the slice's logs"
         else "Paused: L or End returns to live");
      Y : constant Natural := Status.y + (Status.h - UI_Text_Height) / 2;
   begin
      Fill_Rect (C, Status, K.Bar);
      Fill_Rect (C, (Status.x, Status.y, Status.w, 1), K.Rule);
      Text_Right (C, Status.x + Status.w - Margin, Y, Right_Text, K.Muted);
      Text (C, Status.x + Margin, Y,
            Fit_UI_Text (Left_Text, (if Status.w > UI_Text_Width (Right_Text) + 3 * Margin
                                     then Status.w - UI_Text_Width (Right_Text) - 3 * Margin else 0)),
            (if Detail_Valid then K.Text else K.Muted));
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

   procedure Draw_Selection (C : Canvas; K : Colors; P : Dash_Model.Panel_Index);
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

   procedure Paint (C : Canvas; Damage : Rect);
   procedure Paint (C : Canvas; Damage : Rect) is
      K : constant Colors := Current_Colors;
      Content : constant Rect :=
        (Whole.x, Toolbar.y + Toolbar.h, Whole.w,
         (if Whole.h > Toolbar.h + Status.h then Whole.h - Toolbar.h - Status.h else 0));
   begin
      if Board /= null and then Source_Shown and then Inside (Source_Box, Damage) then
         --  All under the opaque overlay.
         Draw_Source (With_Clip (C, Source_Box), K);
         return;
      end if;
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
      if Board /= null and then Source_Shown and then Intersects (Damage, Source_Box) then
         Draw_Source (With_Clip (C, Source_Box), K);
      elsif Hover_Valid and then not Is_Empty (Tooltip) and then Intersects (Damage, Tooltip) then
         Draw_Tooltip (C, K);
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
      if Dash_Layers.Dirty_Count > 0 then
         --  The columns are read under the lock; the copy out needs none.
         Lock_UI;
         for I in 1 .. Dash_Layers.Dirty_Count loop
            declare
               R : constant Rect := Dash_Layers.Dirty (I);
            begin
               Paint (Dash_Layers.Canvas (R), R);
            end;
         end loop;
         Dash_Sync.Unlock;
      end if;
      Dash_Layers.Clear_Dirty;
      Dash_Layers.Copy_Out (C, Damage);
   end Render;

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
        (Hover_Valid and then Hover_Panel = P) or else Selecting or else (Detail_Valid and then Detail_Panel = P);
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
   begin
      if Selected_Panel in 1 .. Board.Panels_Used then
         Mark (Damage, Panels (Selected_Panel).Outer);
      end if;
      Selected_Panel := P;
      if P in 1 .. Board.Panels_Used then
         Mark (Damage, Panels (P).Outer);
      end if;
      Mark (Damage, Status);
      if Source_Shown then
         Mark_Exactly (Damage, Source_Box);
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
      if Selected_Panel in 1 .. Board.Panels_Used then
         Mark (Damage, Panels (Selected_Panel).Outer);
      end if;
      Mark (Damage, Toolbar);
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
               when Show_Source =>
                  Source_Shown := not Source_Shown;
                  if Source_Shown then
                     Hover_Valid := False;
                     Mark_Exactly (Damage, Source_Box);
                  else
                     Mark (Damage, Whole);
                  end if;
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
               when Up | Down | Next_Panel | Remove | Duplicate | Copy | No_Key => null;
            end case;
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

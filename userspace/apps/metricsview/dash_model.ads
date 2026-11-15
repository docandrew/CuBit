------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Dashboards as typed CCL values (MET-002's app): the Dashboard type, the
--  default dashboard (display latency), and the loader that checks a
--  dashboard's CCL source against the type and reads the checked value.
--
--  @description
--  A dashboard is one CCL expression of type Dashboard (TYPE_SOURCE), so an
--  agent or the CCL console builds one with the type checker's help, and
--  the app reads it through CCL.Typed_Values (never a hand-written reader).
--  Panels sit on a 12-column grid (width), height in grid rows. A panel's
--  series are metrics.svc series by exact name or name prefix, optionally
--  of one source. Budgets are in the series' unit (0: none).
--
--  The ceilings below are the CCL value's own (a checked value holds at
--  most CCL.Types.Maximum_Value_Cells cells, so no dashboard can exceed
--  them): they are not choices of this app.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Charts_Buckets;

package Dash_Model is
   TYPE_SOURCE : constant String :=
     "(type Metric_Statistic (enum Minimum P50 P99 Maximum Mean Count Rate Last)) " &
     "(type Series_Match (enum Exact Prefix)) " &
     "(type Panel_Series (record (name String) (source String) (match Series_Match) (label String))) " &
     "(type Span_Ms (range 100 604800000)) " &
     "(type Dashboard_Time (record (span_ms Span_Ms) (live Boolean true))) " &
     "(type Budget (range 0 281474976710656)) " &
     "(type Bucket_Value (range 0 1099511627776)) (type Bucket_Width (range 1 1099511627776)) " &
     "(type Bucket_Count (range 1 64)) " &
     "(type Linear_Buckets (record (first Bucket_Value) (width Bucket_Width) (count Bucket_Count))) " &
     "(type Line_Panel (record (statistics (List Metric_Statistic)) (band Boolean true) (budget Budget 0))) " &
     "(type Histogram_Panel (record (buckets Linear_Buckets) (budget Budget 0))) " &
     "(type Heatmap_Panel (record (statistic Metric_Statistic) (budget Budget 0))) " &
     "(type Stat_Panel (record (statistic Metric_Statistic) (budget Budget 0))) " &
     "(type Logs_Panel (record (service String) (minimum Integer 0))) " &
     "(type Panel_Kind (variant (Time_Series Line_Panel) (Histogram Histogram_Panel) " &
     "(Heatmap Heatmap_Panel) (Stat Stat_Panel) (Logs Logs_Panel))) " &
     "(type Grid_Width (range 1 12)) (type Grid_Height (range 1 24)) " &
     "(type Panel (record (title String) (kind Panel_Kind) (series (List Panel_Series)) " &
     "(width Grid_Width 12) (height Grid_Height 4))) " &
     "(type Dashboard (record (title String) (time Dashboard_Time) (panels (List Panel)))) " &
     --  A series by exact name (of any source), and every series under a prefix.
     "(define (series (name String) (label String)) Panel_Series " &
     "(Panel_Series name """" Series_Match.Exact label)) " &
     "(define (prefix (name String) (label String)) Panel_Series " &
     "(Panel_Series name """" Series_Match.Prefix label))";
   ROOT_NAME : constant String := "Dashboard";

   --  The default: Desktop's display latency (LAT-002) and this app's own.
   DEFAULT_SOURCE : constant String :=
     "(Dashboard title => ""Display latency"" time => (Dashboard_Time span_ms => 120000) panels => [" &
     "(Panel title => ""Input to ready"" width => 9 height => 5 " &
     "kind => (Panel_Kind.Time_Series (Line_Panel statistics => [Metric_Statistic.P50 Metric_Statistic.P99] " &
     "budget => 250)) series => [(series ""desktop.latency.input_to_ready"" ""input"")]) " &
     "(Panel title => ""Input to ready, p99"" width => 3 height => 2 " &
     "kind => (Panel_Kind.Stat (Stat_Panel statistic => Metric_Statistic.P99 budget => 250)) " &
     "series => [(series ""desktop.latency.input_to_ready"" """")]) " &
     "(Panel title => ""Input to ready, p50"" width => 3 height => 2 " &
     "kind => (Panel_Kind.Stat (Stat_Panel statistic => Metric_Statistic.P50 budget => 250)) " &
     "series => [(series ""desktop.latency.input_to_ready"" """")]) " &
     "(Panel title => ""Stages, p99"" width => 9 height => 4 " &
     "kind => (Panel_Kind.Heatmap (Heatmap_Panel statistic => Metric_Statistic.P99 budget => 250)) series => [" &
     "(series ""desktop.latency.driver_to_intake"" ""driver to intake"") " &
     "(series ""desktop.latency.intake_to_delivery"" ""intake to delivery"") " &
     "(series ""desktop.latency.delivery_to_publish"" ""delivery to publish"") " &
     "(series ""desktop.latency.publish_to_compose"" ""publish to compose"") " &
     "(series ""desktop.latency.compose_to_submit"" ""compose to submit"") " &
     "(series ""desktop.latency.submit_to_ready"" ""submit to ready"") " &
     "(series ""desktop.latency.input_to_ready"" ""input to ready"")]) " &
     "(Panel title => ""Inputs"" width => 3 height => 2 " &
     "kind => (Panel_Kind.Stat (Stat_Panel statistic => Metric_Statistic.Count)) " &
     "series => [(series ""desktop.latency.input_to_ready"" """")]) " &
     "(Panel title => ""Frames dropped"" width => 3 height => 2 " &
     "kind => (Panel_Kind.Stat (Stat_Panel statistic => Metric_Statistic.Count)) " &
     "series => [(series ""desktop.frames.dropped"" """")]) " &
     "(Panel title => ""Refreshes late"" width => 3 height => 4 " &
     "kind => (Panel_Kind.Histogram (Histogram_Panel buckets => (Linear_Buckets first => 0 width => 1 count => 5) " &
     "budget => 1)) series => [(series ""desktop.latency.refreshes_late"" """")]) " &
     "(Panel title => ""This app: input to rendered, and its refresh"" width => 9 height => 3 " &
     "kind => (Panel_Kind.Time_Series (Line_Panel statistics => [Metric_Statistic.P99] band => false " &
     "budget => 250)) series => [(series ""metricsview.input_to_render_us"" ""input to rendered"") " &
     "(series ""metricsview.update_us"" ""refresh"")])])";

   type Statistic is (Minimum, P50, P99, Maximum, Mean, Count, Rate, Last);
   type Match_Kind is (Exact, Prefix);
   type Panel_Kind is (Time_Series, Histogram, Heatmap, Stat, Logs);
   type Statistic_Set is array (Statistic) of Boolean;

   --  The store's name and source limit (CuBit.Metric_Query_Pages.Text_Bytes).
   Maximum_Text : constant := 64;
   subtype Text_Length is Natural range 0 .. Maximum_Text;
   type Text is record
      Length : Text_Length := 0;
      Value : String (1 .. Maximum_Text) := [others => ' '];
   end record;
   function To_Text (S : String) return Text;
   function Image (T : Text) return String is (T.Value (1 .. T.Length));

   type Series_Spec is record
      Name, Source, Label : Text;
      Match : Match_Kind := Exact;
   end record;

   --  From the CCL value's cell limit (504): a panel takes at least four
   --  cells and a series three.
   Maximum_Panels : constant := 64;
   Maximum_Panel_Series : constant := 32;
   subtype Panel_Count is Natural range 0 .. Maximum_Panels;
   subtype Panel_Index is Positive range 1 .. Maximum_Panels;
   subtype Series_Count is Natural range 0 .. Maximum_Panel_Series;
   subtype Series_Index is Positive range 1 .. Maximum_Panel_Series;
   type Series_List is array (Series_Index) of Series_Spec;

   Grid_Columns : constant := 12;
   subtype Grid_Width is Positive range 1 .. Grid_Columns;
   Maximum_Grid_Height : constant := 24;
   subtype Grid_Height is Positive range 1 .. Maximum_Grid_Height;

   type Panel is record
      --  The panel's identity while the app runs (not part of the CCL
      --  value): edits move and resize panels, and the data drawn in them
      --  follows by it.
      Id : Natural := 0;
      Title : Text;
      Kind : Panel_Kind := Time_Series;
      Series : Series_List;
      Series_Used : Series_Count := 0;
      Width : Grid_Width := Grid_Columns;
      Height : Grid_Height := 4;
      --  Time_Series: the lines drawn, and the min..max band.
      Lines : Statistic_Set := [others => False];
      Band : Boolean := True;
      --  Heatmap, Stat: the statistic shown.
      Shown : Statistic := P99;
      Budget : Unsigned_64 := 0;
      Buckets : Charts_Buckets.Layout;
      Logs_Service : Text;
      Logs_Minimum : Natural := 0;
   end record;
   type Panel_List is array (Panel_Index) of Panel;

   Minimum_Span_Ms : constant := 100;
   Maximum_Span_Ms : constant := 7 * 24 * 3_600 * 1_000;
   subtype Span_Ms is Unsigned_64 range Minimum_Span_Ms .. Maximum_Span_Ms;

   type Dashboard is record
      Title : Text;
      Span : Span_Ms := 300_000;
      Live : Boolean := True;
      Panels : Panel_List;
      Panels_Used : Panel_Count := 0;
   end record;

   Maximum_Message : constant := 160;
   type Load_Result is record
      Success : Boolean := False;
      --  Why not, and where in the source (from 1).
      Message : String (1 .. Maximum_Message) := [others => ' '];
      Message_Length : Natural range 0 .. Maximum_Message := 0;
      Position : Natural := 0;
   end record;

   --  Source checked against Dashboard and read into Into. On failure Into
   --  is unchanged and Result says why.
   procedure Load (Source : String; Into : in out Dashboard; Result : out Load_Result);

   --  The dashboard as CCL source that Load reads back to the same value
   --  (what is saved, copied and shown): every panel as a Panel expression,
   --  series as (series ...) or a full Panel_Series when it names a source
   --  or a prefix.
   function To_Source (D : Dashboard) return String;
   --  One panel's expression.
   function Panel_Source (P : Panel) return String;

   --  Saved dashboards (PLAN-metricsview.md): the user's documents, one file
   --  a dashboard, named for its title (lower case, words joined by '-'),
   --  in a Dashboards folder among the user's files (as Text's Documents;
   --  made at the first save). The built-ins are compiled into the app and
   --  never written.
   Dashboards_Place : constant String := "@nvme:0/Documents/Dashboards/";
   Dashboard_Suffix : constant String := ".ccl";
   function File_Name (D : Dashboard) return String;
   --  The leaf a dashboard titled Title is saved as (File_Name's last
   --  part), at most Maximum_Leaf long (the file dialog's names).
   Maximum_Leaf : constant := 64;
   function File_Leaf (Title : String) return String;
end Dash_Model;

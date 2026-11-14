with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  The metrics interface (MET-002): metrics.svc's series store as typed CCL
--  values. Each operation takes one record:
--
--    (metrics.series (Metric_Selector name => "desktop.latency.*" source => ""))
--      the series, as List<Metric_Series>: name "" is every series, a
--      trailing * a prefix, else the exact name; source "" any source.
--    (metrics.range (Metric_Range series => "desktop.latency.input_to_ready"
--                    source => "" from => -10000000 to => 0 step => 0))
--      one series' samples in from .. to as List<Metric_Item>: its gaps
--      first (shed by the producer, refused by the store), then the raw
--      points (step 0) or the windows of the coarsest tier no wider than
--      step (Rollup: count, sum, min, max, last, p50, p99), then, when the
--      result is full, a Not_Shown gap saying where to go on from.
--    (metrics.window (Metric_Window series => "desktop.latency.*" source => ""
--                     from => -60000000 to => 0))
--      one Metric_Stats over the window: count, sum, extremes, last, p50,
--      p90, p99, p999 from the merged sketches of every series selected
--      (instances add, never average), the rate per second (in thousandths:
--      a counter's increments, others' samples), what is missing, the
--      resolution it was read at and the relative error bound.
--
--  Times are microseconds of the published monotonic clock (since boot); a
--  from or to of 0 or below is that much before now (to => 0 is now). A
--  reader sees only the series its manifest's metrics scopes name.
--  Results are bounded by one CCL value (CCL.Types.Maximum_Value_Cells): a
--  range says what it did not show, and the caller pages by moving from.
--  When CCL values are unbounded (the cubit-cclvalues work) a range will
--  return the whole selection at once and the Not_Shown gap goes away.
--
--  The types are CCL source; the digest and keys are SHA-256 of it (then
--  "#" and the type's name), checked by tests/ccl-console.
package CCL.Interfaces.Metrics with SPARK_Mode is
   use Standard.Interfaces;

   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 6;
   PUBLISHED_TYPES : constant := 14;

   TYPE_SOURCE : constant String :=
     "(type Metric_Kind (enum Counter Gauge Distribution)) " &
     "(type Metric_Unit (enum Count Microseconds Nanoseconds Bytes Pixels)) " &
     "(type Metric_Selector (record (name String) (source String))) " &
     "(type Metric_Series (record (id Integer) (name String) (source String) (program String) (instance String) " &
     "(kind Metric_Kind) (unit Metric_Unit) (published Integer) (retained Integer) (shed Integer) (evicted Integer) " &
     "(first Integer) (last Integer) (last_value Integer))) " &
     "(type Metric_Range (record (series String) (source String) (from Integer) (to Integer) (step Integer))) " &
     "(type Metric_Gap_Cause (enum Shed_By_Producer Refused_By_Store Not_Shown)) " &
     "(type Metric_Gap (record (first Integer) (last Integer) (count Integer) (cause Metric_Gap_Cause))) " &
     "(type Metric_Point (record (time Integer) (value Integer))) " &
     "(type Metric_Rollup (record (start Integer) (count Integer) (sum Integer) (min Integer) (max Integer) " &
     "(last Integer) (p50 Integer) (p99 Integer))) " &
     "(type Metric_Item (variant (Point Metric_Point) (Rollup Metric_Rollup) (Gap Metric_Gap))) " &
     "(type Metric_Window (record (series String) (source String) (from Integer) (to Integer))) " &
     "(type Metric_Stats (record (series Integer) (count Integer) (sum Integer) (min Integer) (max Integer) " &
     "(last Integer) (p50 Integer) (p90 Integer) (p99 Integer) (p999 Integer) (rate_milli Integer) " &
     "(missing Integer) (resolution Integer) (error_permille Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#90F2_A0AB_E64D_E50A#,
      16#F4BA_8F06_13C9_8343#,
      16#8222_5EE4_F088_74F8#,
      16#6E51_1180_FF83_1356#];
   SERIES_KEY : constant CCL.Objects.Schema_Key :=
     [16#479A_EBFF_B7BE_68FC#,
      16#1391_8449_D468_B1C4#,
      16#3E01_46B9_631D_0D71#,
      16#9A00_0E5C_2F53_933D#];
   ITEMS_KEY : constant CCL.Objects.Schema_Key :=
     [16#B79E_E4FA_B030_1853#,
      16#07D8_A520_7425_B25A#,
      16#40A8_048A_5063_A512#,
      16#1DF1_3EB0_7B0C_C70E#];
   SELECTOR_KEY : constant CCL.Objects.Schema_Key :=
     [16#378D_347D_A42E_DA54#,
      16#91BA_9D81_67B4_575C#,
      16#9261_7A67_4C92_0C37#,
      16#CE20_B50A_DD31_60B8#];
   RANGE_KEY : constant CCL.Objects.Schema_Key :=
     [16#0799_82E5_C926_267B#,
      16#1363_B480_B049_1257#,
      16#F51E_B079_84AA_BE7D#,
      16#0110_A70D_66B9_EFCC#];
   WINDOW_KEY : constant CCL.Objects.Schema_Key :=
     [16#7F0E_458C_5D43_2536#,
      16#65BB_A65F_F0AC_D34E#,
      16#73C1_0288_786F_C15C#,
      16#B21E_8ED3_F6EB_9A10#];
   STATS_KEY : constant CCL.Objects.Schema_Key :=
     [16#8BC6_72D1_EC8C_3FEA#,
      16#F559_5C80_CC23_43AD#,
      16#BE1D_61BB_0649_B7AB#,
      16#6F1C_59B4_C8C5_D098#];

   type Operation is (Series, Range_Of, Window);
   function Name (Op : Operation) return String is
     (case Op is when Series => "series", when Range_Of => "range", when Window => "window");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Metrics);
   function Binding_Of (Op : Operation) return Unsigned_32 is (FIRST_BINDING + Operation'Pos (Op));

   --  Fields of the argument records, in order.
   type Selector_Field is (Selector_Name, Selector_Source);
   type Range_Field is (Range_Series, Range_Source, Range_From, Range_To, Range_Step);
   type Window_Field is (Window_Series, Window_Source, Window_From, Window_To);

   --  Metric_Gap_Cause's members, in order.
   type Gap_Cause is (Shed_By_Producer, Refused_By_Store, Not_Shown);
   --  Metric_Kind's and Metric_Unit's members are CuBit.Metric_Outlet's, in
   --  its order; given here as positions.
   subtype Kind_Position is Natural range 0 .. 2;
   subtype Unit_Position is Natural range 0 .. 4;

   type Contracts is record
      Series_List, Items, Selector, Range_Record, Window_Record, Stats : CCL.Objects.Binding;
   end record;

   procedure Define_Types (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean);
   procedure Publish (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);

   --  Results: Start a list, then add whole items while Added.
   procedure Start (Contract : CCL.Objects.Binding; Image : out CCL.Objects.Image);
   type Series_Row is record
      Id : Unsigned_64 := 0;
      Kind : Kind_Position := 0;
      Measure : Unit_Position := 0;
      Published, Retained, Shed, Evicted, First_Us, Last_Us, Last_Value : Unsigned_64 := 0;
   end record;
   procedure Add_Series
     (Image : in out CCL.Objects.Image; Row : Series_Row; Name, Source, Program, Instance : String;
      Added : out Boolean);
   procedure Add_Point (Image : in out CCL.Objects.Image; Time_Us, Value : Unsigned_64; Added : out Boolean);
   type Rollup_Row is record
      Start_Us, Count, Sum, Minimum, Maximum, Last, P50, P99 : Unsigned_64 := 0;
   end record;
   procedure Add_Rollup (Image : in out CCL.Objects.Image; Row : Rollup_Row; Added : out Boolean);
   procedure Add_Gap
     (Image : in out CCL.Objects.Image; First_Us, Last_Us, Count : Unsigned_64; Cause : Gap_Cause;
      Added : out Boolean);
   --  Whether another item of the largest kind still fits.
   function Has_Room (Image : CCL.Objects.Image) return Boolean;

   Stats_Fields : constant := 14;
   type Stats_Words is array (1 .. Stats_Fields) of Unsigned_64;
   procedure Stats_Value
     (Contract : CCL.Objects.Binding; Values : Stats_Words; Result : out CCL.Objects.Image; Built : out Boolean);
end CCL.Interfaces.Metrics;

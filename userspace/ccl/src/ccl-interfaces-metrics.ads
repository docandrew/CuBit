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
--      first (Producer_Shed, Store_Refused), then the raw
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
     "(type Metric_Gap_Cause (enum Producer_Shed Store_Refused Not_Shown)) " &
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
     [16#4E0E_5D9F_01F4_4AA3#,
      16#F5A2_F786_896C_215B#,
      16#732F_35F8_37F1_18D7#,
      16#58D3_C325_A918_7512#];
   SERIES_KEY : constant CCL.Objects.Schema_Key :=
     [16#C9FB_9D09_2DC0_6C41#,
      16#B4B5_BC8A_5164_D2F7#,
      16#B170_75D2_577C_B1EF#,
      16#D543_4E39_5C19_8E71#];
   ITEMS_KEY : constant CCL.Objects.Schema_Key :=
     [16#80A9_A3C9_D32E_AFFB#,
      16#3813_6F80_D19D_38B3#,
      16#D88E_0A7D_83AC_7013#,
      16#6F4E_0B62_1689_B1B7#];
   SELECTOR_KEY : constant CCL.Objects.Schema_Key :=
     [16#EF01_EBE1_36D3_CC87#,
      16#0F44_AB3D_F4BF_D39D#,
      16#1FAE_C516_C622_8BF9#,
      16#40FB_02FE_8EE8_E93D#];
   RANGE_KEY : constant CCL.Objects.Schema_Key :=
     [16#7384_C941_0AB4_50A9#,
      16#D6E6_4D2A_F0E0_E5C2#,
      16#0D42_CEDB_EB79_F3C1#,
      16#4998_9C39_6157_4234#];
   WINDOW_KEY : constant CCL.Objects.Schema_Key :=
     [16#5B5B_0EE6_34AA_A0E5#,
      16#7811_7A1E_63B7_BCB8#,
      16#D6CE_1C3F_3751_C3B5#,
      16#F0A8_A2B6_C2EC_ABE6#];
   STATS_KEY : constant CCL.Objects.Schema_Key :=
     [16#81B2_9588_3465_9A3B#,
      16#29A8_F350_BC0D_7064#,
      16#2B9F_E56C_44DA_F5AA#,
      16#77DA_9BD3_B933_2DF9#];

   type Operation is (Series, Range_Of, Window);
   function Name (Op : Operation) return String is
     (case Op is when Series => "series", when Range_Of => "range", when Window => "window");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Metrics);
   function Binding_Of (Op : Operation) return Unsigned_32 is (FIRST_BINDING + Operation'Pos (Op));

   --  Fields of the argument records, in order.
   type Selector_Field is (Selector_Name, Selector_Source);
   type Range_Field is (Range_Series, Range_Source, Range_From, Range_To, Range_Step);
   type Window_Field is (Window_Series, Window_Source, Window_From, Window_To);

   --  Metric_Gap_Cause's members, in order (a qualified member name such as
   --  Metric_Gap_Cause.Producer_Shed stays within CCL's 32 characters).
   type Gap_Cause is (Producer_Shed, Store_Refused, Not_Shown);
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

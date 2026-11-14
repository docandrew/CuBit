pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Metric_Outlet;
with Metric_Codecs;
with Metric_Sketches;
with Metric_Strings;
private with Heap_Arrays;

--  metrics.svc's time-series store, in RAM (MET-002, DESIGN-metrics-store.md).
--
--  A series is a metric name and its label set: source (the program's
--  identity, from its launcher), program (its file's short name), instance
--  (its process), node (the machine). Labels are interned strings; the set
--  hashes to a 63-bit series ID, and the full set is kept to tell a hash
--  collision apart (the next free ID is taken). An inverted index maps each
--  label=value to the sorted list of series carrying it, so a selection is
--  an intersection of posting lists.
--
--  Samples go into the series' open head chunk: the first sample's time and
--  value in the chunk header, every later one as the zigzag varints of its
--  time's delta of delta and its value's delta (Metric_Codecs). A full head
--  chunk is sealed into an exactly sized immutable block and a new one
--  opens. Each sample also folds into the open window of every tier (1 s,
--  10 s, 1 min, 10 min: count, sum, min, max, last, and for a Distribution
--  a sparse mergeable sketch, Metric_Sketches), so a graph or a window
--  query over a long range reads windows, not samples.
--
--  Nothing is lost silently. A producer's Loss report and a sample the
--  store cannot keep (a ceiling) become typed gaps of the series, with
--  their time range and count. Sealed chunks and windows that age out past
--  a series' ceilings are counted (Evicted) and the series knows the
--  oldest time it still holds raw.
--
--  Every table grows on the process heap (CuAlloc) by doubling, up to the
--  named ceilings below and Maximum_Store_Bytes in all; a refused growth is
--  counted where it happened, never silent.
package Metric_Series with SPARK_Mode is
   package Outlet renames CuBit.Metric_Outlet;

   Maximum_Series : constant := 65_536;
   --  Raw history per series: sealed chunks of at most Chunk_Bytes.
   Chunk_Bytes : constant := 256;
   Maximum_Chunks_Per_Series : constant := 1_024;
   Maximum_Gaps_Per_Series : constant := 256;
   --  Everything the store allocates for chunks, windows and sketches.
   Maximum_Store_Bytes : constant := 256 * 1024 * 1024;

   subtype Series_Count is Natural range 0 .. Maximum_Series;
   subtype Series_Index is Series_Count range 1 .. Maximum_Series;
   No_Series : constant Series_Count := 0;
   Id_Mask : constant Unsigned_64 := 16#7FFF_FFFF_FFFF_FFFF#;
   subtype Series_Id is Unsigned_64 range 0 .. Id_Mask;

   --  Downsampled tiers: window width and the windows kept per series.
   type Tier is (Second, Ten_Seconds, Minute, Ten_Minutes);
   Microseconds_Per_Second : constant := 1_000_000;
   function Width_Us (Item : Tier) return Unsigned_64 is
     (case Item is
        when Second => Microseconds_Per_Second,
        when Ten_Seconds => 10 * Microseconds_Per_Second,
        when Minute => 60 * Microseconds_Per_Second,
        when Ten_Minutes => 600 * Microseconds_Per_Second);
   --  15 minutes of seconds, 2 hours of 10 s, a day of minutes, a week of 10 min.
   function Windows_Kept (Item : Tier) return Positive is
     (case Item is
        when Second => 900, when Ten_Seconds => 720, when Minute => 1_440, when Ten_Minutes => 1_008);
   --  The coarsest tier whose windows are no wider than Step_Us (Second for
   --  a finer step): what a graph with one column per Step_Us reads.
   function Tier_For_Step (Step_Us : Unsigned_64) return Tier;

   type Labels is record
      Name, Source, Program, Instance, Node : Metric_Strings.String_Id := Metric_Strings.No_String;
   end record;

   type Gap_Reason is (Shed_By_Producer, Refused_By_Store);
   for Gap_Reason use (Shed_By_Producer => 1, Refused_By_Store => 2);
   type Gap is record
      First_Us, Last_Us : Unsigned_64 := 0;
      Count : Unsigned_64 := 0;
      Reason : Gap_Reason := Shed_By_Producer;
   end record;

   type Store is limited private;

   procedure Intern (S : in out Store; Text : String; Id : out Metric_Strings.String_Id);
   function Find_Text (S : Store; Text : String) return Metric_Strings.String_Id;
   function Text_Of (S : Store; Id : Metric_Strings.String_Id) return String;

   --  The series with these labels: an existing one (Same_Kind False when it
   --  was declared with another kind or unit: it is not reused), or a new
   --  one. No_Series when a ceiling or the heap refused.
   procedure Find_Or_Add
     (S : in out Store; With_Labels : Labels; Kind : Outlet.Series_Kind; Measure : Outlet.Unit;
      Index : out Series_Count; Same_Kind : out Boolean)
   with Pre => With_Labels.Name /= Metric_Strings.No_String;

   procedure Append (S : in out Store; Index : Series_Index; Time_Us, Value : Unsigned_64);
   --  The producer shed Count samples between First_Us and Last_Us.
   procedure Note_Shed (S : in out Store; Index : Series_Index; Count, First_Us, Last_Us : Unsigned_64);

   type Series_Info is record
      Id : Series_Id := 0;
      With_Labels : Labels;
      Kind : Outlet.Series_Kind := Outlet.Counter;
      Measure : Outlet.Unit := Outlet.Count;
      --  Samples accepted; still held raw; given up to the raw ceiling;
      --  shed by the producer; refused by the store.
      Published, Retained, Evicted, Shed, Refused : Unsigned_64 := 0;
      First_Us, Last_Us, Last_Value : Unsigned_64 := 0;
      --  The oldest time a raw sample is still held from (0: all kept).
      Raw_From_Us : Unsigned_64 := 0;
      Gaps : Natural := 0;
      --  The encoded bytes of the raw samples held (headers apart).
      Raw_Bytes : Unsigned_64 := 0;
   end record;
   function Count (S : Store) return Series_Count;
   function Info (S : Store; Index : Series_Index) return Series_Info;
   function Find_Id (S : Store; Id : Series_Id) return Series_Count;

   --  Selection: by name (any, exactly, or by prefix) and, optionally, by
   --  source.
   type Name_Match is (Any_Name, Exact_Name, Name_Prefix);
   for Name_Match use (Any_Name => 0, Exact_Name => 1, Name_Prefix => 2);
   Maximum_Selector_Bytes : constant := 64;
   type Selector is record
      Match : Name_Match := Any_Name;
      Name : String (1 .. Maximum_Selector_Bytes) := [others => ' '];
      Name_Length : Natural range 0 .. Maximum_Selector_Bytes := 0;
      Source : String (1 .. Maximum_Selector_Bytes) := [others => ' '];
      Source_Length : Natural range 0 .. Maximum_Selector_Bytes := 0;
   end record;
   function Selects (S : Store; Sel : Selector; Index : Series_Index) return Boolean;
   --  The first selected series after After (No_Series: none). An exact name
   --  walks the intersection of the name's and the source's posting lists.
   procedure Next_Selected (S : Store; Sel : Selector; After : Series_Count; Found : out Series_Count);

   --  Raw samples of Index with From_Us <= time <= To_Us, oldest chunk
   --  first, in each chunk in publication order, until Visit says stop.
   generic
      with procedure Visit (Time_Us, Value : Unsigned_64; Continue : out Boolean);
   procedure Read_Raw (S : Store; Index : Series_Index; From_Us, To_Us : Unsigned_64);

   type Window_Row is record
      Start_Us : Unsigned_64 := 0;
      Count, Sum, Minimum, Maximum, Last : Unsigned_64 := 0;
      --  Distributions only (else 0): from the window's sketch.
      P50, P99 : Unsigned_64 := 0;
   end record;
   --  Index's windows of tier Of_Tier overlapping From_Us .. To_Us, oldest
   --  first, until Visit says stop.
   generic
      with procedure Visit (Row : Window_Row; Continue : out Boolean);
   procedure Read_Windows (S : Store; Index : Series_Index; Of_Tier : Tier; From_Us, To_Us : Unsigned_64);

   --  Index's gaps overlapping From_Us .. To_Us, oldest first.
   generic
      with procedure Visit (Item : Gap; Continue : out Boolean);
   procedure Read_Gaps (S : Store; Index : Series_Index; From_Us, To_Us : Unsigned_64);

   --  Fold Index's samples in From_Us .. To_Us into Into: raw when the raw
   --  history reaches back to From_Us (Resolution_Us 0), else the windows
   --  of the finest tier that does (Resolution_Us its width; its sketches
   --  for a Distribution, its minimum, maximum and counts otherwise).
   --  Missing counts the gaps' samples in the range.
   procedure Summarize
     (S : Store; Index : Series_Index; From_Us, To_Us : Unsigned_64;
      Into : in out Metric_Sketches.Dense; Resolution_Us, Missing : out Unsigned_64);

   function Bytes_Held (S : Store) return Unsigned_64;
   --  Series refused for want of room (Maximum_Series, the heap).
   function Series_Refused (S : Store) return Unsigned_64;
   --  The label key names, as interned (for List rows).
   Name_Key : constant String := "name";
   Source_Key : constant String := "source";
   Program_Key : constant String := "program";
   Instance_Key : constant String := "instance";
   Node_Key : constant String := "node";
private
   pragma SPARK_Mode (Off);
   package Byte_Blocks is new Heap_Arrays (Unsigned_8);
   package Entry_Blocks is new Heap_Arrays (Metric_Sketches.Sparse_Entry);
   package Index_Blocks is new Heap_Arrays (Series_Count);

   subtype Head_Data is Metric_Codecs.Byte_Array (0 .. Chunk_Bytes - 1);
   type Chunk_Header is record
      Count : Natural := 0;
      Used : Natural := 0;
      First_Time, First_Value : Unsigned_64 := 0;
      Last_Time, Last_Value, Last_Delta : Unsigned_64 := 0;
      Min_Time : Unsigned_64 := Unsigned_64'Last;
      Max_Time : Unsigned_64 := 0;
   end record;
   type Sealed_Chunk is record
      Header : Chunk_Header;
      Data : Byte_Blocks.Block := Byte_Blocks.Empty;
   end record;
   package Chunk_Blocks is new Heap_Arrays (Sealed_Chunk);

   type Window is record
      Row : Window_Row;
      Sketch : Entry_Blocks.Block := Entry_Blocks.Empty;
      Sketch_Used : Natural := 0;
   end record;
   package Window_Blocks is new Heap_Arrays (Window);
   type Ring_Position is record
      First : Natural := 0;
      Count : Natural := 0;
   end record;
   type Window_Ring is record
      Items : Window_Blocks.Block := Window_Blocks.Empty;
      At_Ring : Ring_Position;
   end record;
   type Tier_Rings is array (Tier) of Window_Ring;
   package Gap_Blocks is new Heap_Arrays (Gap);

   type Series_Record is record
      Id : Series_Id := 0;
      With_Labels : Labels;
      Kind : Outlet.Series_Kind := Outlet.Counter;
      Measure : Outlet.Unit := Outlet.Count;
      Head : Chunk_Header;
      Head_Bytes : Head_Data := [others => 0];
      Sealed : Chunk_Blocks.Block := Chunk_Blocks.Empty;
      At_Sealed : Ring_Position;
      Windows : Tier_Rings;
      Gaps : Gap_Blocks.Block := Gap_Blocks.Empty;
      At_Gaps : Ring_Position;
      Published, Evicted, Shed, Refused : Unsigned_64 := 0;
      First_Us, Last_Us, Last_Value : Unsigned_64 := 0;
      Raw_From_Us : Unsigned_64 := 0;
   end record;
   package Series_Blocks is new Heap_Arrays (Series_Record);

   type Posting is record
      Key, Value : Metric_Strings.String_Id := Metric_Strings.No_String;
      Items : Index_Blocks.Block := Index_Blocks.Empty;
      Used : Natural := 0;
   end record;
   package Posting_Blocks is new Heap_Arrays (Posting);

   type Store is limited record
      Strings : Metric_Strings.Table;
      Keys_Ready : Boolean := False;
      Keys : Labels;
      Series : Series_Blocks.Block := Series_Blocks.Empty;
      Used : Series_Count := 0;
      --  Series ID -> index (open addressing, half full at most).
      By_Id : Index_Blocks.Block := Index_Blocks.Empty;
      --  label=value -> posting list (open addressing, half full at most).
      Postings : Posting_Blocks.Block := Posting_Blocks.Empty;
      Posting_Count : Natural := 0;
      Held : Unsigned_64 := 0;
      Refusals : Unsigned_64 := 0;
   end record;
end Metric_Series;

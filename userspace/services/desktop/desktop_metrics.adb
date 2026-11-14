with CuBit.Metric_Records;
with CuBit.Metrics;
with Compositor_Release_Metrics;

package body Desktop_Metrics with SPARK_Mode => Off is
   use Interfaces;
   package Records renames CuBit.Metric_Records;
   package Metrics renames CuBit.Metrics;
   package CL renames Compositor_Latency;
   package SM renames Compositor_Stage_Metrics;
   package WM renames Compositor_Work_Metrics;
   package RM renames Compositor_Release_Metrics;

   ---------------------------------------------------------------------------
   --  The compositor families: their declarations name the series.
   ---------------------------------------------------------------------------
   type Family_Series is array (Records.Metric_Key) of Metrics.Series;
   Families : Family_Series := [others => Metrics.No_Series];

   function Series_Of (Declaration : Records.Metric_Record) return Metrics.Series;
   function Series_Of (Declaration : Records.Metric_Record) return Metrics.Series is
      Name : String (1 .. Declaration.Name.Length);
   begin
      if Metrics.Valid (Families (Declaration.Key)) then
         return Families (Declaration.Key);
      end if;
      for I in Name'Range loop
         Name (I) := Character'Val (Declaration.Name.Bytes (I));
      end loop;
      Families (Declaration.Key) := Metrics.Series_Named
        (Name,
         (case Declaration.Declared is
            when Records.Counter => Metrics.Counter,
            when Records.Gauge => Metrics.Gauge,
            when Records.Latency | Records.Span => Metrics.Distribution),
         (case Declaration.Measure is
            when Records.Count => Metrics.Count,
            when Records.Microseconds => Metrics.Microseconds,
            when Records.Nanoseconds => Metrics.Nanoseconds,
            when Records.Bytes => Metrics.Bytes));
      return Families (Declaration.Key);
   end Series_Of;

   procedure Publish_Record (Declaration, Value : Records.Metric_Record);
   procedure Publish_Record (Declaration, Value : Records.Metric_Record) is
      S : constant Metrics.Series := Series_Of (Declaration);
   begin
      case Value.Kind is
         when Records.Counter .. Records.Latency =>
            Metrics.Observe_At (S, Value.Value, Value.Time_Us);
         when Records.Span =>
            Metrics.Observe_At (S, Value.End_Us - Value.Start_Us, Value.End_Us);
         when Records.Describe | Records.Trace =>
            null;
      end case;
   end Publish_Record;

   procedure Record_Work (Kind : WM.Work_Kind; Pixels, Now : Unsigned_64) is
      Value : constant WM.Sample := WM.Prepare (Kind, Pixels, Now);
   begin
      if Value.Valid then
         Publish_Record (WM.Declaration (Kind), Value.Value);
      end if;
   end Record_Work;

   procedure Record_Stage (Stage : SM.Stage; First, Last : Unsigned_64) is
      Value : constant SM.Sample := SM.Prepare (Stage, First, Last);
   begin
      if Value.Valid then
         Publish_Record (SM.Declaration (Stage), Value.Value);
      end if;
   end Record_Stage;

   procedure Record_Completion (Frame : Compositor_Frame_Trace.Record_Value) is
      Value : constant RM.Sample := RM.Prepare (Frame);
   begin
      if Value.Valid then
         Publish_Record (RM.Declaration (Frame.Output_ID), Value.Value);
      end if;
   end Record_Completion;

   ---------------------------------------------------------------------------
   --  LAT-002
   ---------------------------------------------------------------------------
   Latency_Prefix : constant String := "desktop.latency.";
   Frames_Prefix : constant String := "desktop.frames.";
   function Path_Name (Path : CL.Render_Path) return String is
     (case Path is when CL.CPU => "cpu", when CL.GPU => "gpu");

   type Stage_Series is array (CL.Stage) of Metrics.Series;
   type Path_Series is array (CL.Path_Kind, CL.Render_Path) of Metrics.Series;
   type Count_Series is array (CL.Count_Kind) of Metrics.Series;
   Stages : Stage_Series := [others => Metrics.No_Series];
   Paths : Path_Series := [others => [others => Metrics.No_Series]];
   Counts : Count_Series := [others => Metrics.No_Series];
   Late, Vblanks : Metrics.Series := Metrics.No_Series;
   Declared : Boolean := False;

   function Count_Name (Kind : CL.Count_Kind) return String is
     (case Kind is
        when CL.Input_Taken => Latency_Prefix & "inputs",
        when CL.Input_Shown => Latency_Prefix & "shown",
        when CL.Input_Unshown => Latency_Prefix & "unshown",
        when CL.Input_Evicted => Latency_Prefix & "evicted",
        when CL.Input_Untimed => Latency_Prefix & "untimed",
        when CL.Frame_Dropped => Frames_Prefix & "dropped");
   function Path_Series_Name (Kind : CL.Path_Kind; Path : CL.Render_Path) return String is
     (Frames_Prefix & Path_Name (Path) & "." &
      (case Kind is
         when CL.Damage_Pixels => "damage_px",
         when CL.Damage_Rectangles => "damage_rects",
         when CL.Render_Cost => "render_us",
         when CL.Path_Submit_To_Ready => "submit_to_ready_us"));
   function Path_Unit (Kind : CL.Path_Kind) return Metrics.Unit is
     (case Kind is
        when CL.Damage_Pixels => Metrics.Pixels,
        when CL.Damage_Rectangles => Metrics.Count,
        when CL.Render_Cost | CL.Path_Submit_To_Ready => Metrics.Microseconds);

   procedure Declare_Latency;
   procedure Declare_Latency is
   begin
      for St in CL.Stage loop
         Stages (St) := Metrics.Series_Named
           (Latency_Prefix & CL.Stage_Name (St), Metrics.Distribution, Metrics.Microseconds);
      end loop;
      for K in CL.Path_Kind loop
         for P in CL.Render_Path loop
            Paths (K, P) := Metrics.Series_Named (Path_Series_Name (K, P), Metrics.Distribution, Path_Unit (K));
         end loop;
      end loop;
      for K in CL.Count_Kind loop
         Counts (K) := Metrics.Series_Named (Count_Name (K), Metrics.Counter, Metrics.Count);
      end loop;
      Late := Metrics.Series_Named (Latency_Prefix & "refreshes_late", Metrics.Distribution, Metrics.Count);
      Vblanks := Metrics.Series_Named (Frames_Prefix & "vblanks_per_frame", Metrics.Distribution, Metrics.Count);
      Declared := True;
   end Declare_Latency;

   function Series_For (Kind : CL.Observation_Kind; Of_Stage : CL.Stage; Path : CL.Render_Path)
     return Metrics.Series is
     (case Kind is
        when CL.Stage_Sample => Stages (Of_Stage),
        when CL.Late_Sample => Late,
        when CL.Path_Kind => Paths (Kind, Path),
        when CL.Frame_Vblanks => Vblanks,
        when CL.Count_Kind => Counts (Kind));

   procedure Each (Item : CL.Observation);
   procedure Each (Item : CL.Observation) is
   begin
      Metrics.Observe_At (Series_For (Item.Kind, Item.Of_Stage, Item.Path), Item.Value, Item.Time);
   end Each;

   procedure Lost (Kind : CL.Observation_Kind; Of_Stage : CL.Stage; Path : CL.Render_Path;
                   Count, First_Us, Last_Us : Unsigned_64);
   procedure Lost (Kind : CL.Observation_Kind; Of_Stage : CL.Stage; Path : CL.Render_Path;
                   Count, First_Us, Last_Us : Unsigned_64) is
   begin
      Metrics.Note_Lost (Series_For (Kind, Of_Stage, Path), Count, First_Us, Last_Us);
   end Lost;

   procedure Drain is new CL.Drain_Observations (Each, Lost);

   procedure Publish_Latency (L : in out CL.Ledger) is
   begin
      if not Declared then
         Declare_Latency;
      end if;
      Drain (L);
      Metrics.Report_Losses;
   end Publish_Latency;
end Desktop_Metrics;

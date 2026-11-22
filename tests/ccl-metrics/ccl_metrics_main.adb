--  Linux-hosted tests of metrics.* in CCL (MET-002): the typed operations
--  over the whole host environment the console uses, against a scripted
--  CCL_Metric_IO (fake_ccl_metric_io.adb). Checks the selector's name
--  forms, relative times, the typed results, and that a range a value
--  cannot hold says what it did not show. Not CuBit: the headless
--  metrics-latency test runs the real path.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Sessions;
with CCL.Streams;
with CCL_Host_Environment;
with Fake_Calls;

procedure CCL_Metrics_Main is
   Failures, Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String);
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;
   function Contains (Text, Pattern : String) return Boolean is
     (Ada.Strings.Fixed.Index (Text, Pattern) > 0);
   function Occurrences (Text, Pattern : String) return Natural is
     (Ada.Strings.Fixed.Count (Text, Pattern));

   Clock : Unsigned_64 := 1_000;
   function Now (Available : out Boolean) return Unsigned_64;
   function Now (Available : out Boolean) return Unsigned_64 is
   begin
      Available := True;
      Clock := Clock + 10;
      return Clock;
   end Now;
   package Host is new CCL_Host_Environment (Now);

   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   type No_Context is null record;
   Context : No_Context;
   procedure Invoke
     (C : in out No_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
   procedure Invoke
     (C : in out No_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (C);
   begin
      Host.Invoke (Binding, Argument, Reply);
   end Invoke;
   procedure Read
     (C : in out No_Context; Request : CCL.Streams.View_Request; Reply : in out CCL.Streams.View_Reply);
   procedure Read
     (C : in out No_Context; Request : CCL.Streams.View_Request; Reply : in out CCL.Streams.View_Reply)
   is
      pragma Unreferenced (C);
   begin
      Host.Read_Stream (Request, Reply);
   end Read;
   procedure Submit is new CCL.Sessions.Submit_With_Values (No_Context, Invoke, Read_Stream => Read);
   Session : CCL.Sessions.Session;

   function Run (Source : String) return String;
   function Run (Source : String) return String is
      Outcome : CCL.Language.Interpretation_Result;
   begin
      Submit (Session, Source, CCL.Sessions.Default_Fuel, Grants, Context, Outcome);
      return CCL.Sessions.Result_Image (Outcome);
   end Run;

   Installed : Boolean;
   Exact : constant := 1;
   Prefix : constant := 2;
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Catalog.Initialize (Grants);
   Host.Install (Catalog, Grants, Installed);
   Check (Installed, "the host environment installs metrics.*");
   CCL.Sessions.Initialize (Session, Catalog);

   declare
      Listed : constant String := Run ("(metrics.series (Metric_Selector name => ""desktop.latency.*"" source => """"))");
   begin
      Check (Contains (Listed, "List<Metric_Series>") and then
             Contains (Listed, """desktop.latency.input_to_ready"" ""com.cubit.desktop"" ""desktop"" ""12""") and then
             Contains (Listed, "Metric_Kind.Distribution Metric_Unit.Microseconds 96 96"),
             "metrics.series: typed rows: " & Listed);
      Check (Fake_Calls.Last_Match = Prefix and then
             Fake_Calls.Last_Name (1 .. Fake_Calls.Last_Name_Length) = "desktop.latency.",
             "a trailing * is a prefix, sent without it");
   end;
   declare
      Ranged : constant String :=
        "(metrics.range (Metric_Range series => ""desktop.latency.input_to_ready"" source => """" " &
        "from => -1000 to => 0 step => 0))";
      Defined : constant String := Run ("(length " & Ranged & ")");
      Head : constant String := Run ("(first 2 " & Ranged & ")");
      Tail : constant String := Run ("(last 1 " & Ranged & ")");
      Length : constant String := Defined;
   begin
      Check (Fake_Calls.Last_Match = Exact and then Fake_Calls.Last_Id = 77,
             "a range reads the series the exact name picks: " & Defined);
      Check (Fake_Calls.Last_From = Fake_Calls.Now - 1_000 and then Fake_Calls.Last_To = Fake_Calls.Now,
             "from and to of 0 or less are before now");
      Check (Contains (Head, "List<Metric_Item>") and then
             Contains (Head, "(Metric_Item.Gap (Metric_Gap 100 200 4 Metric_Gap_Cause.Producer_Shed))") and then
             Contains (Head, "(Metric_Item.Point (Metric_Point 1000 0))"),
             "the producer's gap first, then the raw points: " & Head);
      Check (Contains (Tail, "Metric_Gap_Cause.Not_Shown"),
             "a range one value cannot hold ends with a Not_Shown gap: " & Tail);
      Check (Contains (Length, "Integer: ") and then not Contains (Length, "Integer: 302"),
             "it holds less than the 300 points and two gaps: " & Length);
   end;
   declare
      Stats : constant String :=
        Run ("(metrics.window (Metric_Window series => ""desktop.latency.*"" source => """" from => 1 to => 5))");
   begin
      Check (Contains (Stats, "(Metric_Stats 2 96 0 0 0 0 0 0 880 0 0 0 0 32)"),
             "metrics.window: merged stats: " & Stats);
      Check (Fake_Calls.Last_From = 1 and then Fake_Calls.Last_To = 5, "positive times are since boot");
   end;
   Check (Contains (Run ("(metrics.range (Metric_Range series => ""a"" source => """" from => 9 to => 1 step => 0))"),
                    "ends before it starts"), "a range that ends before it starts is refused");
   Check (not Contains (Run ("(metrics.series (Metric_Selector name => """ & [1 .. 65 => 'x'] & """ source => """"))"),
                        "List<Metric_Series>"), "a name past 64 characters is refused");

   Put_Line ((if Failures = 0 then "PASS: " else "FAIL: ") & "metrics.* in CCL" & Natural'Image (Checks) &
             " checks" & (if Failures = 0 then "" else "," & Natural'Image (Failures) & " failed"));
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end CCL_Metrics_Main;

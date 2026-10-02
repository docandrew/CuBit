with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Observatory_CCL;
with CuBit.Metric_Records;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Sessions;
with CCL.Language;
procedure CCL_Tests is
   package O renames Observatory_CCL;
   package P renames O.P;
   package R renames CuBit.Metric_Records;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Language.Interpretation_Status;
   type Host is record
      View : O.Context;
      Calls : Natural := 0;
   end record;
   procedure Invoke (State : in out Host; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result) is
   begin
      State.Calls := State.Calls + 1;
      O.Invoke (State.View, Binding, Argument, Reply);
   end Invoke;
   procedure Submit is new CCL.Sessions.Submit_With_Values (Host, Invoke);
   State : Host;
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants, Missing : CCL.Catalog.Granted_Bindings;
   Session : CCL.Sessions.Session;
   Outcome : CCL.Language.Interpretation_Result;
   Error : CCL.Catalog.Catalog_Error;
   Resolved : CCL.Catalog.Resolved_Operation;
   Grant : CCL.Catalog.Grant_Result;
   Found, Accepted : Boolean;
   Rows : P.Summary_Page := [others => [others => 0]];
   Name : constant R.Slot_Words := R.Encode
     ((Kind => R.Describe, Key => 1, Declared => R.Latency,
       Measure => R.Microseconds, Name => R.To_Name ("desktop.release")));
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "CCL check" & Checks'Image; end if;
   end Check;
   procedure Run (Source, Expected : String) is
   begin
      Submit (Session, Source, 4096, Grants, State, Outcome);
      if Outcome.Status /= CCL.Language.Succeeded then
         Put_Line (CCL.Sessions.Result_Image (Outcome));
      end if;
      Check (Outcome.Status = CCL.Language.Succeeded);
      Check (CCL.Sessions.Result_Image (Outcome) = Expected);
   end Run;
begin
   CCL.Catalog.Initialize (Catalog); CCL.Catalog.Initialize (Grants); CCL.Catalog.Initialize (Missing);
   O.Publish (Catalog, Error); Check (Error = CCL.Catalog.Catalog_Valid);
   CCL.Sessions.Initialize (Session, Catalog);
   Submit (Session, "(metrics.count)", 4096, Missing, State, Outcome);
   Check (Outcome.Status = CCL.Language.Host_Authority_Denied and State.Calls = 0);
   for Op in O.Operation loop
      CCL.Catalog.Resolve (Catalog, "metrics." & O.Name (Op), Resolved, Found); Check (Found);
      CCL.Catalog.Install (Grants, Resolved, O.Binding (Op), Grant);
      Check (Grant = CCL.Catalog.Grant_Added);
   end loop;
   Run ("(metrics.ready)", "Boolean: false");
   Run ("(metrics.count)", "Integer: 0");
   Rows (0) (P.Row_Source) := Unsigned_64'Last;
   Rows (0) (P.Row_Publisher_Tag) := P.Publisher_Tag (1);
   Rows (0) (P.Row_Key) := 1;
   Rows (0) (P.Row_Kind) := R.Record_Kind'Enum_Rep (R.Latency);
   Rows (0) (P.Row_Unit) := R.Unit'Enum_Rep (R.Microseconds);
   Rows (0) (P.Row_Count_Word) := 1;
   Rows (0) (P.Row_Minimum) := 9; Rows (0) (P.Row_Maximum) := 9;
   for I in P.Row_P50 .. P.Row_P999 loop Rows (0) (I) := 10; end loop;
   Rows (0) (P.Row_Total) := Unsigned_64'Last;
   for I in 0 .. 3 loop Rows (0) (P.Row_First_Name + I) := Name (4 + I); end loop;
   O.Replace (State.View, Rows, 1, Accepted); Check (Accepted);
   Run ("(metrics.ready)", "Boolean: true");
   Run ("(metrics.count)", "Integer: 1");
   Run ("(metrics.source 0)", "String: 18446744073709551615");
   Run ("(metrics.total 0)", "String: 18446744073709551615");
   Run ("(metrics.lossy 0)", "Boolean: false");
   Run ("(metrics.saturated 0)", "Boolean: false");
   Run ("(concat (metrics.name 0) (concat "": p99 <= "" (concat (metrics.p99-upper 0) (concat "" "" (metrics.unit 0)))))",
        "String: desktop.release: p99 <= 10 us");
   Submit (Session, "(metrics.name -1)", 4096, Grants, State, Outcome);
   Check (Outcome.Status /= CCL.Language.Succeeded);
   Submit (Session, "(metrics.name 1)", 4096, Grants, State, Outcome);
   Check (Outcome.Status /= CCL.Language.Succeeded);
   declare Before : constant Natural := State.Calls; begin
      Submit (Session, "(metrics.name true)", 4096, Grants, State, Outcome);
      Check (Outcome.Status = CCL.Language.Type_Check_Failed and State.Calls = Before);
   end;
   Rows (0) (P.Row_Source_Producer_Dropped) := 7;
   Rows (0) (P.Row_Flags) := P.Flag_Histogram_Saturated;
   O.Replace (State.View, Rows, 1, Accepted); Check (Accepted);
   Run ("(metrics.lossy 0)", "Boolean: true");
   Run ("(metrics.saturated 0)", "Boolean: true");
   Run ("(metrics.dropped 0)", "String: 7");
   Rows (0) (P.Row_Count_Word) := 0;
   for I in P.Row_Minimum .. P.Row_P999 loop Rows (0) (I) := 0; end loop;
   O.Replace (State.View, Rows, 1, Accepted); Check (Accepted);
   Run ("(metrics.p99-upper 0)", "String: unavailable");
   Rows (0) (P.Row_Flags) := 4;
   O.Replace (State.View, Rows, 1, Accepted); Check (not Accepted);
   Run ("(metrics.ready)", "Boolean: false");
   Run ("(metrics.count)", "Integer: 0");
   Submit (Session, "(metrics.name 0)", 4096, Grants, State, Outcome);
   Check (Outcome.Status /= CCL.Language.Succeeded);
   Put_Line ("PASS CCL metrics binding:" & Checks'Image & " checks");
end CCL_Tests;

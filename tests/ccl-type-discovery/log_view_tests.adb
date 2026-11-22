with CCL.Literal_Tables;
with CCL.Held_Texts;
with CCL.Evaluation;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Objects; use CCL.Objects;
with CCL.Objects.Holders;
with CCL.Catalog; use CCL.Catalog;
with CCL.Host_Values;
with CCL.Language; use CCL.Language;
   use CCL.Evaluation;
with CCL.Interfaces.Logs;
with CCL.Presentations;

--  The REPL log viewer's path, hosted: (logs.recent "service") through the
--  interpreter, a host answering with a typed List<LogItem> image (records,
--  a gap and a tally, LOG-005), and the list used as ordinary CCL data
--  (length, at, match, field, where, literal).
procedure Log_View_Tests is
   package Logs renames CCL.Interfaces.Logs;
   use type CCL.Host_Values.Value_Kind;
   RECENT_BINDING : constant Unsigned_32 := 5;
   Catalog : Interface_Catalog;
   Grants : Granted_Bindings;
   Contract : Binding;
   Outcome : Interpretation_Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "log view: " & Why; end if;
   end Check;
   type Host is record
      Calls : Natural := 0;
      Asked : String (1 .. Logs.MAX_SERVICE_NAME) := [others => ' '];
      Asked_Length : Natural := 0;
   end record;
   Context : Host;
   --  A logstore stand-in for "netstack": three records, 40 not retained
   --  between the first two, and its tally; none otherwise.
   procedure Invoke
     (State : in out Host; Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Image : CCL.Objects.Holders.Holder;
      Added : Boolean;
   begin
      State.Calls := State.Calls + 1;
      Check (Binding = RECENT_BINDING and then Argument.Kind = CCL.Host_Values.Text_Value, "recent's argument");
      State.Asked_Length := Argument.Content.Length;
      State.Asked (1 .. State.Asked_Length) := Argument.Content.Data (1 .. State.Asked_Length);
      Logs.Start (Contract, Image);
      if State.Asked (1 .. State.Asked_Length) = "netstack" then
         Logs.Add (Image, 10, Logs.Information, 7, "boot", Added); Check (Added, "add boot");
         Logs.Add_Gap (Image, 20, 7, 40, Logs.Not_Retained, Added); Check (Added, "add gap");
         Logs.Add (Image, 20, Logs.Error, 7, "link down", Added); Check (Added, "add error");
         Logs.Add (Image, 30, Logs.Information, 7, "link up", Added); Check (Added, "add up");
         Logs.Add_Tally (Image, 7, 43, 3, 40, 2, 1, Added); Check (Added, "add tally");
      end if;
      Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
   end Invoke;
   procedure Run is new Evaluate_With_Values (Host, Invoke);
   procedure Evaluate (Source : String) is
   begin
      Run (Source, 4096, Catalog, Grants, Context, Outcome);
      if Outcome.Status /= Succeeded then
         Ada.Text_IO.Put_Line (Source & " => " & Outcome.Status'Image & " " & Outcome.Diagnostic'Image);
      end if;
   end Evaluate;
   Error : Catalog_Error;
   Resolved : Resolved_Operation;
   Found : Boolean;
   Granted : Grant_Result;
   Full : CCL.Objects.Holders.Holder;
   Added : Boolean := True;
   Count : Natural := 0;
begin
   Initialize (Catalog);
   Initialize (Grants);
   Logs.Publish (Catalog, Error); Check (Error = Catalog_Valid, "publish logs");
   Resolve_Schema (Catalog, Logs.SCHEMA_KEY, Contract);
   Check (Is_Bound (Contract), "schema resolves");
   Resolve (Catalog, "logs.recent", Resolved, Found); Check (Found, "resolve recent");
   Install (Grants, Resolved, RECENT_BINDING, Granted); Check (Granted = Grant_Added, "install recent");

   Evaluate ("(logs.recent ""netstack"")");
   Check (Outcome.Status = Succeeded and then Outcome.Has_Literal and then
          CCL.Held_Texts.Image (Outcome.Literal) =
            "[(LogItem.Logged (LogEntry 10 Severity.Information 7 ""boot"")) " &
            "(LogItem.Gap (LogGap 20 7 40 GapReason.Not_Retained ""... 40 records not retained ..."")) " &
            "(LogItem.Logged (LogEntry 20 Severity.Error 7 ""link down"")) " &
            "(LogItem.Logged (LogEntry 30 Severity.Information 7 ""link up"")) " &
            "(LogItem.Tally (LogTally 7 43 3 40 2 1 ""43 published, 3 retained, 40 not retained; " &
            "shed 2 over budget, 1 by the publisher""))]", "the list's literal: " &
          CCL.Held_Texts.Image (Outcome.Literal));
   Check (Context.Asked (1 .. Context.Asked_Length) = "netstack", "the service asked for");
   --  As a table (the console, the Workbench, the Observatory): LogEntry's
   --  columns; the gap's and the tally's rows fill time, source and
   --  message, the columns their records share.
   declare
      Shown : CCL.Presentations.Presentation;
      use type CCL.Presentations.Form;
      function Cell (Row, Column : Positive) return String is
        (CCL.Presentations.Cell_Text (Outcome, Shown, Row, Column));
   begin
      CCL.Presentations.Describe (Outcome, Shown);
      Check (Shown.Kind = CCL.Presentations.Table and then CCL.Literal_Tables.Rows (Shown.Cells) = 5 and then Shown.Shape.Count = 4,
             "logs.recent is a table of five rows");
      Check (Cell (1, 4) = """boot""" and then Cell (1, 1) = "10", "a record's row: " & Cell (1, 4));
      Check (Cell (2, 1) = "20" and then Cell (2, 2) = "" and then Cell (2, 3) = "7" and then
             Cell (2, 4) = """... 40 records not retained ...""", "the gap's row: " & Cell (2, 4));
      Check (Cell (5, 1) = "" and then Cell (5, 3) = "7" and then
             Cell (5, 4) = """43 published, 3 retained, 40 not retained; shed 2 over budget, 1 by the publisher""",
             "the tally's row: " & Cell (5, 4));
   end;
   Evaluate ("(length (logs.recent ""netstack""))");
   Check (Outcome.Status = Succeeded and then Outcome.Result_Value.Integer = 5, "length");
   Evaluate ("(match (at (logs.recent ""netstack"") 3) ((LogItem.Logged e) (field e message)) " &
             "((LogItem.Gap g) ""gap"") ((LogItem.Tally t) ""tally""))");
   Check (Outcome.Status = Succeeded and then Outcome.Has_Text and then
          Outcome.Result_Text.Data (1 .. Outcome.Result_Text.Length) = "link down", "a field of an entry");
   Evaluate ("(length (where (fn ((i LogItem)) (match i ((LogItem.Logged e) (= (field e severity) Severity.Error)) " &
             "((LogItem.Gap g) false) ((LogItem.Tally t) false))) (logs.recent ""netstack"")))");
   Check (Outcome.Status = Succeeded and then Outcome.Result_Value.Integer = 1, "errors only");
   --  The gap is a typed value: how many, and why.
   Evaluate ("(match (at (logs.recent ""netstack"") 2) ((LogItem.Gap g) " &
             "(if (= (field g reason) GapReason.Not_Retained) (field g count) 0)) " &
             "((LogItem.Logged e) -1) ((LogItem.Tally t) -1))");
   Check (Outcome.Status = Succeeded and then Outcome.Result_Value.Integer = 40, "the gap's count");
   Evaluate ("(match (at (logs.recent ""netstack"") 5) ((LogItem.Tally t) " &
             "(+ (field t retained) (field t not_retained))) ((LogItem.Logged e) -1) ((LogItem.Gap g) -1))");
   Check (Outcome.Status = Succeeded and then Outcome.Result_Value.Integer = 43, "retained + not retained");
   Evaluate ("(length (logs.recent ""nobody""))");
   Check (Outcome.Status = Succeeded and then Outcome.Result_Value.Integer = 0, "an unknown service has none");

   --  The builder adds only whole entries, as many as there are: past a
   --  page's room, and up to a value's ceiling, where it refuses.
   Logs.Start (Contract, Full);
   while Added loop
      Logs.Add (Full, 1, Logs.Debug, 2, "x", Added);
      if Added then Count := Count + 1; end if;
   end loop;
   Check (Count = (CCL.Objects.VALUE_CELL_CEILING - 1) / Logs.ENTRY_CELLS,
          "a result holds entries up to the value ceiling:" & Count'Image);
   Check (CCL.Objects.Holders.Validate (Full, Contract), "a full result is a valid value");
   --  And whole gaps only.
   Logs.Start (Contract, Full);
   Count := 0;
   Added := True;
   while Added and then Count < 10_000 loop
      Logs.Add_Gap (Full, 1, 1, 2, Logs.Not_Shown, Added);
      if Added then Count := Count + 1; end if;
   end loop;
   Check (Count = 10_000, "a result of gaps holds what it is given");
   Check (CCL.Objects.Holders.Validate (Full, Contract), "a result of gaps is a valid value");
   Check (CCL.Objects.Holders.Cells_Of (Full) = 1 + 10_000 * Logs.GAP_CELLS, "and is as large as its gaps");
   Ada.Text_IO.Put_Line ("Log view: PASS" & Checks'Image & " checks");
end Log_View_Tests;

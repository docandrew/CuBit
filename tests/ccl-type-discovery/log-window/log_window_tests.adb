with CCL.Literal_Tables;
with CCL.Presentations;
with CCL.Held_Texts;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog; use CCL.Catalog;
with CCL.Evaluation; use CCL.Evaluation;
with CCL.Host_Values;
with CCL.Interfaces.Logs;
with CCL.Language; use CCL.Language;
with CCL_Log_Bindings;
with CuBit.Failures;
with Log_Window_Script;

--  logs.between and logs.recent, Linux-hosted: CCL source analysed,
--  compiled, verified and run on the VM, the real logs bindings
--  (CCL_Log_Bindings: the LogWindow argument and its refusals) and the real
--  selection (CCL_Log_Selection), over a scripted stand-in for logstore's
--  replay (log-window/ccl_log_io.adb), not CuBit's logstore.
procedure Log_Window_Tests is
   package Logs renames CCL.Interfaces.Logs;
   use type CuBit.Failures.Reason;
   Catalog : Interface_Catalog;
   Grants : Granted_Bindings;
   Outcome : Interpretation_Result;
   Checks : Natural := 0;
   Installed : Boolean;
   --  Enough for a where over a whole result.
   FUEL : constant := 100_000;
   --  The largest time a window can name: CCL's Integer'Last.
   LATEST : constant String := "9223372036854775807";

   procedure Check (Good : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "log window: " & Why; end if;
   end Check;

   type Host is null record;
   Context : Host;
   procedure Invoke
     (State : in out Host; Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (State);
   begin
      CCL_Log_Bindings.Invoke (Binding, Argument, Reply);
   end Invoke;
   procedure Run is new Evaluate_With_Values (Host, Invoke);

   procedure Evaluate (Source : String) is
   begin
      Run (Source, FUEL, Catalog, Grants, Context, Outcome);
   end Evaluate;
   function Literal return String is (CCL.Held_Texts.Image (Outcome.Literal));
   function Integer_Of (Source : String) return Integer_64 is
   begin
      Evaluate (Source);
      Check (Outcome.Status = Succeeded and then Outcome.Has_Value, Source & " => " & Outcome.Status'Image);
      return Outcome.Result_Value.Integer;
   end Integer_Of;
   function Literal_Of (Source : String) return String is
   begin
      Evaluate (Source);
      Check (Outcome.Status = Succeeded and then Outcome.Has_Literal,
             Source & " => " & Outcome.Status'Image & " " & Outcome.Diagnostic'Image);
      return Literal;
   end Literal_Of;

   --  The literal of each LogItem the stand-in's scripts give.
   function Image (Value : Unsigned_64) return String is
      Raw : constant String := Unsigned_64'Image (Value);
   begin
      return Raw (Raw'First + 1 .. Raw'Last);
   end Image;
   function Logged (Time, Source : Unsigned_64; Text : String) return String is
     ("(LogItem.Logged (LogEntry " & Image (Time) & " Severity.Information " & Image (Source) & " """ &
      Text & """))");
   function Gap (Time, Source, Count : Unsigned_64; Reason : Logs.Gap_Reason) return String is
     ("(LogItem.Gap (LogGap " & Image (Time) & " " & Image (Source) & " " & Image (Count) & " GapReason." &
      (case Reason is
          when Logs.Not_Retained => "Not_Retained", when Logs.Lost => "Lost", when Logs.Not_Shown => "Not_Shown") &
      " """ & Logs.Gap_Message (Count, Reason) & """))");
   function Tally (Source, Retained, Not_Retained : Unsigned_64) return String is
     ("(LogItem.Tally (LogTally " & Image (Source) & " " & Image (Retained + Not_Retained) & " " &
      Image (Retained) & " " & Image (Not_Retained) & " 0 0 """ &
      Logs.Tally_Message (Retained + Not_Retained, Retained, Not_Retained, 0, 0) & """))");
   ACPI_TALLY : constant String := Tally (7, 151, 5);
   function Acpi (Step : Natural) return String is
     (Logged (Unsigned_64 (1_000 + 10 * Step), 7, "acpi: step" & Natural'Image (Step)));
   function Wifi (Source : Unsigned_64; First : Unsigned_64; Step : Natural) return String is
     (Logged (First + Unsigned_64 (10 * Step), Source, "wifi: step" & Natural'Image (Step)));

   function Between (Service : String; From, To : String) return String is
     ("(logs.between (LogWindow """ & Service & """ " & From & " " & To & "))");
   --  How many records a result shows, and how many Not_Shown gaps it has.
   function Shown_Records (List : String) return Integer_64 is
     (Integer_Of ("(length (where (fn ((i LogItem)) (match i ((LogItem.Logged e) true) ((LogItem.Gap g) false) " &
                  "((LogItem.Tally t) false))) " & List & "))"));
   function Not_Shown_Gaps (List : String) return Integer_64 is
     (Integer_Of ("(length (where (fn ((i LogItem)) (match i ((LogItem.Gap g) (= (field g reason) " &
                  "GapReason.Not_Shown)) ((LogItem.Logged e) false) ((LogItem.Tally t) false))) " & List & "))"));
   procedure Refused (Source : String; Why : CuBit.Failures.Reason; Because : String) is
      Asked : constant Natural := Log_Window_Script.Calls;
   begin
      Evaluate (Source);
      Check (Outcome.Status = Host_Call_Failed and then Outcome.Failure.Why = Why,
             Because & ": " & Source & " => " & Outcome.Status'Image & " " & Outcome.Failure.Why'Image);
      Check (Why = CuBit.Failures.Not_Found or else Log_Window_Script.Calls = Asked,
             Because & ": refused before logstore is asked");
   end Refused;
begin
   Initialize (Catalog);
   Initialize (Grants);
   CCL_Log_Bindings.Install (Catalog, Grants, Installed);
   Check (Installed, "the logs interface installs with between");

   --  A window inside the history: its records, and the tally; the records
   --  outside it are in no gap.
   Check (Literal_Of (Between ("acpi", "1100", "1150")) =
            "[" & Acpi (10) & " " & Acpi (11) & " " & Acpi (12) & " " & Acpi (13) & " " & Acpi (14) & " " &
            Acpi (15) & " " & ACPI_TALLY & "]", "a window's records: " & Literal);
   --  Named fields read the same.
   Check (Literal_Of ("(logs.between (LogWindow ""acpi"" from => 1100 to => 1150))") =
            Literal_Of (Between ("acpi", "1100", "1150")), "named LogWindow fields");
   --  Both ends are in the window.
   Check (Literal_Of (Between ("acpi", "1100", "1100")) = "[" & Acpi (10) & " " & ACPI_TALLY & "]",
          "a window of one millisecond: " & Literal);
   --  A gap touching the window is in it, at the time of the record before.
   Check (Literal_Of (Between ("acpi", "1495", "1505")) =
            "[" & Gap (1_490, 7, 5, Logs.Not_Retained) & " " & Acpi (50) & " " & ACPI_TALLY & "]",
          "a gap the window touches: " & Literal);
   Check (Literal_Of (Between ("acpi", "1491", "1499")) =
            "[" & Gap (1_490, 7, 5, Logs.Not_Retained) & " " & ACPI_TALLY & "]",
          "a window holding only a gap: " & Literal);

   --  Empty windows: before the first record and after the last.
   Check (Literal_Of (Between ("acpi", "0", "999")) = "[" & ACPI_TALLY & "]", "before the first: " & Literal);
   Check (Literal_Of (Between ("acpi", "2501", LATEST)) = "[" & ACPI_TALLY & "]", "after the last: " & Literal);
   Check (Integer_Of ("(length " & Between ("acpi", "0", "999") & ")") = 1, "an empty window holds the tally");

   --  A result is as large as its history (LOG-005 with CCL values sized
   --  by their content): the whole acpi history, all 151 records, and no
   --  Not_Shown gap; a window, its records only.
   declare
      Whole : constant String := Between ("acpi", "0", LATEST);
      Part : constant String := Between ("acpi", "1000", "2000");
      Records_In_Part : constant := 101;
   begin
      Check (Shown_Records (Whole) = Integer_64 (151), "the whole history is shown: all 151 records");
      Check (Not_Shown_Gaps (Whole) = 0, "and nothing is left out");
      Check (Shown_Records (Part) = Integer_64 (Records_In_Part), "a window shows its records only");
      Check (Not_Shown_Gaps (Part) = 0, "and none is left out");
      --  The not-retained records inside the window are still accounted.
      Evaluate (Part);
      Check (Outcome.Status = Succeeded and then
             Ada.Strings.Fixed.Index (Literal, " 7 5 GapReason.Not_Retained ") > 0,
             "the window's not-retained gap is kept: " & Literal);
   end;
   --  A whole retention plan's history (4096 records): every record, the
   --  first and the last, and the tally; far past a 16 KiB page.
   Check (Integer_Of ("(length (logs.recent ""busy""))") = 4_097, "4096 records and the tally");
   Evaluate ("(logs.recent ""busy"")");
   Check (Outcome.Status = Succeeded and then Outcome.Has_Literal, "busy's literal");
   Check (Ada.Strings.Fixed.Index (Literal, "busy: step 0""") > 0 and then
          Ada.Strings.Fixed.Index (Literal, "busy: step 4095""") > 0 and then
          Ada.Strings.Fixed.Index (Literal, "Not_Shown") = 0, "busy: first and last record, nothing left out");
   --  And a table of every row, as the console and the Workbench show it.
   declare
      Shown : CCL.Presentations.Presentation;
      use type CCL.Presentations.Form;
   begin
      CCL.Presentations.Describe (Outcome, Shown);
      Check (Shown.Kind = CCL.Presentations.Table and then CCL.Literal_Tables.Rows (Shown.Cells) = 4_097 and then
             CCL.Presentations.Cell_Text (Outcome, Shown, 4_096, 4) = """busy: step 4095""",
             "busy: a table of all 4097 rows");
   end;
   --  Past the named ceiling only: the first records and the newest, and
   --  one typed Not_Shown gap counting exactly what was left out.
   Check (Integer_Of ("(length (logs.recent ""flood""))") = Integer_64 (Logs.RESULT_ITEM_CEILING + 2),
          "the ceiling's items, the Not_Shown gap and the tally");
   Evaluate ("(logs.recent ""flood"")");
   Check (Outcome.Status = Succeeded and then
          Ada.Strings.Fixed.Index
            (Literal, " 22" & Natural'Image (Log_Window_Script.FLOOD_OVER) & " GapReason.Not_Shown ") > 0,
          "flood: the records past the ceiling are one Not_Shown gap");

   --  logs.recent is logs.between over the whole history.
   Check (Literal_Of ("(logs.recent ""acpi"")") = Literal_Of (Between ("acpi", "0", LATEST)), "recent = whole acpi");
   Check (Literal_Of ("(logs.recent ""many"")") = Literal_Of (Between ("many", "0", LATEST)), "recent = whole many");
   --  Two instances, oldest records first, each instance's tally.
   Check (Literal_Of ("(logs.recent ""wifi"")") =
            "[" & Wifi (11, 100, 0) & " " & Wifi (11, 100, 1) & " " & Wifi (11, 100, 2) & " " &
            Wifi (12, 500, 0) & " " & Wifi (12, 500, 1) & " " & Wifi (12, 500, 2) & " " &
            Tally (11, 3, 0) & " " & Tally (12, 3, 0) & "]", "recent over two instances: " & Literal);
   Check (Literal_Of (Between ("wifi", "100", "120")) =
            "[" & Wifi (11, 100, 0) & " " & Wifi (11, 100, 1) & " " & Wifi (11, 100, 2) & " " &
            Tally (11, 3, 0) & " " & Tally (12, 3, 0) & "]", "a window over the older instance: " & Literal);
   --  Instances beyond the shown ones: a whole history counts their
   --  retained records, a window only those inside it.
   --  The older instances' gap is at the first shown record's time.
   Check (Literal_Of ("(logs.recent ""many"")") (1 .. Gap (3_000, 0, 4, Logs.Not_Shown)'Length + 1) =
            "[" & Gap (3_000, 0, 4, Logs.Not_Shown), "the older instances' records: " & Literal);
   Check (Literal_Of (Between ("many", "1000", "1001")) (1 .. Gap (0, 0, 2, Logs.Not_Shown)'Length + 1) =
            "[" & Gap (0, 0, 2, Logs.Not_Shown), "the older instances' records in the window: " & Literal);
   Check (Integer_Of ("(length " & Between ("many", "1000", "1001") & ")") = 9,
          "an older instance's window: its gap and the shown instances' tallies");

   --  Refusals are typed failures, not crashes, and logstore is not asked.
   Refused (Between ("acpi", "2000", "1000"), CuBit.Failures.Invalid_Argument, "from > to");
   Check (Outcome.Failed_Operation.Length > 0, "the refused operation is named");
   Refused (Between ("acpi", "-1", "1000"), CuBit.Failures.Invalid_Argument, "before boot");
   Refused (Between ("", "0", "1000"), CuBit.Failures.Invalid_Argument, "no service");
   Refused (Between ("nobody", "0", "1000"), CuBit.Failures.Not_Found, "an unknown service");
   --  The window is one typed argument, not three.
   Evaluate ("(logs.between ""acpi"" 0 1000)");
   Check (Outcome.Status in Parse_Failed | Type_Check_Failed, "three bare arguments are refused by the analyser: " &
          Outcome.Status'Image);
   Evaluate ("(logs.between (LogWindow ""acpi"" 0))");
   Check (Outcome.Status in Parse_Failed | Type_Check_Failed, "a LogWindow names both ends: " &
          Outcome.Status'Image);
   Ada.Text_IO.Put_Line ("Log window: PASS" & Checks'Image & " checks");
end Log_Window_Tests;

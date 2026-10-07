with CCL.Evaluation;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Objects; use CCL.Objects;
with CCL.Catalog; use CCL.Catalog;
with CCL.Host_Values;
with CCL.Language; use CCL.Language;
   use CCL.Evaluation;
with CCL.Interfaces.Logs;

--  The REPL log viewer's path, hosted: (logs.recent "service") through the
--  interpreter, a host answering with a typed List<LogEntry> image, and the
--  list used as ordinary CCL data (length, at, field, where, literal).
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
   --  A logstore stand-in: three records for "netstack", none otherwise.
   procedure Invoke
     (State : in out Host; Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Image : CCL.Objects.Image;
      Added : Boolean;
   begin
      State.Calls := State.Calls + 1;
      Check (Binding = RECENT_BINDING and then Argument.Kind = CCL.Host_Values.Text_Value, "recent's argument");
      State.Asked_Length := Argument.Content.Length;
      State.Asked (1 .. State.Asked_Length) := Argument.Content.Data (1 .. State.Asked_Length);
      Logs.Start (Contract, Image);
      if State.Asked (1 .. State.Asked_Length) = "netstack" then
         Logs.Add (Image, 10, Logs.Information, 7, "boot", Added); Check (Added, "add boot");
         Logs.Add (Image, 20, Logs.Error, 7, "link down", Added); Check (Added, "add error");
         Logs.Add (Image, 30, Logs.Information, 7, "link up", Added); Check (Added, "add up");
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
   Full : CCL.Objects.Image;
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
          Outcome.Literal.Data (1 .. Outcome.Literal.Length) =
            "[(LogEntry 10 Severity.Information 7 ""boot"") (LogEntry 20 Severity.Error 7 ""link down"") " &
            "(LogEntry 30 Severity.Information 7 ""link up"")]", "the list's literal");
   Check (Context.Asked (1 .. Context.Asked_Length) = "netstack", "the service asked for");
   Evaluate ("(length (logs.recent ""netstack""))");
   Check (Outcome.Status = Succeeded and then Outcome.Result_Value.Integer = 3, "length");
   Evaluate ("(field (at (logs.recent ""netstack"") 2) message)");
   Check (Outcome.Status = Succeeded and then Outcome.Has_Text and then
          Outcome.Result_Text.Data (1 .. Outcome.Result_Text.Length) = "link down", "a field of an entry");
   Evaluate ("(length (where (fn ((e LogEntry)) (= (field e severity) Severity.Error)) (logs.recent ""netstack"")))");
   Check (Outcome.Status = Succeeded and then Outcome.Result_Value.Integer = 1, "errors only");
   Evaluate ("(each (fn ((e LogEntry)) (field e message)) (logs.recent ""netstack""))");
   Check (Outcome.Status = Succeeded and then Outcome.Has_List and then Outcome.List_Total = 3, "messages");
   Evaluate ("(length (logs.recent ""nobody""))");
   Check (Outcome.Status = Succeeded and then Outcome.Result_Value.Integer = 0, "an unknown service has none");

   --  The builder adds only whole entries and stops at the image's bound.
   Logs.Start (Contract, Full);
   while Added loop
      Logs.Add (Full, 1, Logs.Debug, 2, "x", Added);
      if Added then Count := Count + 1; end if;
   end loop;
   Check (Count = Logs.MAX_ENTRIES, "a full result holds MAX_ENTRIES");
   Check (Validate (Full, Contract), "a full result is a valid image");
   Ada.Text_IO.Put_Line ("Log view: PASS" & Checks'Image & " checks");
end Log_View_Tests;

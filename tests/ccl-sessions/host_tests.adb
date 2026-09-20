with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Sessions; use CCL.Sessions;
with CCL.Catalog; use CCL.Catalog;
with CCL.Language; use CCL.Language;
with CCL.VM;
with CCL.Interfaces.Clock;
with CCL.Interfaces.Workbench_UI;
with CCL.UI_Labels;
with CCL.Compiler;

procedure Host_Tests is
   use type CCL.VM.Value_Kind;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.VM.Validation_Error;
   use type CCL.VM.Execution_Status;
   type Host_State is record
      Calls : Natural := 0;
      Label : CCL.UI_Labels.Model;
      Fail, Wrong_Type : Boolean := False;
   end record;
   procedure Invoke
     (Context : in out Host_State; Binding : Unsigned_32;
      Argument : CCL.VM.Value; Value : out CCL.VM.Value; Success : out Boolean) is
   begin
      Context.Calls := Context.Calls + 1;
      Success := False;
      Value := CCL.VM.Integer_Constant (123_456);
      if Context.Fail then return; end if;
      case Binding is
         when 77 => Success := Argument.Kind = CCL.VM.Integer_Value and then Argument.Integer = 0;
         when 78 | 79 =>
            CCL.UI_Labels.Apply
              (Context.Label,
               (if Binding = 78 then CCL.UI_Labels.Set_Value else CCL.UI_Labels.Set_Visible),
               Argument, Success);
            Value := CCL.VM.Boolean_Constant (Success);
         when others => null;
      end case;
      if Context.Wrong_Type then Value := CCL.VM.Boolean_Constant (True); end if;
   end Invoke;
   procedure Submit_Host is new Submit_With_Host (Host_State, Invoke);
   Catalog : Interface_Catalog;
   Grants, Missing : Granted_Bindings;
   S, Isolated : Session;
   Host, Other_Host : Host_State;
   Outcome : Interpretation_Result;
   Entry_Value : Submission;
   Found : Boolean;
   Error : Catalog_Error;
   procedure Grant (Name : String; Binding : Unsigned_32) is
      Op : Resolved_Operation;
      Result : Grant_Result;
   begin
      Resolve (Catalog, Name, Op, Found); pragma Assert (Found);
      Install (Grants, Op, Binding, Result); pragma Assert (Result = Grant_Added);
   end Grant;
   procedure Run (Text : String; Fuel : Fuel_Budget := Default_Fuel) is
   begin
      Submit_Host (S, Text, Fuel, Grants, Host, Outcome);
   end Run;
begin
   Initialize (Catalog); Initialize (Grants); Initialize (Missing);
   CCL.Interfaces.Clock.Publish (Catalog, Error); pragma Assert (Error = Catalog_Valid);
   CCL.Interfaces.Workbench_UI.Publish (Catalog, Error); pragma Assert (Error = Catalog_Valid);
   Initialize (S, Catalog); Initialize (Isolated, Catalog);
   Grant ("clock.monotonic-ms", 77);
   Run ("(ui.label-value (clock.monotonic-ms))");
   pragma Assert (Outcome.Status = Host_Authority_Denied and Host.Calls = 0);
   pragma Assert (Length (S) = 1 and not CCL.UI_Labels.Visible (Host.Label));
   Grant ("ui.label-value", 78); Grant ("ui.label-visible", 79);
   Run ("(ui.label-value true)");
   pragma Assert (Outcome.Status = Type_Check_Failed and Host.Calls = 0);
   Run ("(type Color (enum Red)) (ui.label-value Color.Red)");
   pragma Assert (Outcome.Status = Type_Check_Failed and Host.Calls = 0);
   Run ("(type Color (enum Red)) (ui.label-visible Color.Red)");
   pragma Assert (Outcome.Status = Type_Check_Failed and Host.Calls = 0);
   Run ("(ui.label-value 42)", 0);
   pragma Assert (Outcome.Status = Evaluation_Fuel_Exhausted and Host.Calls = 0);
   Run ("(ui.label-value (clock.monotonic-ms))");
   pragma Assert (Outcome.Status = Succeeded and Host.Calls = 2);
   pragma Assert (CCL.UI_Labels.Visible (Host.Label) and CCL.UI_Labels.Value (Host.Label) = 123_456);
   pragma Assert (CCL.UI_Labels.Changed (Host.Label));
   CCL.UI_Labels.Painted (Host.Label);
   Run ("(ui.label-value 123456)");
   pragma Assert (not CCL.UI_Labels.Changed (Host.Label));
   Run ("(to-string (clock.monotonic-ms))");
   pragma Assert (Result_Image (Outcome) = "String: 123456" and Host.Calls = 4);
   Recall (S, Length (S), Entry_Value, Found);
   pragma Assert (Found and Entry_Value.Outcome.Result_Text = Outcome.Result_Text);
   Clear_History (S);
   pragma Assert (Length (S) = 0 and Host.Calls = 4 and CCL.UI_Labels.Visible (Host.Label));
   Run ("(ui.label-visible false)");
   pragma Assert (not CCL.UI_Labels.Visible (Host.Label) and Host.Calls = 5);
   Submit_Host (Isolated, "(ui.label-value 9)", Default_Fuel, Missing, Other_Host, Outcome);
   pragma Assert (Outcome.Status = Host_Authority_Denied and Other_Host.Calls = 0);
   Host.Fail := True;
   Run ("(+ (clock.monotonic-ms) (clock.monotonic-ms))");
   pragma Assert (Outcome.Status = Host_Call_Failed and Host.Calls = 6);
   Host.Fail := False; Host.Wrong_Type := True;
   Run ("(clock.monotonic-ms)");
   pragma Assert (Outcome.Status = Host_Result_Type_Mismatch and Host.Calls = 7);
   Run (String'(1 .. MAX_SOURCE_LENGTH + 1 => 'a'));
   pragma Assert (Outcome.Status = Parse_Failed and Host.Calls = 7);
   declare
      Analysis : Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Verified : CCL.VM.Validated_Program;
      Machine : CCL.VM.Machine_State;
      Execution : CCL.VM.Execution_Result;
      Link : Link_Result;
      Validation : CCL.VM.Validation_Error;
      Response : CCL.VM.Value;
      Accepted : Boolean;
   begin
      Host := (others => <>);
      Analyze ("(ui.label-value 42)", Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      pragma Assert (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      Link_Program (Grants, Compiled.Linkage, Compiled.Program, Link);
      pragma Assert (Link = Link_Valid);
      CCL.VM.Verify (Compiled.Program, Verified, Validation);
      pragma Assert (Validation = CCL.VM.Valid);
      CCL.VM.Initialize (Verified, 4096, Machine);
      CCL.VM.Continue_Execution_For (Verified, Machine, 4096, Execution);
      pragma Assert (Execution.Status = CCL.VM.Waiting_For_Host);
      Invoke (Host, Execution.Requested_Binding, Execution.Request_Argument, Response, Accepted);
      CCL.VM.Complete_Host_Call (Verified, Machine, Response, Accepted);
      CCL.VM.Continue_Execution_For (Verified, Machine, 4096, Execution);
      pragma Assert (Execution.Status = CCL.VM.Completed and Execution.Result_Value.Boolean);
      pragma Assert (Host.Calls = 1 and CCL.UI_Labels.Value (Host.Label) = 42);
   end;
   Put_Line ("PASS: hosted REPL admission, exactly-once evaluation, isolation and scoped label lifecycle");
end Host_Tests;

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog; use CCL.Catalog;
with CCL.Host_Values; use CCL.Host_Values;
with CCL.Interfaces.Clock;
with CCL.Interfaces.Workbench_UI;
with CCL.Language; use CCL.Language;
with CCL.Sessions;
with CCL.Periodic_Programs;
with CCL.UI_Labels;
with CCL.Compiler;
with CCL.VM;

procedure Text_Tests is
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Periodic_Programs.Load_Result;
   type Host_State is record
      Calls : Natural := 0;
      Label : CCL.UI_Labels.Model;
      Return_Too_Long : Boolean := False;
   end record;
   procedure Invoke
     (Context : in out Host_State; Binding : Unsigned_32;
      Argument : Value; Value : out CCL.Host_Values.Value; Success : out Boolean) is
      Buffer : Text;
   begin
      Context.Calls := Context.Calls + 1;
      Value := Boolean_Constant (False);
      case Binding is
         when 77 => Value := Integer_Constant (3661000); Success := True;
         when 78 =>
            CCL.UI_Labels.Apply_Value (Context.Label, CCL.UI_Labels.Set_Text, Argument, Success);
            Value := Boolean_Constant (Success);
         when 79 =>
            Copy_Text ((if Context.Return_Too_Long then "oversize" else "hello"), Buffer, Success);
            Value := Text_Constant (Buffer);
         when others => Success := False;
      end case;
   end Invoke;
   procedure Scalar_Invoke
     (Context : in out Host_State; Binding : Unsigned_32;
      Argument : CCL.VM.Value; Value : out CCL.VM.Value; Success : out Boolean) is
      pragma Unreferenced (Binding, Argument);
   begin
      Context.Calls := Context.Calls + 1;
      Value := CCL.VM.Integer_Constant (0); Success := True;
   end Scalar_Invoke;
   procedure Submit is new CCL.Sessions.Submit_With_Values (Host_State, Invoke);
   procedure Scalar_Submit is new CCL.Sessions.Submit_With_Host (Host_State, Scalar_Invoke);
   function Now (Context : Host_State) return Unsigned_64 is
      pragma Unreferenced (Context);
   begin return 1000; end Now;
   procedure Pump is new CCL.Periodic_Programs.Evaluate_Values_Due (Host_State, Now, Invoke);
   Catalog : Interface_Catalog;
   Grants, Missing : Granted_Bindings;
   S : CCL.Sessions.Session;
   Host : Host_State;
   Outcome : Interpretation_Result;
   Error : Catalog_Error;
   Descriptor : Interface_Descriptor;
   Operation : Operation_Descriptor;
   Resolved, Changed : Resolved_Operation;
   Grant : Grant_Result;
   Found : Boolean;
   Binding : Unsigned_32;
   procedure Install_Name (Name : String; Id : Unsigned_32) is
   begin
      Resolve (Catalog, Name, Resolved, Found); pragma Assert (Found);
      Install (Grants, Resolved, Id, Grant); pragma Assert (Grant = Grant_Added);
   end Install_Name;
   procedure Run (Source : String) is
   begin Submit (S, Source, 4096, Grants, Host, Outcome); end Run;
begin
   Initialize (Catalog); Initialize (Grants); Initialize (Missing);
   CCL.Interfaces.Clock.Publish (Catalog, Error); pragma Assert (Error = Catalog_Valid);
   CCL.Interfaces.Workbench_UI.Publish (Catalog, Error); pragma Assert (Error = Catalog_Valid);
   Define_Interface ("test", 1, 0, [others => 1], Descriptor, Error);
   Define_Host_Operation ("echo", 1,
     (Argument => Text_Value, Argument_Text_Limit => 5,
      Result => Text_Value, Result_Text_Limit => 5, others => <>), Operation, Error);
   pragma Assert (Error = Catalog_Valid);
   Add_Operation (Descriptor, Operation, Error);
   Publish (Catalog, Descriptor, Error); pragma Assert (Error = Catalog_Valid);
   CCL.Sessions.Initialize (S, Catalog);
   Install_Name ("clock.monotonic-ms", 77);
   Run ("(ui.label-text (to-string (clock.monotonic-ms)))");
   pragma Assert (Outcome.Status = Host_Authority_Denied and Host.Calls = 0);
   Install_Name ("ui.label-text", 78);
   Changed := Resolved;
   Changed.Import.Argument_Text_Limit := 5;
   Find_Granted_Binding (Grants, Changed, Binding, Found);
   pragma Assert (not Found); -- same digest/name/operation, different contract
   Run ("(ui.label-text 42)");
   pragma Assert (Outcome.Status = Type_Check_Failed and Host.Calls = 0);
   Run ("(ui.label-text (concat ""Hello, "" ""Cubie""))");
   pragma Assert (Outcome.Status = Succeeded and Host.Calls = 1);
   pragma Assert (CCL.UI_Labels.Image (Host.Label) = " Hello, Cubie");
   CCL.UI_Labels.Painted (Host.Label);
   Run ("(ui.label-text ""Hello, Cubie"")");
   pragma Assert (Outcome.Status = Succeeded and not CCL.UI_Labels.Changed (Host.Label));
   CCL.Sessions.Clear_History (S);
   pragma Assert (CCL.UI_Labels.Image (Host.Label) = " Hello, Cubie");
   Run ("(ui.label-text """")");
   pragma Assert (Outcome.Status = Succeeded and CCL.UI_Labels.Visible (Host.Label));
   pragma Assert (CCL.UI_Labels.Image (Host.Label) = " ");
   Run ("(ui.label-text (to-string (clock.monotonic-ms)))");
   pragma Assert (Outcome.Status = Succeeded and CCL.UI_Labels.Image (Host.Label) = " 3661000");
   declare
      Before : constant Natural := Host.Calls;
   begin
      Scalar_Submit (S, "(ui.label-text (to-string (clock.monotonic-ms)))", 4096,
        Grants, Host, Outcome);
      pragma Assert (Outcome.Status = Host_Contract_Unsupported and Host.Calls = Before);
   end;
   Install_Name ("test.echo", 79);
   Run ("(concat (test.echo ""12345"") "" world"")");
   pragma Assert (Outcome.Status = Succeeded and Outcome.Has_Text);
   pragma Assert (Outcome.Result_Text.Data (1 .. Outcome.Result_Text.Length) = "hello world");
   declare
      Before : constant Natural := Host.Calls;
   begin
      Run ("(test.echo ""123456"")");
      pragma Assert (Outcome.Status = Host_Argument_Out_Of_Bounds and Host.Calls = Before);
   end;
   Host.Return_Too_Long := True;
   Run ("(test.echo ""x"")");
   pragma Assert (Outcome.Status = Host_Result_Type_Mismatch);
   declare
      Analysis : Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Linkage : Linkage_Table;
      Index : CCL.VM.Import_Index;
      Interned : Intern_Result;
      Program : CCL.VM.Program;
      Linked : Link_Result;
   begin
      Analyze ("(ui.label-text ""hello"")", Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      pragma Assert (Compiled.Status = CCL.Compiler.Unsupported_Form);
      Resolve (Catalog, "ui.label-text", Resolved, Found); pragma Assert (Found);
      Initialize (Linkage);
      Intern (Linkage, Resolved, Index, Interned); pragma Assert (Interned = Linkage_Added);
      Program.Imports_Length := 1;
      Link_Program (Grants, Linkage, Program, Linked);
      pragma Assert (Linked = Import_Contract_Mismatch and Program.Imports (Index).Binding = 0);
   end;
   declare
      Item : Text;
      Copied : Boolean;
      Exact : constant String (1 .. Maximum_Text_Length) := [others => 'x'];
   begin
      Copy_Text (Exact, Item, Copied); pragma Assert (Copied);
      CCL.UI_Labels.Apply_Value (Host.Label, CCL.UI_Labels.Set_Text, Text_Constant (Item), Copied);
      pragma Assert (Copied and CCL.UI_Labels.Image (Host.Label)'Length = Maximum_Text_Length + 1);
      Copy_Text (Exact & "x", Item, Copied); pragma Assert (not Copied);
   end;
   declare
      Half : constant String (1 .. Maximum_Text_Length / 2) := [others => 'x'];
   begin
      -- A runtime-produced maximum-size argument without exceeding source size.
      Run ("(let ((s """ & Half & """)) (ui.label-text (concat s s)))");
      pragma Assert (Outcome.Status = Succeeded);
      pragma Assert (CCL.UI_Labels.Image (Host.Label)'Length = Maximum_Text_Length + 1);
   end;
   declare
      Program : CCL.Periodic_Programs.Program;
      Loaded : CCL.Periodic_Programs.Load_Result;
      Updated : Boolean;
   begin
      CCL.Periodic_Programs.Load (Program, "(ui.label-text ""watched"")", 1000, 1000, 4096, Loaded);
      pragma Assert (Loaded = CCL.Periodic_Programs.Loaded);
      Pump (Program, Catalog, Grants, Host, Updated);
      pragma Assert (Updated and CCL.UI_Labels.Image (Host.Label) = " watched");
      Pump (Program, Catalog, Grants, Host, Updated);
      pragma Assert (not Updated);
   end;
   Put_Line ("PASS: bounded text host values, grants, owned labels, returned strings, scalar rejection and Watch");
end Text_Tests;

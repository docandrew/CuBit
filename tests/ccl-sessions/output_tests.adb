with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog; use CCL.Catalog;
with CCL.Host_Values; use CCL.Host_Values;
with CCL.Interfaces.Workbench_UI;
with CCL.Language; use CCL.Language;
with CCL.Sessions;
with CCL.UI_Outputs;

procedure Output_Tests is
   type Host_State is record
      Output : CCL.UI_Outputs.Model;
      Calls : Natural := 0;
   end record;
   procedure Invoke
     (Context : in out Host_State; Binding : Unsigned_32;
      Argument : Value; Value : out CCL.Host_Values.Value;
      Success : out Boolean)
   is
      Accepted : Boolean;
   begin
      Context.Calls := Context.Calls + 1;
      CCL.UI_Outputs.Apply (Context.Output,
        (if Binding = 1 then CCL.UI_Outputs.Append_Line
         else CCL.UI_Outputs.Clear_Output), Argument, Accepted);
      Value := Boolean_Constant (Accepted);
      Success := True;
   end Invoke;
   procedure Submit is new CCL.Sessions.Submit_With_Values (Host_State, Invoke);
   Catalog : Interface_Catalog;
   Grants : Granted_Bindings;
   S : CCL.Sessions.Session;
   Host : Host_State;
   Outcome : Interpretation_Result;
   Error : Catalog_Error;
   Resolved : Resolved_Operation;
   Grant : Grant_Result;
   Found, Accepted : Boolean;
   procedure Run (Source : String) is
   begin
      Submit (S, Source, 4096, Grants, Host, Outcome);
   end Run;
begin
   Initialize (Catalog);
   Initialize (Grants);
   CCL.Interfaces.Workbench_UI.Publish (Catalog, Error);
   pragma Assert (Error = Catalog_Valid);
   CCL.Sessions.Initialize (S, Catalog);
   Run ("(ui.output-append ""denied"")");
   pragma Assert (Outcome.Status = Host_Authority_Denied and Host.Calls = 0);
   for Op in CCL.UI_Outputs.Operation loop
      Resolve (Catalog, "ui." & CCL.UI_Outputs.Name (Op), Resolved, Found);
      pragma Assert (Found);
      Install (Grants, Resolved,
        Unsigned_32 (CCL.UI_Outputs.Operation'Pos (Op) + 1), Grant);
      pragma Assert (Grant = Grant_Added);
   end loop;
   Run ("(ui.output-append 42)");
   pragma Assert (Outcome.Status = Type_Check_Failed and Host.Calls = 0);
   Run ("(ui.output-append (concat ""Hello, "" ""Cubie!""))");
   pragma Assert (Outcome.Status = Succeeded);
   pragma Assert (CCL.UI_Outputs.Content (Host.Output) = "Hello, Cubie!" & ASCII.LF);
   Run ("(ui.output-append ""another line"")");
   pragma Assert (Outcome.Status = Succeeded);
   pragma Assert (CCL.UI_Outputs.Content (Host.Output) =
     "Hello, Cubie!" & ASCII.LF & "another line" & ASCII.LF);
   CCL.Sessions.Clear_History (S);
   pragma Assert (CCL.UI_Outputs.Length (Host.Output) > 0);
   Run ("(ui.output-clear)");
   pragma Assert (Outcome.Status = Succeeded and
     CCL.UI_Outputs.Length (Host.Output) = 0);
   CCL.UI_Outputs.Painted (Host.Output);
   CCL.UI_Outputs.Clear (Host.Output);
   pragma Assert (not CCL.UI_Outputs.Changed (Host.Output));
   declare
      Text : constant String (17 .. 19) := "abc";
   begin
      CCL.UI_Outputs.Append (Host.Output, Text, Accepted);
   end;
   pragma Assert (Accepted and CCL.UI_Outputs.Content (Host.Output) = "abc" & ASCII.LF);
   CCL.UI_Outputs.Clear (Host.Output);
   CCL.UI_Outputs.Append (Host.Output, "", Accepted);
   pragma Assert (Accepted and CCL.UI_Outputs.Content (Host.Output) = String'(1 => ASCII.LF));
   CCL.UI_Outputs.Clear (Host.Output);
   CCL.UI_Outputs.Append (Host.Output,
     String'(1 .. CCL.UI_Outputs.Maximum_Length - 1 => 'x'), Accepted);
   pragma Assert (Accepted and
     CCL.UI_Outputs.Length (Host.Output) = CCL.UI_Outputs.Maximum_Length);
   CCL.UI_Outputs.Painted (Host.Output);
   Run ("(ui.output-append ""does not fit"")");
   pragma Assert (Outcome.Status = Succeeded);
   pragma Assert (CCL.Sessions.Result_Image (Outcome) = "Boolean: false");
   pragma Assert (not CCL.UI_Outputs.Changed (Host.Output));
   pragma Assert (CCL.UI_Outputs.Content (Host.Output) =
     String'(1 .. CCL.UI_Outputs.Maximum_Length - 1 => 'x') & ASCII.LF);
   CCL.UI_Outputs.Clear (Host.Output);
   CCL.UI_Outputs.Append (Host.Output,
     String'(1 .. CCL.UI_Outputs.Maximum_Length => 'x'), Accepted);
   pragma Assert (not Accepted and CCL.UI_Outputs.Length (Host.Output) = 0);
   Put_Line ("PASS: typed, authorized output; persistent lines, atomic exhaustion, clear");
end Output_Tests;

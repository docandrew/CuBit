with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog; use CCL.Catalog;
with CCL.Interfaces.Clock;
with CCL.Language; use CCL.Language;
with CCL.Sessions;
with CCL.Imports;
with Interpreter_Host;

procedure Host_Tests is
   Catalog, Empty : Interface_Catalog;
   Grants, Missing : Granted_Bindings;
   Operation, Forged : Resolved_Operation;
   Error : Catalog_Error;
   Grant : Grant_Result;
   Found : Boolean;
   Context : Interpreter_Host.State;
   Result : Interpretation_Result;
   Source : constant String :=
     "(let ((elapsed-ms (clock.monotonic-ms)))" &
     " (let ((hours (to-string (/ elapsed-ms 3600000))))" &
     " (let ((minutes (to-string (/ (mod elapsed-ms 3600000) 60000))))" &
     " (let ((seconds (to-string (/ (mod elapsed-ms 60000) 1000))))" &
     " (concat (if (= (length hours) 1) (concat ""0"" hours) hours)" &
     " (concat "":"" (concat (if (= (length minutes) 1) (concat ""0"" minutes) minutes)" &
     " (concat "":"" (if (= (length seconds) 1) (concat ""0"" seconds) seconds)))))))))";
   procedure Run (Text : String; Fuel : Natural := 4096) is
   begin
      Interpreter_Host.Evaluate (Text, Fuel, Catalog, Grants, Context, Result);
   end Run;
begin
   Initialize (Catalog); Initialize (Empty); Initialize (Grants); Initialize (Missing);
   CCL.Interfaces.Clock.Publish (Catalog, Error); pragma Assert (Error = Catalog_Valid);
   CCL.Interfaces.Clock.Resolve_Monotonic_Ms (Catalog, Operation, Found); pragma Assert (Found);
   Run ("(clock.monotonic-ms)");
   pragma Assert (Result.Status = Host_Authority_Denied and Context.Calls = 0);
   Forged := Operation; Forged.Interface_Digest (0) := Forged.Interface_Digest (0) + 1;
   Install (Grants, Forged, 77, Grant); pragma Assert (Grant = Grant_Added);
   Run ("(clock.monotonic-ms)");
   pragma Assert (Result.Status = Host_Authority_Denied and Context.Calls = 0);
   Initialize (Grants); Install (Grants, Operation, 77, Grant); pragma Assert (Grant = Grant_Added);
   Interpreter_Host.Evaluate ("(clock.monotonic-ms)", 4096, Empty, Grants, Context, Result);
   pragma Assert (Result.Status = Parse_Failed and Context.Calls = 0);
   Interpret ("(clock.monotonic-ms)", 4096, Catalog, Result);
   pragma Assert (Result.Status = Host_Import_Required and Context.Calls = 0);
   Run ("(+ true (clock.monotonic-ms))");
   pragma Assert (Result.Status = Type_Check_Failed and Context.Calls = 0);
   Run ("(if true 7 (clock.monotonic-ms))");
   pragma Assert (Result.Status = Succeeded and Context.Calls = 0);
   Run ("(clock.monotonic-ms)", 0);
   pragma Assert (Result.Status = Evaluation_Fuel_Exhausted and Context.Calls = 0);
   Run (Source);
   pragma Assert (Result.Status = Succeeded and Context.Calls = 1);
   pragma Assert (CCL.Sessions.Result_Image (Result) = "String: 01:01:01");
   -- Exercise string storage and decimal boundaries through both evaluator
   -- instantiations. These are representation regressions, not host effects.
   declare
      procedure Check_Text (Expression, Expected : String) is
         Plain : Interpretation_Result;
      begin
         Run (Expression);
         Interpret (Expression, 4096, Plain);
         pragma Assert (Result.Status = Succeeded and Plain.Status = Succeeded);
         pragma Assert (Result.Has_Text and Plain.Has_Text);
         pragma Assert (Result.Result_Text.Data (1 .. Result.Result_Text.Length) = Expected);
         pragma Assert (Plain.Result_Text.Data (1 .. Plain.Result_Text.Length) = Expected);
      end Check_Text;
   begin
      Check_Text ("""""", "");
      Check_Text ("(concat """" """" )", "");
      Check_Text ("(concat ""left"" """")", "left");
      Check_Text ("(concat """" ""right"")", "right");
      Check_Text ("(to-string 0)", "0");
      Check_Text ("(to-string -1)", "-1");
      Check_Text ("(to-string 9223372036854775807)", "9223372036854775807");
      Check_Text ("(to-string -9223372036854775808)", "-9223372036854775808");
      declare
         Chunk : constant String (1 .. 512) := [others => 'x'];
         Definition : constant String := "(let ((s """ & Chunk & """)) ";
      begin
         Check_Text (Definition & "(concat s s))", Chunk & Chunk);
         Run (Definition & "(concat (concat s s) ""x""))");
         pragma Assert (Result.Status = Evaluation_Text_Storage_Exhausted and not Result.Has_Value);
      end;
   end;
   Context := (others => <>);
   Run ("(+ (clock.monotonic-ms) (clock.monotonic-ms))");
   pragma Assert (Result.Status = Succeeded and Context.Calls = 2 and Result.Result_Value.Integer = 7_322_001);
   Context := (Fail => True, others => <>);
   Run ("(+ (clock.monotonic-ms) (clock.monotonic-ms))");
   pragma Assert (Result.Status = Host_Call_Failed and Context.Calls = 1 and not Result.Has_Value);
   Context := (Wrong_Type => True, others => <>);
   Run ("(clock.monotonic-ms)");
   pragma Assert (Result.Status = Host_Result_Type_Mismatch and Context.Calls = 1 and not Result.Has_Value);
   declare
      D : Interface_Descriptor;
      O : Operation_Descriptor;
   begin
      Define_Interface ("clock", 1, 0, Operation.Interface_Digest, D, Error);
      Define_Operation ("monotonic-ms", 0, Operation.Import, O, Error);
      Add_Operation (D, O, Error); pragma Assert (Error = Catalog_Valid);
      Define_Operation ("other", 0, Operation.Import, O, Error);
      Add_Operation (D, O, Error); pragma Assert (Error = Catalog_Valid);
      Initialize (Catalog); Publish (Catalog, D, Error); pragma Assert (Error = Catalog_Valid);
      Context := (others => <>);
      Run ("(+ (clock.monotonic-ms) (clock.other))");
      pragma Assert (Result.Status = Host_Authority_Denied and Context.Calls = 0);
   end;
   -- An advertised cancellation protocol cannot silently become a blocking
   -- scalar callback; the entire expression is rejected before any invocation.
   declare
      D : Interface_Descriptor;
      O : Operation_Descriptor;
   begin
      Define_Interface ("clock", 1, 0, Operation.Interface_Digest, D, Error);
      Forged := Operation;
      Forged.Import.Cancellation := CCL.Imports.Best_Effort_Cancellation;
      Define_Operation ("monotonic-ms", 0, Forged.Import, O, Error);
      pragma Assert (Error = Catalog_Valid);
      Add_Operation (D, O, Error); pragma Assert (Error = Catalog_Valid);
      Initialize (Catalog); Publish (Catalog, D, Error); pragma Assert (Error = Catalog_Valid);
      Initialize (Grants); Install (Grants, Forged, 77, Grant); pragma Assert (Grant = Grant_Added);
      Context := (others => <>); Run ("(clock.monotonic-ms)");
      pragma Assert (Result.Status = Host_Contract_Unsupported and Context.Calls = 0);
   end;
   Put_Line ("PASS: interpreter host authority/digest gates, no eager calls, live values, fuel, errors and HH:MM:SS");
end Host_Tests;

with CCL.Evaluation;
with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces;
with CCL.Host_Values;
with CCL.Types;
with CCL.Catalog;
with CCL.Language;
with CCL.Compiler;
with CCL.Imports;
with CCL.Ownership;
with CCL.Resource_Policies;
with CCL.Resources;
with CCL.Objects;
with CCL.Objects.Values;
with CCL.VM;

procedure Signature_Tests is
   package H renames CCL.Host_Values;
   package T renames CCL.Types;
   package C renames CCL.Catalog;
   package L renames CCL.Language;
   package R renames CCL.Resources;
   use type T.Type_Reference;
   use type T.Definition_Result;
   use type T.Import_Result;
   use type C.Catalog_Error;
   use type C.Resource_Publication;
   use type C.Grant_Result;
   use type L.Analysis_Status;
   use type L.Interpretation_Status;
   use type CCL.Compiler.Compilation_Status;
   use type R.Outcome;
   Types : T.Registry;
   Kind, Other, Ref : T.Type_Reference;
   Defined : T.Definition_Result;
   Imported : T.Import_Result;
   Catalog, Unapproved : C.Interface_Catalog;
   Policy : CCL.Resource_Policies.Description :=
     (Mode => CCL.Ownership.Must_Handle, Count => 1, others => <>);
   Published : C.Resource_Publication;
   Descriptor : C.Interface_Descriptor;
   Operation : C.Operation_Descriptor;
   Error : C.Catalog_Error;
   Factory, Read_Call, Close_Call, Write_Call, Bad : H.Import_Declaration;
   Analysis : L.Analysis_Result;
   Compiled : CCL.Compiler.Compilation_Result;
   Grants : C.Granted_Bindings;
   Resolved : C.Resolved_Operation;
   Found : Boolean;
   Installed : C.Grant_Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Site; end if;
   end Check;
   procedure Define_Call (Name : String; Parameters : C.Parameter_Count; Import : H.Import_Declaration) is
   begin
      C.Define_Host_Operation (Name, Parameters, Import, Operation, Error);
      Check (Error = C.Catalog_Valid);
      C.Add_Operation (Descriptor, Operation, Error);
      Check (Error = C.Catalog_Valid);
   end Define_Call;
   procedure Analyze (Source : String; Expected : T.Type_Reference) is
   begin
      L.Analyze (Source, Catalog, Analysis);
      Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Succeeded);
      Check (L.Analysis_Node (Analysis, L.Analysis_Root (Analysis)).Static_Kind = Expected);
   end Analyze;
   type Context is record
      Calls : Natural := 0;
   end record;
   State : Context;
   procedure Invoke
     (State : in out Context; Binding : Interfaces.Unsigned_32;
      Argument : H.Value; Reply : out H.Call_Result)
   is
      pragma Unreferenced (Binding, Argument);
   begin
      State.Calls := State.Calls + 1;
      Reply := (others => <>);
   end Invoke;
   procedure Evaluate is new CCL.Evaluation.Evaluate_With_Values (Context, Invoke);
   Result : L.Interpretation_Result;
begin
   T.Define (Types, (Identifier => T.Named ("IntegerCollection"), Form => T.Resource,
     Count => 1, Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Kind, Defined);
   Check (Defined = T.Defined);
   T.Define (Types, (Identifier => T.Named ("OtherCollection"), Form => T.Resource,
     Count => 1, Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Other, Defined);
   Check (Defined = T.Defined);
   Policy.Dispositions (0) := (Verb => 1, Effect => CCL.Ownership.Consume, others => <>);
   C.Publish_Resource (Catalog, Types, Kind, Policy, Ref, Published);
   Check (Published = C.Resource_Published);
   Kind := Ref;
   C.Publish_Resource (Catalog, Types, Other, Policy, Ref, Published);
   Check (Published = C.Resource_Published);

   Factory := (Result => H.Resource_Value, Result_Resource => T.Named ("IntegerCollection"), others => <>);
   Read_Call := (Argument => H.Resource_Value, Argument_Resource => Factory.Result_Resource,
     Ownership_Argument => True, Transfer => CCL.Imports.Borrowed_RO_Argument, others => <>);
   Close_Call := Read_Call;
   Close_Call.Transfer := CCL.Imports.Move_Argument;
   Close_Call.Success_Verb := 1;
   Close_Call.Failure_Verb := 1;
   Check (H.Well_Formed (Factory) and H.Well_Formed (Read_Call) and H.Well_Formed (Close_Call));
   Bad := Factory; Bad.Result_Resource := T.Named (""); Check (not H.Well_Formed (Bad));
   Bad := Factory; Bad.Result_Resource.Data (32) := 'X'; Check (not H.Well_Formed (Bad));
   Bad := Factory; Bad.Result_Schema := [others => 1]; Check (not H.Well_Formed (Bad));
   Bad := Factory; Bad.Result := H.Integer_Value; Check (not H.Well_Formed (Bad));
   Bad := Factory; Bad.Result_Text_Limit := 1; Check (not H.Well_Formed (Bad));
   Bad := Read_Call; Bad.Ownership_Argument := False; Check (not H.Well_Formed (Bad));
   Bad := Read_Call; Bad.Transfer := CCL.Imports.Copy_Argument; Check (not H.Well_Formed (Bad));
   Write_Call := (Receiver_Resource => Factory.Result_Resource,
     Ownership_Argument => True, Transfer => CCL.Imports.Borrowed_RW_Argument, others => <>);
   Check (H.Well_Formed (Write_Call) and H.Has_Resources (Write_Call));
   Check (not H.Scalar_Only (Write_Call));
   Bad := Write_Call; Bad.Ownership_Argument := False; Check (not H.Well_Formed (Bad));
   Bad := Write_Call; Bad.Transfer := CCL.Imports.Copy_Argument; Check (not H.Well_Formed (Bad));
   Bad := Write_Call; Bad.Receiver_Resource.Data (32) := 'X'; Check (not H.Well_Formed (Bad));
   Bad := Write_Call; Bad.Argument := H.Resource_Value;
   Bad.Argument_Resource := Factory.Result_Resource; Check (not H.Well_Formed (Bad));
   Check (not H.Scalar_Only (Factory));
   C.Define_Interface ("collections", 1, 0, [others => 1], Descriptor, Error);
   Check (Error = C.Catalog_Valid);
   Define_Call ("open", 0, Factory);
   Define_Call ("get", 1, Read_Call);
   Define_Call ("close", 1, Close_Call);
   Define_Call ("tick", 0, (others => <>));
   Define_Call ("set", 1, Write_Call);
   Bad := Write_Call; Bad.Transfer := CCL.Imports.Borrowed_RO_Argument;
   Define_Call ("read", 0, Bad);
   Bad := Factory; Bad.Result_Resource := T.Named ("OtherCollection");
   Define_Call ("other", 0, Bad);
   C.Publish (Catalog, Descriptor, Error); Check (Error = C.Catalog_Valid);
   C.Publish (Unapproved, Descriptor, Error); Check (Error = C.Catalog_Valid);
   C.Publish_Type (Unapproved, Types, Kind, Ref, Imported); Check (Imported = T.Imported);
   Analyze ("(collections.open)", Kind);
   Analyze ("(collections.get (collections.open))", T.Integer_Type);
   Analyze ("(collections.close (collections.open))", T.Integer_Type);
   Analyze ("(collections.set (collections.open) 42)", T.Integer_Type);
   Analyze ("(collections.read (collections.open))", T.Integer_Type);
   L.Analyze ("(collections.set (collections.other) 42)", Catalog, Analysis);
   Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Type_Check_Failed);
   L.Analyze ("(collections.set (collections.open) true)", Catalog, Analysis);
   Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Type_Check_Failed);
   L.Analyze ("(collections.set 42 42)", Catalog, Analysis);
   Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Type_Check_Failed);
   L.Analyze ("(collections.set (collections.open))", Catalog, Analysis);
   Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Parse_Failed);
   L.Analyze ("(collections.read (collections.open) 42)", Catalog, Analysis);
   Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Parse_Failed);
   L.Analyze ("(collections.get (collections.other))", Catalog, Analysis);
   Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Type_Check_Failed);
   L.Analyze ("(collections.get 7)", Catalog, Analysis);
   Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Type_Check_Failed);
   L.Analyze ("(collections.open)", Unapproved, Analysis);
   Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Type_Check_Failed);

   -- Direct interpretation still needs an owned resource host. No side effect
   -- may escape before its whole-program admission rejects the unsupported call.
   C.Resolve (Catalog, "collections.open", Resolved, Found); Check (Found);
   C.Install (Grants, Resolved, 1, Installed); Check (Installed = C.Grant_Added);
   Bad := Resolved.Import;
   Resolved.Import.Result_Resource := T.Named ("OtherCollection");
   declare
      Binding : Interfaces.Unsigned_32;
   begin
      C.Find_Granted_Binding (Grants, Resolved, Binding, Found);
      Check (not Found);
   end;
   Resolved.Import := Bad;
   Evaluate ("(collections.open)", 100, Catalog, Grants, State, Result);
   Check (Result.Status = L.Host_Contract_Unsupported and State.Calls = 0);
   C.Resolve (Catalog, "collections.tick", Resolved, Found); Check (Found);
   C.Install (Grants, Resolved, 2, Installed); Check (Installed = C.Grant_Added);
   Evaluate ("(let ((earlier (collections.tick))) (collections.set (collections.open) 42))",
     100, Catalog, Grants, State, Result);
   --  The opened collection is never closed: an ownership error, found
   --  before anything runs.
   Check (Result.Status = L.Type_Check_Failed and then
          L."=" (Result.Diagnostic, L.Resource_Ownership_Violation) and then State.Calls = 0);
   Evaluate ("(let ((earlier (collections.tick))) (collections.open))",
     100, Catalog, Grants, State, Result);
   Check (Result.Status = L.Host_Contract_Unsupported and State.Calls = 0);
   Analyze ("(collections.open)", Kind);
   CCL.Compiler.Compile (Analysis, Compiled);
   Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);

   -- Even a legitimate live reference cannot be coerced to persistable data.
   declare
      Owner : R.Registry (991);
      Session : R.Run;
      Ticket : R.Ticket;
      Reference : R.Reference;
      Outcome : R.Outcome;
      Contract : CCL.Objects.Binding;
      Image : CCL.Objects.Image;
      Accepted : Boolean;
      Scalar : CCL.VM.Value;
      Import : CCL.VM.Import_Declaration;
   begin
      R.Start (Owner, Types, Session, Outcome); Check (Outcome = R.Succeeded);
      R.Reserve (Owner, Session, Kind, Ticket, Outcome); Check (Outcome = R.Succeeded);
      R.Publish (Owner, Ticket, True, Reference, Outcome); Check (Outcome = R.Succeeded);
      CCL.Objects.Bind (Types, T.Integer_Type, [others => 99], Contract, Accepted); Check (Accepted);
      CCL.Objects.Values.From_Host (Contract, H.Resource_Constant (Reference), Image, Accepted);
      Check (not Accepted);
      Check (not H.Matches (H.Resource_Constant (Reference), H.Resource_Value, 0));
      H.To_Scalar (H.Resource_Constant (Reference), Scalar, Accepted);
      Check (not Accepted);
      H.To_Bytecode (Factory, Types, T.Integer_Type, Kind, Import, Accepted);
      Check (not Accepted);
      Check (not H.Matches_Bytecode ((others => <>), Factory));
   end;
   Ada.Text_IO.Put_Line ("Typed resource signatures: PASS" & Checks'Image & " checks");
end Signature_Tests;

with CuBit.Messages; use CuBit.Messages;
with CCL.Types;
with CCL.Resources;
with CCL.Ownership;
with CCL.Imports;
with CCL.VM.Resource_Values;
with CCL.Objects.Values;
with CCL.Catalog;
with CCL.Compiler;
with CCL.Language;
with CCL.Host_Values;
with CCL.Resource_Policies;
with Config_Object_Client;
with Config_Object_Messages;

package body Resource_Fixture is
   use Interfaces;
   package T renames CCL.Types;
   package R renames CCL.Resources;
   package V renames CCL.VM;
   package C renames Config_Object_Client;
   package W renames Config_Object_Messages;
   use type T.Definition_Result;
   use type R.Outcome;
   use type R.Reference;
   use type V.Validation_Error;
   use type V.Execution_Status;
   use type V.Value_Kind;
   use type C.Submission;
   use type C.Completion_Result;
   use type W.Status;
   use type CCL.Catalog.Resource_Publication;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Catalog.Link_Result;
   use type CCL.Language.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   type Operation is (Acquire, Read_Value, Close);

   procedure Run (Token : in out Unsigned_64; Good : out Boolean) is
      Types : T.Registry;
      Kind : T.Type_Reference;
      Defined : T.Definition_Result;
      Contract : CCL.Objects.Binding;
      Owner : R.Registry (501);
      Session : R.Run;
      Factory, Call : R.Ticket;
      Ref : R.Reference;
      Result : R.Outcome;
      Client : C.Client;
      Answer : C.Response;
      Sent : C.Submission;
      Done : C.Completion_Result;
      Completion : aliased CompletionEntry := NULL_COMPLETION;
      Activity : Activity_Result;
      Program : V.Program;
      Checked : V.Validated_Program;
      Error : V.Validation_Error;
      State : V.Machine_State;
      Step : V.Execution_Result;
      Value : V.Value;
      Taken : Boolean;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Operation_Description : CCL.Catalog.Operation_Descriptor;
      Resolved : CCL.Catalog.Resolved_Operation;
      Signature : CCL.Host_Values.Import_Declaration;
      Policy : CCL.Resource_Policies.Description :=
        (Mode => CCL.Ownership.Must_Handle, Count => 1, others => <>);
      Published : CCL.Catalog.Resource_Publication;
      Catalog_Error : CCL.Catalog.Catalog_Error;
      Granted : CCL.Catalog.Grant_Result;
      Linked : CCL.Catalog.Link_Result;
      Resource_Type : T.Type_Reference;
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      function Name (Op : Operation) return String is
        (case Op is when Acquire => "open", when Read_Value => "get", when Close => "close");
   begin
      Good := False;
      T.Define (Types, (Identifier => T.Named ("IntegerCollection"), Form => T.Resource,
        Count => 1, Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Kind, Defined);
      if Defined /= T.Defined then return; end if;
      CCL.Objects.Bind (Types, T.Integer_Type, [1, 2, 3, 4], Contract, Good);
      if not Good then return; end if;
      Policy.Dispositions (0) := (Verb => 1, Effect => CCL.Ownership.Consume, others => <>);
      CCL.Catalog.Publish_Resource (Catalog, Types, Kind, Policy, Resource_Type, Published);
      if Published /= CCL.Catalog.Resource_Published then Good := False; return; end if;
      CCL.Catalog.Define_Interface ("config-resource", 1, 0, [others => 51], Descriptor, Catalog_Error);
      if Catalog_Error /= CCL.Catalog.Catalog_Valid then Good := False; return; end if;
      for Op in Operation loop
         Signature := (others => <>);
         if Op = Acquire then
            Signature.Result := CCL.Host_Values.Resource_Value;
            Signature.Result_Resource := T.Named ("IntegerCollection");
         else
            Signature.Argument := CCL.Host_Values.Resource_Value;
            Signature.Argument_Resource := T.Named ("IntegerCollection");
            Signature.Ownership_Argument := True;
            Signature.Transfer := (if Op = Read_Value then CCL.Imports.Borrowed_RO_Argument
                                   else CCL.Imports.Move_Argument);
            if Op = Close then Signature.Success_Verb := 1; Signature.Failure_Verb := 1; end if;
         end if;
         CCL.Catalog.Define_Host_Operation
           (Name (Op), (if Op = Acquire then 0 else 1), Signature, Operation_Description, Catalog_Error);
         if Catalog_Error /= CCL.Catalog.Catalog_Valid then Good := False; return; end if;
         CCL.Catalog.Add_Operation (Descriptor, Operation_Description, Catalog_Error);
         if Catalog_Error /= CCL.Catalog.Catalog_Valid then Good := False; return; end if;
      end loop;
      CCL.Catalog.Publish (Catalog, Descriptor, Catalog_Error);
      if Catalog_Error /= CCL.Catalog.Catalog_Valid then Good := False; return; end if;
      for Op in Operation loop
         CCL.Catalog.Resolve (Catalog, "config-resource." & Name (Op), Resolved, Good);
         if not Good then return; end if;
         CCL.Catalog.Install (Grants, Resolved, Operation'Pos (Op) + 1, Granted);
         if Granted /= CCL.Catalog.Grant_Added then Good := False; return; end if;
      end loop;
      CCL.Language.Analyze
        ("(let ((collection (config-resource.open))) " &
         "(let ((value (config-resource.get collection))) " &
         "(let ((closed (config-resource.close collection))) value)))", Catalog, Analysis);
      Good := CCL.Language.Analysis_Status_Of (Analysis) = CCL.Language.Analysis_Succeeded;
      if not Good then return; end if;
      CCL.Compiler.Compile (Analysis, Compiled);
      Good := Compiled.Status = CCL.Compiler.Compilation_Succeeded;
      if not Good then return; end if;
      Program := Compiled.Program;
      CCL.Catalog.Link_Program (Grants, Compiled.Linkage, Program, Linked, Catalog);
      Good := Linked = CCL.Catalog.Link_Valid;
      if not Good then return; end if;
      V.Verify (Program, Checked, Error); Good := Error = V.Valid;
      if not Good then return; end if;
      R.Start (Owner, Types, Session, Result); Good := Result = R.Succeeded;
      if not Good then return; end if;
      C.Initialize (Client, CAP_SLOT_CONFIG, Good); if not Good then return; end if;
      V.Initialize (Checked, 16, State);
      for Op in Operation loop
         V.Continue_Execution (Checked, State, Step);
         Good := Step.Status = V.Waiting_For_Host and then Step.Requested_Import = Operation'Pos (Op);
         if not Good then return; end if;
         Token := Token + 1;
         if Op = Acquire then
            R.Reserve (Owner, Session, Kind, Factory, Result); Good := Result = R.Succeeded;
            if not Good then return; end if;
            -- Existing collection: no extra durable revisions or schema rows.
            C.Create (Client, "org.cubit.publication", Contract, W.Read_Write, 0, Token, Sent);
         else
            Good := Step.Request_Argument.Kind = V.Resource_Value and then Step.Request_Argument.Resource = Ref;
            if not Good then return; end if;
            R.Begin_Use (Owner, Ref, Kind, Call, Result); Good := Result = R.Succeeded;
            if not Good then return; end if;
            if Op = Read_Value then C.Get (Client, Token, Sent);
            else C.Close (Client, Token, Sent); end if;
         end if;
         Good := Sent = C.Submitted; if not Good then return; end if;
         if Op /= Acquire then V.Acknowledge_Host_Submission (Checked, State, True); end if;
         loop
            if Poll_Completion (Completion'Address) = 1 then
               C.Complete (Client, Completion, Done); Good := Done = C.Completed;
               exit;
            end if;
            Activity := Wait_For_Activity_Until (Unsigned_64'Last);
            if Activity = Unavailable then Good := False; return; end if;
         end loop;
         if not Good then return; end if;
         C.Take_Result (Client, Answer, Taken); Good := Taken and Answer.Valid and Answer.Code = W.Success;
         if not Good then return; end if;
         if Op = Acquire then
            R.Publish (Owner, Factory, True, Ref, Result); Good := Result = R.Succeeded;
            if not Good then return; end if;
            V.Resource_Values.Complete (Checked, State, Owner, Ref, Good);
         else
            R.Finish_Use (Owner, Call, Op = Read_Value, Result); Good := Result = R.Succeeded;
            if not Good then return; end if;
            if Op = Read_Value then
               CCL.Objects.Values.To_VM (Contract, Types, Answer.Value, Value, Good);
               Good := Good and then Value.Kind = V.Integer_Value and then Value.Integer = 42 and then Answer.Revision = 2;
               if not Good then return; end if;
            else Value := V.Integer_Constant (0); end if;
            V.Complete_Host_Call (Checked, State, Value, True);
         end if;
         if not Good then return; end if;
      end loop;
      V.Continue_Execution (Checked, State, Step);
      Good := Step.Status = V.Completed and then Step.Has_Value and then Step.Result_Value.Integer = 42;
      if not Good then return; end if;
      C.Retire (Client, Good); if not Good then return; end if;
      R.Reclaim (Owner, Factory, Result); Good := Result = R.Succeeded and then R.Empty (Owner);
   end Run;
end Resource_Fixture;

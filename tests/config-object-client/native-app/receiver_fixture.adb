with CuBit.Messages; use CuBit.Messages;
with CCL.Types;
with CCL.Resources;
with CCL.Ownership;
with CCL.Imports;
with CCL.Catalog;
with CCL.Compiler;
with CCL.Host_Values;
with CCL.Language;
with CCL.Resource_Policies;
with CCL.Objects.Catalog;
with CCL.VM.Native_Objects;
with Config_Object_Client.Resources;
with Config_Object_Messages;
with Nested_Fixture;

package body Receiver_Fixture is
   use Interfaces;
   package T renames CCL.Types;
   package R renames CCL.Resources;
   package O renames CCL.Objects;
   package V renames CCL.VM;
   package N renames V.Native_Objects;
   package C renames Config_Object_Client;
   package H renames C.Resources;
   package W renames Config_Object_Messages;
   use type T.Definition_Result;
   use type O.Catalog.Publication_Result;
   use type O.Image;
   use type R.Outcome;
   use type R.Reference;
   use type V.Validation_Error;
   use type V.Execution_Status;
   use type C.Submission;
   use type C.Completion_Result;
   use type H.Cleanup_Result;
   use type W.Status;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Resource_Publication;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Catalog.Link_Result;
   use type CCL.Language.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   type Operation is (Acquire, Initial_Read, Supply, Write_Value, Read_Back, Close);
   function Name (Op : Operation) return String is
     (case Op is when Acquire => "create", when Initial_Read => "initial-read",
       when Supply => "supplied", when Write_Value => "set", when Read_Back => "get",
       when Close => "close");

   procedure Run
     (Contract : O.Binding; First, Second : O.Image;
      Token : in out Unsigned_64; Good : out Boolean)
   is
      Catalog : O.Catalog.Schema_Catalog;
      Published : O.Catalog.Publication_Result;
      Types : T.Registry;
      Kind, Data_Type : T.Type_Reference;
      Defined : T.Definition_Result;
      Owner : R.Registry (502);
      Session : R.Run;
      Ref : R.Reference;
      Result : R.Outcome;
      Client : H.Collection;
      Cleanup : H.Cleanup_Result;
      Answer : C.Response;
      Sent : C.Submission;
      Done : C.Completion_Result;
      Completion : aliased CompletionEntry := NULL_COMPLETION;
      Activity : Activity_Result;
      Program : V.Program;
      Checked : V.Validated_Program;
      Error : V.Validation_Error;
      State : N.Machine;
      Step : V.Execution_Result;
      Value : O.Image;
      Taken : Boolean;
      Interfaces : CCL.Catalog.Interface_Catalog;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Declared : CCL.Catalog.Operation_Descriptor;
      Signature : CCL.Host_Values.Import_Declaration;
      Policy : CCL.Resource_Policies.Description :=
        (Mode => CCL.Ownership.Must_Handle, Count => 1, others => <>);
      Resource_Published : CCL.Catalog.Resource_Publication;
      Resource_Type : T.Type_Reference;
      Catalog_Error : CCL.Catalog.Catalog_Error;
      Grants : CCL.Catalog.Granted_Bindings;
      Resolved : CCL.Catalog.Resolved_Operation;
      Granted : CCL.Catalog.Grant_Result;
      Found : Boolean;
      Linked : CCL.Catalog.Link_Result;
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
   begin
      Good := False;
      O.Catalog.Publish (Catalog, Contract, Published);
      if Published /= O.Catalog.Published then return; end if;
      Types := O.Catalog.Visible_Types (Catalog);
      Data_Type := O.Catalog.Root_Of (Catalog, O.Identity (Contract));
      T.Define (Types, (Identifier => T.Named ("PreferencesCollection"), Form => T.Resource,
        Count => 1, Parts => [1 => (T.Named ("Value"), Data_Type), others => <>]), Kind, Defined);
      if Defined /= T.Defined then return; end if;
      CCL.Catalog.Publish_Schema (Interfaces, Contract, Published);
      if Published /= O.Catalog.Published then return; end if;
      Policy.Dispositions (0) := (Verb => 1, Effect => CCL.Ownership.Consume, others => <>);
      CCL.Catalog.Publish_Resource (Interfaces, Types, Kind, Policy, Resource_Type, Resource_Published);
      if Resource_Published /= CCL.Catalog.Resource_Published then return; end if;
      CCL.Catalog.Define_Interface ("config-receiver", 1, 0, [others => 503], Descriptor, Catalog_Error);
      if Catalog_Error /= CCL.Catalog.Catalog_Valid then return; end if;
      for Op in Operation loop
         Signature := (others => <>);
         case Op is
            when Acquire =>
               Signature.Result := CCL.Host_Values.Resource_Value;
               Signature.Result_Resource := T.Named ("PreferencesCollection");
            when Initial_Read | Read_Back | Write_Value =>
               Signature.Receiver_Resource := T.Named ("PreferencesCollection");
               Signature.Ownership_Argument := True;
               if Op = Write_Value then
                  Signature.Transfer := CCL.Imports.Borrowed_RW_Argument;
                  Signature.Argument := CCL.Host_Values.Object_Value;
                  Signature.Argument_Schema := O.Identity (Contract);
               else
                  Signature.Transfer := CCL.Imports.Borrowed_RO_Argument;
                  Signature.Result := CCL.Host_Values.Object_Value;
                  Signature.Result_Schema := O.Identity (Contract);
               end if;
            when Supply =>
               Signature.Result := CCL.Host_Values.Object_Value;
               Signature.Result_Schema := O.Identity (Contract);
            when Close =>
               Signature.Argument := CCL.Host_Values.Resource_Value;
               Signature.Argument_Resource := T.Named ("PreferencesCollection");
               Signature.Ownership_Argument := True;
               Signature.Transfer := CCL.Imports.Move_Argument;
               Signature.Success_Verb := 1;
               Signature.Failure_Verb := 1;
         end case;
         CCL.Catalog.Define_Host_Operation (Name (Op), (if Op in Write_Value | Close then 1 else 0),
           Signature, Declared, Catalog_Error);
         if Catalog_Error /= CCL.Catalog.Catalog_Valid then return; end if;
         CCL.Catalog.Add_Operation (Descriptor, Declared, Catalog_Error);
         if Catalog_Error /= CCL.Catalog.Catalog_Valid then return; end if;
      end loop;
      CCL.Catalog.Publish (Interfaces, Descriptor, Catalog_Error);
      if Catalog_Error /= CCL.Catalog.Catalog_Valid then return; end if;
      for Op in Operation loop
         CCL.Catalog.Resolve (Interfaces, "config-receiver." & Name (Op), Resolved, Found);
         if not Found then return; end if;
         CCL.Catalog.Install (Grants, Resolved, Operation'Pos (Op) + 1, Granted);
         if Granted /= CCL.Catalog.Grant_Added then return; end if;
      end loop;
      CCL.Language.Analyze
        ("(let ((collection (config-receiver.create))) " &
         "(let ((initial (config-receiver.initial-read collection))) " &
         "(let ((written (config-receiver.set collection (config-receiver.supplied)))) " &
         "(let ((value (config-receiver.get collection))) " &
         "(let ((closed (config-receiver.close collection))) value)))))", Interfaces, Analysis);
      if CCL.Language.Analysis_Status_Of (Analysis) /= CCL.Language.Analysis_Succeeded then return; end if;
      CCL.Compiler.Compile (Analysis, Compiled);
      if Compiled.Status /= CCL.Compiler.Compilation_Succeeded then return; end if;
      Program := Compiled.Program;
      CCL.Catalog.Link_Program (Grants, Compiled.Linkage, Program, Linked, Interfaces);
      if Linked /= CCL.Catalog.Link_Valid then return; end if;
      V.Verify (Program, Checked, Error); Good := Error = V.Valid;
      if not Good then return; end if;
      R.Start (Owner, Types, Session, Result); Good := Result = R.Succeeded;
      if not Good then return; end if;
      N.Initialize (Checked, 32, State);
      for Op in Operation loop
         N.Continue_Execution_For (Checked, State, 32, Step);
         Good := Step.Status = V.Waiting_For_Host and then Step.Requested_Binding = Operation'Pos (Op) + 1;
         if not Good then return; end if;
         if Op = Supply then
            N.Complete_Object (Checked, State, Contract, Second, True);
         else
            Token := Token + 1;
            if Op = Acquire then
               H.Create (Client, Owner, Session, Kind, CAP_SLOT_CONFIG,
                 Nested_Fixture.Name, Contract, W.Read_Write, 0, Token, Sent);
            else
               -- The shared resource client owns and checks this pairing.
               Good := (if Op = Close then Step.Request_Argument.Resource = Ref
                        else Step.Request_Receiver = Ref);
               if not Good then return; end if;
               case Op is
                  when Initial_Read | Read_Back => H.Get (Client, Owner, Ref, Token, Sent);
                  when Write_Value =>
                     N.Export_Argument (Checked, State, Contract, Value, Good);
                     Good := Good and then Value = Second;
                     if not Good then return; end if;
                     H.Set (Client, Owner, Ref, Value, 1, Token, Sent);
                  when Close => H.Close (Client, Owner, Ref, Token, Sent);
                  when others => Good := False; return;
               end case;
            end if;
            Good := Sent = C.Submitted; if not Good then return; end if;
            if Op /= Acquire then N.Acknowledge_Host_Submission (Checked, State, True); end if;
            loop
               if Poll_Completion (Completion'Address) = 1 then
                  H.Complete (Client, Owner, Completion, Done); Good := Done = C.Completed; exit;
               end if;
               Activity := Wait_For_Activity_Until (Unsigned_64'Last);
               if Activity = Unavailable then Good := False; return; end if;
            end loop;
            if not Good then return; end if;
            H.Take_Result (Client, Owner, Answer, Taken); Good := Taken and Answer.Valid and Answer.Code = W.Success;
            if not Good then return; end if;
            if Op = Acquire then
               Ref := H.Reference_Of (Client, Owner); Good := Ref /= R.No_Reference;
               if not Good then return; end if;
               N.Complete_Resource (Checked, State, Owner, Ref, Good);
            else
               if Op in Initial_Read | Read_Back then
                  Good := Answer.Value = (if Op = Initial_Read then First else Second) and then
                    Answer.Revision = (if Op = Initial_Read then 1 else 2);
                  if not Good then return; end if;
                  N.Complete_Object (Checked, State, Contract, Answer.Value, True);
               else
                  if Op = Write_Value and then Answer.Revision /= 2 then Good := False; return; end if;
                  N.Complete_Scalar (Checked, State, V.Integer_Constant (0), True);
               end if;
            end if;
            if not Good then return; end if;
         end if;
      end loop;
      N.Continue_Execution_For (Checked, State, 32, Step);
      Good := Step.Status = V.Completed;
      if not Good then return; end if;
      N.Export_Result (Checked, State, Contract, Value, Good);
      Good := Good and then Value = Second;
      if not Good then return; end if;
      Token := Token + 1;
      H.Cleanup (Client, Owner, Token, Cleanup);
      Good := Cleanup = H.Released and then R.Empty (Owner);
   end Run;
end Receiver_Fixture;

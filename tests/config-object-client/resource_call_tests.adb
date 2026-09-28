with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects;
with CCL.Objects.Catalog;
with CCL.Resources;
with CCL.Ownership;
with CCL.Imports;
with CCL.Resource_Policies;
with CCL.Catalog; use CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Compiler;
with CCL.VM.Native_Objects;
with Config_Object_Client.Resources.Calls;
with Config_Read_Outcomes;
with Config_Object_Outcomes;
with Config_Object_Messages;
with CuBit.Messages;
with CuBit.Memory_Grants;

-- Production resource/client/VM code, modeled IPC and grants on Linux.
procedure Resource_Call_Tests is
   package C renames Config_Object_Client;
   package H renames C.Resources;
   package B renames H.Calls;
   package R renames CCL.Resources;
   package O renames CCL.Objects;
   package V renames CCL.VM;
   package N renames V.Native_Objects;
   package W renames Config_Object_Messages;
   package IPC renames CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   use type C.Submission;
   use type C.Completion_Result;
   use type H.Cleanup_Result;
   use type B.Resume_State;
   use type B.Operation;
   use type R.Outcome;
   use type R.Reference;
   use type O.Build_Result;
   use type O.Image;
   use type O.Catalog.Publication_Result;
   use type W.Status;
   use type V.Execution_Status;
   use type V.Validation_Error;
   use type CCL.Language.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   Types : Registry;
   Contract : O.Binding;
   Read_Description, Wrong_Read : Config_Read_Outcomes.Description;
   Catalog : Interface_Catalog;
   Grants : Granted_Bindings;
   Descriptor : Interface_Descriptor;
   Declared : Operation_Descriptor;
   Error : Catalog_Error;
   Published : O.Catalog.Publication_Result;
   Specialized : Resource_Specialization_Result;
   Policy : CCL.Resource_Policies.Description :=
     (Mode => CCL.Ownership.Must_Handle, Count => 1, others => <>);
   Kind : Type_Reference;
   Good : Boolean;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Site; end if;
   end Check;
   function Reply (Token : Unsigned_64; Code : W.Status; Word : Unsigned_64 := 0)
     return IPC.CompletionEntry is
     (requestId => 1, token => Token, msg => W.Reply (Code, Word),
      from => 42, status => IPC.COMPLETION_OK, valid => True);
   procedure Add (Name : String; Parameters : Parameter_Count;
                  Signature : CCL.Host_Values.Import_Declaration) is
   begin
      Define_Host_Operation (Name, Parameters, Signature, Declared, Error);
      Check (Error = Catalog_Valid);
      Add_Operation (Descriptor, Declared, Error); Check (Error = Catalog_Valid);
   end Add;
   procedure Grant (Name : String; Binding : Unsigned_32) is
      Resolved : Resolved_Operation;
      Found : Boolean;
      Result : Grant_Result;
   begin
      Resolve (Catalog, "collection." & Name, Resolved, Found); Check (Found);
      Install (Grants, Resolved, Binding, Result); Check (Result = Grant_Added);
   end Grant;

   procedure Exercise (Action : B.Operation; Code : W.Status;
                       Stop_Before_Resume : Boolean := False;
                       Invalid_Transport : Boolean := False;
                       Wrong_Result : Boolean := False) is
      Owner : R.Registry (501);
      Session : R.Run;
      Outcome : R.Outcome;
      Object, Foreign_Object : H.Collection;
      Call : B.Invocation;
      Machine : N.Machine;
      Program : V.Validated_Program;
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Linked : Link_Result;
      Validity : V.Validation_Error;
      Step : V.Execution_Result;
      Sent : C.Submission;
      Done : C.Completion_Result;
      Resumed : B.Resume_State;
      Cleanup : H.Cleanup_Result;
      Answer : C.Response;
      Ref : R.Reference;
      Taken : Boolean;
      Before : Natural;
      Completion : IPC.CompletionEntry;
      Read_Call : constant String :=
        (if Wrong_Result then "collection.bad-read" else "collection.read");
      Source : constant String :=
        "(let ((collection (collection.create))) " &
        "(let ((result (" & (if Action = B.Read_Value then Read_Call else "collection.write") &
        " collection" & (if Action = B.Write_Value then " 42" else "") & "))) " &
        "(let ((closed (collection.close collection))) result)))";
      Binding : constant Unsigned_32 :=
        (if Wrong_Result then 5 elsif Action = B.Read_Value then 2 else 3);
   begin
      CCL.Language.Analyze (Source, Catalog, Analysis);
      Check (CCL.Language.Analysis_Status_Of (Analysis) = CCL.Language.Analysis_Succeeded);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      Link_Program (Grants, Compiled.Linkage, Compiled.Program, Linked, Catalog);
      if Linked /= Link_Valid then Ada.Text_IO.Put_Line (Linked'Image & " " & Source); end if;
      Check (Linked = Link_Valid);
      V.Verify (Compiled.Program, Program, Validity);
      if Validity /= V.Valid then Ada.Text_IO.Put_Line (Validity'Image & " " & Source); end if;
      Check (Validity = V.Valid);
      N.Initialize (Program, 128, Machine);
      R.Start (Owner, Visible_Types (Catalog), Session, Outcome); Check (Outcome = R.Succeeded);
      N.Continue_Execution_For (Program, Machine, 128, Step);
      Check (Step.Status = V.Waiting_For_Host and Step.Requested_Binding = 1);
      G.Expected_Pages := W.Creation_Bytes / 4096;
      H.Create (Object, Owner, Session, Kind, 5, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent);
      Check (Sent = C.Submitted);
      H.Complete (Object, Owner, Reply (1, W.Success, 55), Done); Check (Done = C.Completed);
      H.Take_Result (Object, Owner, Answer, Taken); Check (Taken and Answer.Valid);
      Ref := H.Reference_Of (Object, Owner); Check (Ref /= R.No_Reference);
      N.Complete_Resource (Program, Machine, Owner, Ref, Good); Check (Good);
      N.Continue_Execution_For (Program, Machine, 128, Step);
      Check (Step.Status = V.Waiting_For_Host and Step.Requested_Binding = Binding);
      Before := IPC.Submissions;
      B.Submit (Call, Foreign_Object, Owner, Action, Binding, Program, Machine, Read_Description, 6, 2, Sent);
      Check (Sent = C.Invalid_Request and IPC.Submissions = Before and not B.Pending (Call));
      B.Submit (Call, Object, Owner, Action, 99, Program, Machine, Read_Description, 6, 2, Sent);
      Check (Sent = C.Invalid_Request and IPC.Submissions = Before);
      if Action = B.Read_Value then
         B.Submit (Call, Object, Owner, Action, Binding, Program, Machine, Wrong_Read, 0, 2, Sent);
         Check (Sent = C.Invalid_Request and IPC.Submissions = Before);
      end if;
      IPC.Accept_Submission := False;
      B.Submit (Call, Object, Owner, Action, Binding, Program, Machine, Read_Description, 6, 2, Sent);
      IPC.Accept_Submission := True;
      Check (Sent /= C.Submitted and not B.Pending (Call) and not N.Ready_For_Completion (Program, Machine));
      B.Submit (Call, Object, Owner, Action, Binding, Program, Machine, Read_Description, 6, 3, Sent);
      if Wrong_Result then
         Check (Sent = C.Invalid_Request and not B.Pending (Call) and IPC.Submissions = Before);
      else
         Check (Sent = C.Submitted and B.Pending (Call) and N.Ready_For_Completion (Program, Machine));
         Before := IPC.Submissions;
         B.Submit (Call, Object, Owner, Action, Binding, Program, Machine, Read_Description, 6, 4, Sent);
         Check (Sent = C.Busy and IPC.Submissions = Before);
         B.Resume (Call, Object, Owner, Program, Machine, Resumed); Check (Resumed = B.No_Completion);
         if Action = B.Read_Value and Code in W.Success | W.Stale then
            declare
               Loan : W.Frame with Import, Address => G.Mapping;
               Built : O.Build_Result;
            begin
               Loan.Value := O.Empty (Contract);
               O.Append (Loan.Value, O.Integer_Cell (42), Built); Check (Built = O.Added);
            end;
         end if;
         Completion := Reply (3, Code, (if Code in W.Success | W.Stale then 7 else 0));
         if Invalid_Transport then Completion.status := IPC.COMPLETION_TARGET_DIED; end if;
         H.Complete (Object, Owner, Completion, Done); Check (Done = C.Completed);
         B.Resume (Call, Foreign_Object, Owner, Program, Machine, Resumed);
         Check (Resumed = B.Other_Call and B.Pending (Call));
         if Stop_Before_Resume then
            N.Stop (Machine);
            R.Stop (Owner, Session, Outcome); Check (Outcome = R.Succeeded);
         end if;
         B.Resume (Call, Object, Owner, Program, Machine, Resumed);
         if Stop_Before_Resume then
            Check (Resumed = B.Other_Call and B.Pending (Call));
            B.Drain (Call, Foreign_Object, Owner, Answer, Taken); Check (not Taken and B.Pending (Call));
            B.Drain (Call, Object, Owner, Answer, Taken); Check (Taken and not B.Pending (Call));
         else
            Check (Resumed = B.Resumed and not B.Pending (Call));
            B.Resume (Call, Object, Owner, Program, Machine, Resumed); Check (Resumed = B.No_Completion);
            N.Continue_Execution_For (Program, Machine, 128, Step);
            Check (Step.Status = V.Waiting_For_Host and Step.Requested_Binding = 4);
            if not Invalid_Transport and Code /= W.Uncertain then
               H.Close (Object, Owner, Ref, 4, Sent); Check (Sent = C.Submitted);
               N.Acknowledge_Host_Submission (Program, Machine, True);
               H.Complete (Object, Owner, Reply (4, W.Success), Done); Check (Done = C.Completed);
               H.Take_Result (Object, Owner, Answer, Taken); Check (Taken);
               N.Complete_Scalar (Program, Machine, V.Integer_Constant (0), True);
               N.Continue_Execution_For (Program, Machine, 128, Step); Check (Step.Status = V.Completed);
               declare
                  Value, Expected : O.Image;
                  Accepted : Boolean;
                  Host_Reply : CCL.Host_Values.Call_Result;
               begin
                  if Action = B.Read_Value then
                     N.Export_Result (Program, Machine, Config_Read_Outcomes.Schema (Read_Description), Value, Accepted);
                     Check (Accepted);
                     -- The alternative tag is observable after the borrow and close.
                     Check (Value.Cells (1).First =
                       (case Code is when W.Success => 1, when W.Stale => 2,
                         when W.Missing => 3, when W.Denied => 4, when others => 6));
                  else
                     N.Export_Result (Program, Machine, Config_Object_Outcomes.Schema, Value, Accepted);
                     Check (Accepted);
                     Config_Object_Outcomes.To_Host (True, Code, (if Code = W.Success then 7 else 0), Host_Reply);
                     Expected := Host_Reply.Value.Object;
                     Check (O.Validate (Value, Config_Object_Outcomes.Schema) and Value = Expected);
                  end if;
               end;
            end if;
         end if;
      end if;
      N.Stop (Machine);
      H.Cleanup (Object, Owner, 5, Cleanup);
      if Cleanup = H.Close_Submitted then
         H.Complete (Object, Owner, Reply (5, W.Success), Done); Check (Done = C.Completed);
         H.Take_Result (Object, Owner, Answer, Taken); Check (Taken);
         H.Cleanup (Object, Owner, 6, Cleanup);
      end if;
      Check (Cleanup = H.Released and R.Empty (Owner));
   end Exercise;
begin
   O.Bind (Types, Integer_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   Config_Read_Outcomes.Define (Contract, Named ("Snapshot"), Named ("ConfigRead"),
     [11, 12, 13, 14], Read_Description, Good); Check (Good);
   Publish_Schema (Catalog, Contract, Published); Check (Published = O.Catalog.Published);
   Publish_Schema (Catalog, Config_Read_Outcomes.Schema (Read_Description), Published);
   Check (Published = O.Catalog.Published);
   Config_Object_Outcomes.Publish (Catalog, Good); Check (Good);
   Policy.Dispositions (0) := (Verb => 1, Effect => CCL.Ownership.Consume, others => <>);
   Specialize_Unary_Resource (Catalog, Types, "ConfigCollection", "Value", Integer_Type,
     Policy, Kind, Specialized); Check (Specialized = Specialization_Ready);
   Define_Interface ("collection", 1, 0, [21, 22, 23, 24], Descriptor, Error); Check (Error = Catalog_Valid);
   Add ("create", 0, (Result => CCL.Host_Values.Resource_Value,
     Result_Resource => Named ("ConfigCollection-Integer"), others => <>));
   Add ("read", 0, (Receiver_Resource => Named ("ConfigCollection-Integer"), Ownership_Argument => True,
     Transfer => CCL.Imports.Borrowed_RO_Argument, Result => CCL.Host_Values.Object_Value,
     Result_Schema => O.Identity (Config_Read_Outcomes.Schema (Read_Description)), others => <>));
   Add ("write", 1, (Receiver_Resource => Named ("ConfigCollection-Integer"), Ownership_Argument => True,
     Transfer => CCL.Imports.Borrowed_RW_Argument, Argument => CCL.Host_Values.Object_Value,
     Argument_Schema => O.Identity (Contract), Result => CCL.Host_Values.Object_Value,
     Result_Schema => Config_Object_Outcomes.Key, others => <>));
   Add ("close", 1, (Argument => CCL.Host_Values.Resource_Value,
     Argument_Resource => Named ("ConfigCollection-Integer"), Ownership_Argument => True,
     Transfer => CCL.Imports.Move_Argument, Success_Verb => 1, Failure_Verb => 1, others => <>));
   Add ("bad-read", 0, (Receiver_Resource => Named ("ConfigCollection-Integer"), Ownership_Argument => True,
     Transfer => CCL.Imports.Borrowed_RO_Argument, others => <>));
   Publish (Catalog, Descriptor, Error); Check (Error = Catalog_Valid);
   Grant ("create", 1); Grant ("read", 2); Grant ("write", 3); Grant ("close", 4); Grant ("bad-read", 5);
   Exercise (B.Read_Value, W.Success);
   Exercise (B.Read_Value, W.Stale);
   Exercise (B.Read_Value, W.Missing);
   Exercise (B.Read_Value, W.Denied);
   Exercise (B.Write_Value, W.Success);
   Exercise (B.Write_Value, W.Conflict);
   Exercise (B.Write_Value, W.Denied);
   Exercise (B.Read_Value, W.Success, Stop_Before_Resume => True);
   Exercise (B.Write_Value, W.Success, Stop_Before_Resume => True);
   Exercise (B.Read_Value, W.Success, Wrong_Result => True);
   Exercise (B.Write_Value, W.Uncertain);
   Exercise (B.Write_Value, W.Success, Invalid_Transport => True);
   Ada.Text_IO.Put_Line ("Resource-bound Config typed calls: PASS" & Checks'Image & " checks");
end Resource_Call_Tests;

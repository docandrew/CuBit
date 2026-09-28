with CCL.Types;
with CCL.Host_Values;
with CCL.Catalog;
with CCL.Objects.Catalog;
with CCL.Objects.Views;
with CCL.Language;
with CCL.VM;
with CCL.VM.Native_Objects;
with CCL.Compiler;
with Config_Object_Client.Host;
with Config_Object_Client.VM.Calls;
with Config_Read_Outcomes;
with Config_Object_Outcomes;
with CuBit.Messages;

package body Read_Fixture is
   type Scenario is (Inspect_Stored, Store_Supplied);
   type Operation is (Read_Value, Supplied_Value, Store_Value);
   for Operation use (Read_Value => 1, Supplied_Value => 2, Store_Value => 3);
   procedure Execute
     (Client : in out Config_Object_Client.Client; Contract : CCL.Objects.Binding;
      Expected : CCL.Objects.Image; Revision : Interfaces.Unsigned_64;
      Token : in out Interfaces.Unsigned_64; Action : Scenario; Good : out Boolean;
      Value_Source : String)
   is
      package C renames Config_Object_Client;
      package H renames Config_Object_Client.Host;
      package R renames Config_Read_Outcomes;
      use CuBit.Messages;
      use type Interfaces.Unsigned_64;
      use type Interfaces.Unsigned_32;
      use type Interfaces.Integer_64;
      use type C.Submission;
      use type C.Completion_Result;
      use type H.Outcome_State;
      use type CCL.Host_Values.Value_Kind;
      use type CCL.Objects.Cell_Array;
      use type CCL.Objects.Image;
      use CCL.Catalog;
      use type CCL.Objects.Catalog.Publication_Result;
      use type CCL.Language.Interpretation_Status;
      use type CCL.VM.Value;
      Definition : R.Description;
      Last_Reply : CCL.Host_Values.Call_Result;
      Catalog : Interface_Catalog;
      Grants : Granted_Bindings;
      Interface_Item : Interface_Descriptor;
      Op : Operation_Descriptor;
      Error : Catalog_Error;
      Published : CCL.Objects.Catalog.Publication_Result;
      Resolved : Resolved_Operation;
      Installed : Grant_Result;
      type Context_Type is record Calls : Natural := 0; end record;
      Context : Context_Type;
      Result : CCL.Language.Interpretation_Result;
      Read_Source : constant String :=
        "(match (config-test.read) ((NestedRead.Found snapshot) (field snapshot revision)) " &
        "((NestedRead.Stale snapshot) -2) ((NestedRead.Missing) 0) ((NestedRead.Denied) -3) " &
        "((NestedRead.Busy) -4) ((NestedRead.Unavailable) -5) ((NestedRead.SchemaMismatch) -6) " &
        "((NestedRead.InvalidRequest) -7) ((NestedRead.InvalidCompletion) -8))";
      Store_Source : constant String :=
        "(match (config-test.store " & Value_Source & ") " &
        "((ConfigWrite.Committed revision) revision) ((ConfigWrite.InvalidRequest) -1) " &
        "((ConfigWrite.Denied) -2) ((ConfigWrite.Busy) -3) ((ConfigWrite.Unavailable) -4) " &
        "((ConfigWrite.Conflict) -5) ((ConfigWrite.Rejected) -6) ((ConfigWrite.Uncertain) -7))";
      procedure Invoke
        (State : in out Context_Type; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
      is
         Sent : C.Submission;
         Done : C.Completion_Result;
         Outcome : H.Outcome_State;
         Completion : aliased CompletionEntry := NULL_COMPLETION;
         Activity : Activity_Result;
         Saved : C.Response;
         Taken : Boolean;
      begin
         Reply := (others => <>);
         if Binding = Operation'Enum_Rep (Supplied_Value) and then Action = Store_Supplied then
            if Argument.Kind /= CCL.Host_Values.Integer_Value or else Argument.Integer /= 0 then return; end if;
            State.Calls := State.Calls + 1;
            Reply := (Value => CCL.Host_Values.Object_Constant (Expected), Success => True);
            return;
         elsif Binding = Operation'Enum_Rep (Read_Value) and then Action = Inspect_Stored then
            if Argument.Kind /= CCL.Host_Values.Integer_Value or else Argument.Integer /= 0 then return; end if;
         elsif Binding = Operation'Enum_Rep (Store_Value) and then Action = Store_Supplied then
            if Argument.Kind /= CCL.Host_Values.Object_Value or else Argument.Object /= Expected or else
              Revision = 0 then return; end if;
         else return;
         end if;
         if Token = Interfaces.Unsigned_64'Last then return; end if;
         State.Calls := State.Calls + 1;
         Token := Token + 1;
         if Action = Inspect_Stored then C.Get (Client, Token, Sent);
         else H.Set_Value (Client, Argument, Revision - 1, Token, Sent);
         end if;
         if Sent /= C.Submitted then return; end if;
         loop
            if Poll_Completion (Completion'Address) = 1 then
               C.Complete (Client, Completion, Done);
               if Done /= C.Completed then return; end if;
               exit;
            end if;
            Activity := Wait_For_Activity_Until (Interfaces.Unsigned_64'Last);
            if Activity = Unavailable then return; end if;
         end loop;
         if Action = Inspect_Stored then
            H.Take_Read_Outcome (Client, Definition, Reply, Outcome);
            if Outcome /= H.Outcome_Ready then Reply.Success := False; end if;
         else
            C.Take_Result (Client, Saved, Taken);
            Config_Object_Outcomes.To_Host (Taken and Saved.Valid, Saved.Code, Saved.Revision, Reply);
         end if;
         Last_Reply := Reply;
      end Invoke;
      procedure Evaluate is new CCL.Language.Interpret_With_Values (Context_Type, Invoke);
      procedure Evaluate_Object is new CCL.Language.Interpret_Object_With_Values (Context_Type, Invoke);
   begin
      Good := False;
      R.Define (Contract, CCL.Types.Named ("NestedSnapshot"), CCL.Types.Named ("NestedRead"),
        [101, 102, 103, 104], Definition, Good);
      if not Good then return; end if;
      Good := False;
      Publish_Schema (Catalog, R.Schema (Definition), Published);
      if Published /= CCL.Objects.Catalog.Published then return; end if;
      Publish_Schema (Catalog, Contract, Published);
      if Published /= CCL.Objects.Catalog.Published then return; end if;
      Config_Object_Outcomes.Publish (Catalog, Good); if not Good then return; end if;
      Good := False;
      Define_Interface ("config-test", 1, 0, [111, 112, 113, 114], Interface_Item, Error);
      if Error /= Catalog_Valid then return; end if;
      Define_Host_Operation ("read", 0,
        (Result => CCL.Host_Values.Object_Value, Result_Schema => CCL.Objects.Identity (R.Schema (Definition)), others => <>), Op, Error);
      if Error /= Catalog_Valid then return; end if;
      Add_Operation (Interface_Item, Op, Error); if Error /= Catalog_Valid then return; end if;
      Define_Host_Operation ("supplied", 0,
        (Result => CCL.Host_Values.Object_Value, Result_Schema => CCL.Objects.Identity (Contract), others => <>), Op, Error);
      if Error /= Catalog_Valid then return; end if;
      Add_Operation (Interface_Item, Op, Error); if Error /= Catalog_Valid then return; end if;
      Define_Host_Operation ("store", 1,
        (Argument => CCL.Host_Values.Object_Value, Argument_Schema => CCL.Objects.Identity (Contract),
         Result => CCL.Host_Values.Object_Value, Result_Schema => Config_Object_Outcomes.Key, others => <>), Op, Error);
      if Error /= Catalog_Valid then return; end if;
      Add_Operation (Interface_Item, Op, Error); if Error /= Catalog_Valid then return; end if;
      Publish (Catalog, Interface_Item, Error); if Error /= Catalog_Valid then return; end if;
      Resolve (Catalog, "config-test.read", Resolved, Good); if not Good then return; end if;
      Good := False;
      Install (Grants, Resolved, Operation'Enum_Rep (Read_Value), Installed); if Installed /= Grant_Added then return; end if;
      if Action = Store_Supplied then
         Resolve (Catalog, "config-test.supplied", Resolved, Good); if not Good then return; end if;
         Good := False;
         Install (Grants, Resolved, Operation'Enum_Rep (Supplied_Value), Installed); if Installed /= Grant_Added then return; end if;
         Resolve (Catalog, "config-test.store", Resolved, Good); if not Good then return; end if;
         Good := False;
         Install (Grants, Resolved, Operation'Enum_Rep (Store_Value), Installed); if Installed /= Grant_Added then return; end if;
      end if;
      Evaluate ((if Action = Inspect_Stored then Read_Source else Store_Source), 128, Catalog, Grants, Context, Result);
      if Result.Status /= CCL.Language.Succeeded or else not Result.Has_Value or else
        Result.Result_Value /= CCL.VM.Integer_Constant (Interfaces.Integer_64 (Revision)) or else
        Context.Calls /= (if Action = Inspect_Stored then 1 else 2)
      then return; end if;
      if Action = Store_Supplied then Good := True; return; end if;
      if not Last_Reply.Success or else
        Last_Reply.Value.Kind /= CCL.Host_Values.Object_Value or else
        not CCL.Objects.Validate (Last_Reply.Value.Object, R.Schema (Definition))
      then return; end if;
      if Revision = 0 then
         Good := Last_Reply.Value.Object.Cells (1).First = R.Alternative'Enum_Rep (R.Missing);
      else
         Good := Expected.Used_Cells <= CCL.Objects.Maximum_Cells - 3 and then
           Last_Reply.Value.Object.Cells (1).First = R.Alternative'Enum_Rep (R.Found) and then
           Last_Reply.Value.Object.Cells (3).First = Revision and then
           Last_Reply.Value.Object.Used_Cells = Expected.Used_Cells + 3 and then
           Last_Reply.Value.Object.Used_Bytes = Expected.Used_Bytes and then
           Last_Reply.Value.Object.Text = Expected.Text and then
           Last_Reply.Value.Object.Cells (4 .. 3 + Natural (Expected.Used_Cells)) =
             Expected.Cells (1 .. Natural (Expected.Used_Cells));
      end if;
      if Good then
         declare
            Prior : constant CCL.Objects.Image := Last_Reply.Value.Object;
            Returned : CCL.Language.Object_Interpretation_Result;
         begin
            Context.Calls := 0;
            Evaluate_Object ("(config-test.read)", 128, Catalog, Grants, Context, R.Schema (Definition), Returned);
            Good := Returned.Status = CCL.Language.Succeeded and Returned.Has_Value and
              Context.Calls = 1 and Returned.Value = Prior and
              CCL.Objects.Validate (Returned.Value, R.Schema (Definition));
         end;
      end if;
      if Good then
         declare
            Text_Contract : CCL.Objects.Binding;
            Text_Types : CCL.Types.Registry;
            Expected_Text : CCL.Objects.Image;
            View : CCL.Objects.Views.Snapshot;
            Built : CCL.Objects.Build_Result;
            Returned : CCL.Language.Object_Interpretation_Result;
            Text_Source : constant String :=
              "(match (config-test.read) " &
              "((NestedRead.Found snapshot) (field (field snapshot value) name)) " &
              "((NestedRead.Stale snapshot) """") ((NestedRead.Missing) """") " &
              "((NestedRead.Denied) """") ((NestedRead.Busy) """") " &
              "((NestedRead.Unavailable) """") ((NestedRead.SchemaMismatch) """") " &
              "((NestedRead.InvalidRequest) """") ((NestedRead.InvalidCompletion) """"))";
            use type CCL.Objects.Build_Result;
         begin
            CCL.Objects.Bind (Text_Types, CCL.Types.String_Type,
              [81, 82, 83, 84], Text_Contract, Good);
            if not Good then return; end if;
            if Revision = 0 then
               Expected_Text := CCL.Objects.Empty (Text_Contract);
               CCL.Objects.Append_Text (Expected_Text, "", Built);
               Good := Built = CCL.Objects.Added;
            else
               CCL.Objects.Views.Capture (View, Contract, Expected, Good);
               if Good then
                  CCL.Objects.Views.Copy_Value (View,
                    CCL.Objects.Views.Field (View, CCL.Objects.Views.Root (View), 1),
                    Text_Contract, Expected_Text, Good);
               end if;
            end if;
            if not Good then return; end if;
            Context.Calls := 0;
            Evaluate_Object (Text_Source, 128, Catalog, Grants, Context, Text_Contract, Returned);
            Good := Returned.Status = CCL.Language.Succeeded and Returned.Has_Value and
              Context.Calls = 1 and Returned.Value = Expected_Text;
         end;
      end if;
      if Good then
         declare
            Analysis : CCL.Language.Analysis_Result;
            Compiled : CCL.Compiler.Compilation_Result;
            Linked : Link_Result;
            Program : CCL.VM.Validated_Program;
            Validation : CCL.VM.Validation_Error;
            Machine : CCL.VM.Native_Objects.Machine;
            Step : CCL.VM.Execution_Result;
            Sent : C.Submission;
            Done : C.Completion_Result;
            Resumed : Config_Object_Client.VM.Calls.Resume_State;
            Completion : aliased CompletionEntry := NULL_COMPLETION;
            Activity : Activity_Result;
            Returned : CCL.Objects.Image;
            Prior : constant CCL.Objects.Image := Last_Reply.Value.Object;
            use type CCL.Language.Analysis_Status;
            use type CCL.Compiler.Compilation_Status;
            use type CCL.VM.Validation_Error;
            use type CCL.VM.Execution_Status;
            use type Config_Object_Client.VM.Calls.Resume_State;
         begin
            for Inspect_Fields in Boolean loop
            CCL.Language.Analyze ((if Inspect_Fields then Read_Source else "(config-test.read)"), Catalog, Analysis);
            Good := CCL.Language.Analysis_Status_Of (Analysis) = CCL.Language.Analysis_Succeeded;
            if not Good then return; end if;
            CCL.Compiler.Compile (Analysis, Compiled);
            Good := Compiled.Status = CCL.Compiler.Compilation_Succeeded;
            if not Good then return; end if;
            Link_Program (Grants, Compiled.Linkage, Compiled.Program, Linked, Catalog);
            Good := Linked = Link_Valid;
            if not Good then return; end if;
            CCL.VM.Verify (Compiled.Program, Program, Validation);
            Good := Validation = CCL.VM.Valid;
            if not Good then return; end if;
            CCL.VM.Native_Objects.Initialize (Program, 128, Machine);
            CCL.VM.Native_Objects.Continue_Execution_For (Program, Machine, 128, Step);
            Good := Step.Status = CCL.VM.Waiting_For_Host and then
              Step.Requested_Binding = Operation'Enum_Rep (Read_Value) and then
              Step.Request_Argument = CCL.VM.Integer_Constant (0);
            if not Good then return; end if;
            if Token = Interfaces.Unsigned_64'Last then Good := False; return; end if;
            Token := Token + 1;
            Config_Object_Client.VM.Calls.Submit
              (Client, Config_Object_Client.VM.Calls.Read_Value, Program, Machine,
               Definition, Operation'Enum_Rep (Read_Value), 0, Token, Sent);
            Good := Sent = C.Submitted;
            if not Good then return; end if;
            loop
               if Poll_Completion (Completion'Address) = 1 then
                  C.Complete (Client, Completion, Done);
                  if Done = C.Completed then
                     Config_Object_Client.VM.Calls.Resume
                       (Client, Program, Machine, Definition, Operation'Enum_Rep (Read_Value), Resumed);
                     Good := Resumed = Config_Object_Client.VM.Calls.Resumed;
                     exit;
                  end if;
               else
                  -- Only this dedicated test event loop parks. The shared
                  -- submit/resume bridge and VM never wait for the service.
                  Activity := Wait_For_Activity_Until (Interfaces.Unsigned_64'Last);
                  if Activity = Unavailable then Good := False; exit; end if;
               end if;
            end loop;
            if not Good then return; end if;
            CCL.VM.Native_Objects.Continue_Execution_For (Program, Machine, 128, Step);
            Good := Step.Status = CCL.VM.Completed and Step.Has_Value;
            if not Good then return; end if;
            if Inspect_Fields then
               Good := Step.Result_Value = CCL.VM.Integer_Constant (Interfaces.Integer_64 (Revision));
            else
               CCL.VM.Native_Objects.Export_Result (Program, Machine, R.Schema (Definition), Returned, Good);
               Good := Good and then Returned = Prior;
            end if;
            CCL.VM.Native_Objects.Stop (Machine);
            if not Good then return; end if;
            end loop;
         end;
      end if;
   end Execute;

   procedure Run
     (Client : in out Config_Object_Client.Client; Contract : CCL.Objects.Binding;
      Expected : CCL.Objects.Image; Revision : Interfaces.Unsigned_64;
      Token : in out Interfaces.Unsigned_64; Good : out Boolean) is
   begin
      Execute (Client, Contract, Expected, Revision, Token, Inspect_Stored, Good, "");
   end Run;

   procedure Store
     (Client : in out Config_Object_Client.Client; Contract : CCL.Objects.Binding;
      Value : CCL.Objects.Image; Expected_New_Revision : Interfaces.Unsigned_64;
      Token : in out Interfaces.Unsigned_64; Good : out Boolean;
      Value_Source : String := "(config-test.supplied)") is
   begin
      Execute (Client, Contract, Value, Expected_New_Revision, Token, Store_Supplied, Good, Value_Source);
   end Store;
end Read_Fixture;

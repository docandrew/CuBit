with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects; use CCL.Objects;
with CCL.Objects.Catalog;
with CCL.Catalog; use CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Compiler;
with CCL.VM;
with CCL.VM.Native_Objects;
with Config_Object_Client.VM.Calls;
with Config_Object_Outcomes;
with Config_Read_Outcomes;
with Config_Object_Messages;
with CuBit.Messages;
with CuBit.Memory_Grants;

procedure Native_Call_Tests is
   package C renames Config_Object_Client;
   package B renames C.VM.Calls;
   package W renames Config_Object_Messages;
   package R renames Config_Read_Outcomes;
   package V renames CCL.VM;
   package N renames V.Native_Objects;
   package IPC renames CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   use type C.Phase;
   use type C.Submission;
   use type C.Completion_Result;
   use type B.Resume_State;
   use type W.Status;
   use type V.Value;
   use type V.Value_Kind;
   use type V.Execution_Status;
   use type V.Validation_Error;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Objects.Catalog.Publication_Result;
   Types : Registry;
   Root : Type_Reference;
   Declared : Definition_Result;
   Contract : Binding;
   Description, Wrong_Description : R.Description;
   Catalog : Interface_Catalog;
   Grants : Granted_Bindings;
   Interface_Item : Interface_Descriptor;
   Operation : Operation_Descriptor;
   Error : Catalog_Error;
   Published : CCL.Objects.Catalog.Publication_Result;
   Resolved : Resolved_Operation;
   Installed : Grant_Result;
   Value, Output, Expected : CCL.Objects.Image;
   Built : Build_Result;
   Object : C.Client;
   Machine : N.Machine;
   Program : V.Validated_Program;
   Step : V.Execution_Result;
   Sent : C.Submission;
   Done : C.Completion_Result;
   Resumed : B.Resume_State;
   Native : C.Response;
   Good, Taken : Boolean;
   Token : Unsigned_64 := 1;
   Count, Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "native Config call check" & Checks'Image; end if;
   end Check;
   function Answer (ID : Unsigned_64; Code : W.Status; Word : Unsigned_64 := 0)
     return IPC.CompletionEntry is
     (requestId => 100, token => ID, msg => W.Reply (Code, Word), from => 42,
      status => IPC.COMPLETION_OK, valid => True);
   procedure Prepare (Source : String) is
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Linked : Link_Result;
      Validity : V.Validation_Error;
   begin
      CCL.Language.Analyze (Source, Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      Link_Program (Grants, Compiled.Linkage, Compiled.Program, Linked, Catalog);
      Check (Linked = Link_Valid);
      V.Verify (Compiled.Program, Program, Validity); Check (Validity = V.Valid);
      N.Initialize (Program, 256, Machine);
      N.Continue_Execution_For (Program, Machine, 256, Step);
      Check (Step.Status = V.Waiting_For_Host and Step.Requested_Binding = 77);
   end Prepare;
   procedure Read_Success is
      Loan : W.Frame with Import, Address => G.Mapping;
   begin
      Token := Token + 1;
      B.Submit (Object, B.Read_Value, Program, Machine, Description, 77, 0, Token, Sent);
      Check (Sent = C.Submitted);
      Loan.Value := Value;
      C.Complete (Object, Answer (Token, W.Success, 7), Done); Check (Done = C.Completed);
      B.Resume (Object, Program, Machine, Description, 77, Resumed); Check (Resumed = B.Resumed);
      N.Continue_Execution_For (Program, Machine, 256, Step);
   end Read_Success;
   function Write_Source return String is
     ("(match (config-test.read) ((ConfigRead.Found snapshot) (config-test.write (field snapshot value))) " &
      "((ConfigRead.Stale snapshot) ConfigWrite.Unavailable) ((ConfigRead.Missing) ConfigWrite.Unavailable) " &
      "((ConfigRead.Denied) ConfigWrite.Unavailable) ((ConfigRead.Busy) ConfigWrite.Unavailable) " &
      "((ConfigRead.Unavailable) ConfigWrite.Unavailable) ((ConfigRead.SchemaMismatch) ConfigWrite.Unavailable) " &
      "((ConfigRead.InvalidRequest) ConfigWrite.Unavailable) ((ConfigRead.InvalidCompletion) ConfigWrite.Unavailable))");
begin
   Define (Types, (Identifier => Named ("Settings"), Form => Product, Count => 2,
     Parts => [1 => (Named ("title"), String_Type), 2 => (Named ("enabled"), Boolean_Type), others => <>]), Root, Declared);
   Check (Declared = Defined);
   Bind (Types, Root, [1, 2, 3, 4], Contract, Good); Check (Good);
   Value := Empty (Contract);
   Append (Value, Product_Cell (2), Built); Check (Built = Added);
   Append_Text (Value, String'(1 .. Maximum_Text_Bytes => 'x'), Built); Check (Built = Added);
   Append (Value, Boolean_Cell (True), Built); Check (Built = Added);
   R.Define (Contract, Named ("Snapshot"), Named ("ConfigRead"), [11, 12, 13, 14], Description, Good); Check (Good);
   Publish_Schema (Catalog, Contract, Published); Check (Published = CCL.Objects.Catalog.Published);
   Publish_Schema (Catalog, R.Schema (Description), Published); Check (Published = CCL.Objects.Catalog.Published);
   Config_Object_Outcomes.Publish (Catalog, Good); Check (Good);
   Define_Interface ("config-test", 1, 0, [21, 22, 23, 24], Interface_Item, Error); Check (Error = Catalog_Valid);
   Define_Host_Operation ("read", 0, (Result => CCL.Host_Values.Object_Value,
     Result_Schema => Identity (R.Schema (Description)), others => <>), Operation, Error);
   Check (Error = Catalog_Valid);
   Add_Operation (Interface_Item, Operation, Error); Check (Error = Catalog_Valid);
   Define_Host_Operation ("write", 1, (Argument => CCL.Host_Values.Object_Value,
     Argument_Schema => Identity (Contract), Result => CCL.Host_Values.Object_Value,
     Result_Schema => Config_Object_Outcomes.Key, others => <>), Operation, Error);
   Check (Error = Catalog_Valid);
   Add_Operation (Interface_Item, Operation, Error); Check (Error = Catalog_Valid);
   Publish (Catalog, Interface_Item, Error); Check (Error = Catalog_Valid);
   Resolve (Catalog, "config-test.read", Resolved, Good); Check (Good);
   Install (Grants, Resolved, 77, Installed); Check (Installed = Grant_Added);
   Resolve (Catalog, "config-test.write", Resolved, Good); Check (Good);
   Install (Grants, Resolved, 78, Installed); Check (Installed = Grant_Added);

   G.Expected_Pages := W.Creation_Bytes / 4096;
   C.Initialize (Object, 5, Good); Check (Good);
   C.Create (Object, "org.cubit.test", Contract, W.Read_Write, 0, Token, Sent); Check (Sent = C.Submitted);
   Prepare ("(config-test.read)");
   B.Submit (Object, B.Read_Value, Program, Machine, Description, 77, 0, Token + 1, Sent);
   Check (Sent = C.Busy); -- no collection use before create completes
   C.Complete (Object, Answer (Token, W.Success, 55), Done); Check (Done = C.Completed);
   B.Resume (Object, Program, Machine, Description, 77, Resumed);
   Check (Resumed = B.Other_Call and C.Status (Object) = C.Result_Ready);
   C.Take_Result (Object, Native, Taken); Check (Taken and Native.Valid);
   for Code in W.Status loop
      if Code not in W.Conflict | W.Rejected | W.Capacity_Exceeded | W.Uncertain then
         declare Loan : W.Frame with Import, Address => G.Mapping; begin
            Prepare ("(config-test.read)");
            Token := Token + 1;
            Count := IPC.Submissions;
            B.Submit (Object, B.Read_Value, Program, Machine, Description, 78, 0, Token, Sent);
            Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
            B.Submit (Object, B.Read_Value, Program, Machine, Wrong_Description, 77, 0, Token, Sent);
            Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
            B.Submit (Object, B.Read_Value, Program, Machine, Description, 77, 0, Token, Sent);
            Check (Sent = C.Submitted);
            B.Submit (Object, B.Read_Value, Program, Machine, Description, 77, 0, Token + 1, Sent);
            Check (Sent = C.Busy and IPC.Submissions = Count + 1);
            B.Resume (Object, Program, Machine, Description, 77, Resumed); Check (Resumed = B.No_Completion);
            C.Complete (Object, Answer (Token + 1, Code), Done); Check (Done = C.Ignored);
            Loan.Value := Value;
            C.Complete (Object, Answer (Token, Code, (if Code in W.Success | W.Stale then 7 else 0)), Done);
            Check (Done = C.Completed);
            B.Resume (Object, Program, Machine, Description, 78, Resumed);
            Check (Resumed = B.Other_Call and C.Status (Object) = C.Result_Ready);
            B.Resume (Object, Program, Machine, Wrong_Description, 77, Resumed);
            Check (Resumed = B.Type_Mismatch and C.Status (Object) = C.Result_Ready);
            B.Resume (Object, Program, Machine, Description, 77, Resumed); Check (Resumed = B.Resumed);
            B.Resume (Object, Program, Machine, Description, 77, Resumed); Check (Resumed = B.No_Completion);
            N.Continue_Execution_For (Program, Machine, 256, Step); Check (Step.Status = V.Completed);
            N.Export_Result (Program, Machine, R.Schema (Description), Output, Good); Check (Good);
            R.Build (Description, True, Code, (if Code in W.Success | W.Stale then 7 else 0), Value, Expected, Good);
            Check (Good and Output = Expected and C.Status (Object) = C.Ready);
         end;
      end if;
   end loop;
   for Trial in 1 .. 3 loop
      declare
         Loan : W.Frame with Import, Address => G.Mapping;
         Code : constant W.Status := (if Trial = 2 then W.Denied else W.Success);
         Revision : constant Unsigned_64 := (if Trial = 1 then 8 elsif Trial = 2 then 0 else 99);
      begin
         Prepare (Write_Source);
         Read_Success;
         Check (Step.Status = V.Waiting_For_Host and Step.Requested_Binding = 78);
         Token := Token + 1;
         B.Submit (Object, B.Write_Value, Program, Machine, Wrong_Description, 78, 7, Token, Sent);
         Check (Sent = C.Submitted and Loan.Value = Value);
         C.Complete (Object, Answer (Token, Code, Revision), Done); Check (Done = C.Completed);
         B.Resume (Object, Program, Machine, Wrong_Description, 78, Resumed); Check (Resumed = B.Resumed);
         N.Continue_Execution_For (Program, Machine, 256, Step); Check (Step.Status = V.Completed);
         Check (Step.Result_Value.Kind = V.Variant_Value and Step.Result_Value.Alternative =
           (if Trial = 1 then Config_Object_Outcomes.Write_Alternative'Enum_Rep (Config_Object_Outcomes.Committed)
            elsif Trial = 2 then Config_Object_Outcomes.Write_Alternative'Enum_Rep (Config_Object_Outcomes.Denied)
            else Config_Object_Outcomes.Write_Alternative'Enum_Rep (Config_Object_Outcomes.Uncertain)));
         Check (C.Status (Object) = (if Trial = 3 then C.Failed else C.Ready));
      end;
   end loop;
   Count := IPC.Submissions;
   B.Submit (Object, B.Write_Value, Program, Machine, Description, 78, 7, Token + 1, Sent);
   Check (Sent = C.Unavailable and IPC.Submissions = Count); -- never retry uncertain write
   C.Retire (Object, Good); Check (Good);
   declare
      Other : C.Client;
   begin
      C.Initialize (Other, 5, Good); Check (Good);
      C.Open (Other, "org.cubit.test", Contract, W.Read_Only, 0, 1, Sent); Check (Sent = C.Submitted);
      C.Complete (Other, Answer (1, W.Success, 56), Done); Check (Done = C.Completed);
      C.Take_Result (Other, Native, Taken); Check (Taken and Native.Valid);
      Prepare ("(config-test.read)");
      B.Submit (Other, B.Read_Value, Program, Machine, Description, 77, 0, 2, Sent); Check (Sent = C.Submitted);
      N.Stop (Machine);
      C.Complete (Other, Answer (2, W.Missing), Done); Check (Done = C.Completed);
      B.Resume (Other, Program, Machine, Description, 77, Resumed);
      Check (Resumed = B.Other_Call and C.Status (Other) = C.Result_Ready);
      C.Take_Result (Other, Native, Taken); Check (Taken and Native.Valid and Native.Code = W.Missing);
      C.Close (Other, 3, Sent); Check (Sent = C.Submitted);
      C.Complete (Other, Answer (3, W.Success), Done); Check (Done = C.Completed);
      C.Take_Result (Other, Native, Taken); Check (Taken and Native.Valid);
      C.Retire (Other, Good); Check (Good);
   end;
   Check (IPC.Waits = 0);
   Ada.Text_IO.Put_Line ("Nonblocking native Config calls: PASS" & Checks'Image & " checks");
end Native_Call_Tests;

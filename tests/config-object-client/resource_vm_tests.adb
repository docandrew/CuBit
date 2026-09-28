with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Resources;
with CCL.Ownership;
with CCL.Imports;
with CCL.VM.Resource_Values;
with CCL.Objects.Values;
with Config_Object_Client;
with Config_Object_Messages;
with CuBit.Messages;
with CuBit.Memory_Grants;

-- Shared production client and VM, modeled kernel IPC/grants on Linux.
-- Raw service handle 55 remains solely inside Config_Object_Client.Client.
procedure Resource_VM_Tests is
   package R renames CCL.Resources;
   package T renames CCL.Types;
   package V renames CCL.VM;
   package C renames Config_Object_Client;
   package W renames Config_Object_Messages;
   package IPC renames CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   use type R.Outcome;
   use type R.Reference;
   use type C.Submission;
   use type C.Completion_Result;
   use type W.Status;
   use type T.Definition_Result;
   use type CCL.Objects.Build_Result;
   use type V.Validation_Error;
   use type V.Execution_Status;
   use type V.Value_Kind;
   Types : T.Registry;
   Kind : T.Type_Reference;
   Defined : T.Definition_Result;
   Contract : CCL.Objects.Binding;
   Data : CCL.Objects.Image;
   Built : CCL.Objects.Build_Result;
   Owner : R.Registry (901);
   Session : R.Run;
   Factory, Call : R.Ticket;
   Ref : R.Reference;
   Result : R.Outcome;
   Client : C.Client;
   Answer : C.Response;
   Sent : C.Submission;
   Completed : C.Completion_Result;
   Program : V.Program;
   Checked : V.Validated_Program;
   Error : V.Validation_Error;
   State : V.Machine_State;
   Step : V.Execution_Result;
   Value : V.Value;
   Good, Taken, Retired : Boolean;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why; end if;
   end Check;
   function Reply (Token : Unsigned_64; Status : W.Status; Value : Unsigned_64 := 0)
     return IPC.CompletionEntry is
     (requestId => 1, token => Token, msg => W.Reply (Status, Value),
      from => 42, status => IPC.COMPLETION_OK, valid => True);
begin
   T.Define (Types, (Identifier => T.Named ("Collection"), Form => T.Resource,
     Count => 1, Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Kind, Defined);
   Check (Defined = T.Defined, "resource type");
   CCL.Objects.Bind (Types, T.Integer_Type, [1, 2, 3, 4], Contract, Good); Check (Good, "approved data schema");
   Data := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Data, CCL.Objects.Integer_Cell (42), Built); Check (Built = CCL.Objects.Added, "native value");
   Program.Data_Types := Types;
   Program.Types_Length := 2;
   Program.Types (1).Mode := CCL.Ownership.Must_Handle;
   Program.Types (1).Dispositions_Length := 1;
   Program.Types (1).Dispositions (0) := (Verb => 1, Effect => CCL.Ownership.Consume, Next_Type => 0);
   Program.Locals_Length := 1; Program.Dynamic_Locals_Length := 1;
   Program.Local_Types (0) := 1; Program.Local_Kinds (0) := V.Resource_Value;
   Program.Local_Data_Types (0) := Kind;
   Program.Imports_Length := 3;
   Program.Imports (0) := (Result => V.Resource_Value, Result_Data_Type => Kind,
     Result_Type_Tag => 1, Binding => 1, others => <>);
   Program.Imports (1) := (Argument => V.Resource_Value, Argument_Data_Type => Kind,
     Ownership_Argument => True, Transfer => CCL.Imports.Borrowed_RO_Argument, Binding => 2, others => <>);
   Program.Imports (2) := (Argument => V.Resource_Value, Argument_Data_Type => Kind,
     Ownership_Argument => True, Transfer => CCL.Imports.Move_Argument,
     Success_Verb => 1, Failure_Verb => 1, Binding => 3, others => <>);
   Program.Length := 7;
   Program.Code (0) := (Op => V.Push_Integer, others => <>);
   Program.Code (1) := (Op => V.Invoke_Import, Import => 0, others => <>);
   Program.Code (2) := (Op => V.Initialize_Local, Local => 0, others => <>);
   Program.Code (3) := (Op => V.Invoke_Import, Import => 1, others => <>);
   Program.Code (4) := (Op => V.Invoke_Import, Import => 2, others => <>);
   Program.Code (5) := (Op => V.Drop, others => <>); -- close result; retain Get value
   -- Return the native Get value, not the close receipt.
   Program.Code (6) := (Op => V.Halt, others => <>);
   V.Verify (Program, Checked, Error); Check (Error = V.Valid, "verify collection factory flow");
   V.Initialize (Checked, 16, State);
   V.Continue_Execution (Checked, State, Step); Check (Step.Status = V.Waiting_For_Host, "factory suspension");
   R.Start (Owner, Types, Session, Result); Check (Result = R.Succeeded, "resource run");
   R.Reserve (Owner, Session, Kind, Factory, Result); Check (Result = R.Succeeded, "reserve before create");
   G.Expected_Pages := W.Creation_Bytes / 4096;
   C.Initialize (Client, 5, Good); Check (Good, "client initialization");
   C.Create (Client, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent);
   Check (Sent = C.Submitted and W.Valid_Request (IPC.Last_Request, W.Create_Collection), "async typed create");
   C.Complete (Client, Reply (1, W.Success, 55), Completed); Check (Completed = C.Completed, "authenticated acquisition");
   C.Take_Result (Client, Answer, Taken); Check (Taken and Answer.Valid and Answer.Code = W.Success, "take acquisition");
   R.Publish (Owner, Factory, True, Ref, Result); Check (Result = R.Succeeded, "publish opaque resource");
   V.Resource_Values.Complete (Checked, State, Owner, Ref, Good); Check (Good, "VM factory return");
   V.Continue_Execution (Checked, State, Step);
   Check (Step.Status = V.Waiting_For_Host and Step.Requested_Import = 1 and
     Step.Request_Argument.Kind = V.Resource_Value and Step.Request_Argument.Resource = Ref,
     "VM borrows reference, never service handle 55");
   R.Begin_Use (Owner, Ref, Kind, Call, Result); Check (Result = R.Succeeded, "borrow host resource");
   C.Get (Client, 2, Sent); Check (Sent = C.Submitted, "async native get");
   V.Acknowledge_Host_Submission (Checked, State, True);
   declare
      Loan : W.Frame with Import, Address => G.Mapping;
   begin
      Loan.Value := Data;
   end;
   C.Complete (Client, Reply (2, W.Success, 1), Completed); Check (Completed = C.Completed, "get receipt");
   C.Take_Result (Client, Answer, Taken); Check (Taken and Answer.Valid and Answer.Code = W.Success, "native get value");
   R.Finish_Use (Owner, Call, True, Result); Check (Result = R.Succeeded, "return borrow");
   CCL.Objects.Values.To_VM (Contract, Types, Answer.Value, Value, Good);
   Check (Good and Value.Kind = V.Integer_Value and Value.Integer = 42, "native value to typed VM scalar");
   V.Complete_Host_Call (Checked, State, Value, True);
   V.Continue_Execution (Checked, State, Step);
   Check (Step.Status = V.Waiting_For_Host and Step.Requested_Import = 2 and Step.Request_Argument.Resource = Ref,
     "second owned call consumes reference");
   R.Begin_Use (Owner, Ref, Kind, Call, Result); Check (Result = R.Succeeded, "start close");
   C.Close (Client, 3, Sent); Check (Sent = C.Submitted and IPC.Last_Request.words (0) = 55, "client uses private service handle");
   V.Acknowledge_Host_Submission (Checked, State, True);
   C.Complete (Client, Reply (3, W.Success), Completed); Check (Completed = C.Completed, "close receipt");
   C.Take_Result (Client, Answer, Taken); Check (Taken and Answer.Valid and Answer.Code = W.Success, "closed");
   R.Finish_Use (Owner, Call, False, Result); Check (Result = R.Succeeded, "retire host reference");
   V.Complete_Host_Call (Checked, State, V.Integer_Constant (0), True);
   V.Continue_Execution (Checked, State, Step);
   Check (Step.Status = V.Completed and Step.Has_Value and Step.Result_Value.Integer = 42, "return original Config value");
   G.Is_Retired := True;
   C.Retire (Client, Retired); Check (Retired, "grant retired");
   R.Reclaim (Owner, Factory, Result); Check (Result = R.Succeeded and R.Empty (Owner), "reclaim drained backing");
   Ada.Text_IO.Put_Line ("Async Config opaque VM factory/read/close: PASS" & Checks'Image & " checks");
end Resource_VM_Tests;

with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects; use CCL.Objects;
with CCL.VM;
with CCL.Catalog;
with Config_Object_Outcomes;
with Config_Object_Client.VM.Calls;
with Config_Object_Messages;
with CuBit.Messages;
with CuBit.Memory_Grants;

procedure Call_Tests is
   package C renames Config_Object_Client;
   package B renames Config_Object_Client.VM.Calls;
   package W renames Config_Object_Messages;
   package V renames CCL.VM;
   package IPC renames CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   use type C.Phase;
   use type C.Submission;
   use type C.Completion_Result;
   use type B.Outcome_State;
   use type B.Operation;
   use type W.Status;
   use type V.Value;
   Types : Registry;
   Catalog : CCL.Catalog.Interface_Catalog;
   Contract : Binding;
   Object : C.Client;
   Request : V.Execution_Result;
   Reply : B.Outcome;
   Native : C.Response;
   Value : CCL.Objects.Image;
   Built : Build_Result;
   Sent : C.Submission;
   Done : C.Completion_Result;
   Good, Taken : Boolean;
   Token : Unsigned_64 := 1;
   Count, Checks : Natural := 0;
   function Answer (ID : Unsigned_64; Code : W.Status; Word : Unsigned_64 := 0)
     return IPC.CompletionEntry is
     (requestId => 100, token => ID, msg => W.Reply (Code, Word),
      from => 42, status => IPC.COMPLETION_OK, valid => True);
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "Config VM call check" & Checks'Image; end if;
   end Check;
begin
   G.Expected_Pages := W.Creation_Bytes / 4096;
   Config_Object_Outcomes.Publish (Catalog, Good); Check (Good);
   Types := CCL.Catalog.Visible_Types (Catalog);
   Bind (Types, Integer_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   Value := Empty (Contract); Append (Value, Integer_Cell (42), Built); Check (Built = Added);
   B.Take_Outcome (Object, Types, Reply); Check (Reply.State = B.No_Outcome);
   C.Initialize (Object, 5, Good); Check (Good);
   C.Create (Object, "org.cubit.test", Contract, W.Read_Write, 0, Token, Sent);
   Check (Sent = C.Submitted);
   C.Complete (Object, Answer (Token, W.Success, 55), Done); Check (Done = C.Completed);
   B.Take_Outcome (Object, Types, Reply);
   Check (Reply.State = B.Other_Operation and C.Status (Object) = C.Result_Ready);
   C.Take_Result (Object, Native, Taken); Check (Taken and Native.Valid);
   Token := Token + 1;
   Count := IPC.Submissions;
   B.Submit (Object, B.Read_Value, Types, Request, 0, Token, Sent);
   Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
   Request.Status := V.Waiting_For_Host;
   B.Submit (Object, B.Read_Value, Types, Request, 0, Token, Sent);
   Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
   Request.Requested_Binding := 77;
   Request.Request_Argument := V.Integer_Constant (1);
   B.Submit (Object, B.Read_Value, Types, Request, 0, Token, Sent);
   Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
   Request.Request_Argument := V.Integer_Constant (0);
   Request.Request_Owned := True;
   B.Submit (Object, B.Read_Value, Types, Request, 0, Token, Sent);
   Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
   Request.Request_Argument := V.Integer_Constant (42);
   B.Submit (Object, B.Write_Value, Types, Request, 0, Token, Sent);
   Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
   Request.Request_Owned := False;
   Request.Request_Argument := V.Integer_Constant (0);
   for Bad in 1 .. 3 loop
      case Bad is
         when 1 => Request.Request_Argument := V.Boolean_Constant (False);
         when 2 => Request.Request_Argument := V.Integer_Constant (0); Request.Request_Argument.Type_Tag := 1;
         when others => Request.Request_Argument := V.Integer_Constant (0); Request.Request_Argument.Copyable := False;
      end case;
      B.Submit (Object, B.Read_Value, Types, Request, 0, Token, Sent);
      Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
   end loop;
   Request.Request_Argument := V.Integer_Constant (0);
   IPC.Accept_Submission := False;
   B.Submit (Object, B.Read_Value, Types, Request, 0, Token, Sent);
   Check (Sent = C.Not_Submitted and C.Status (Object) = C.Ready);
   IPC.Accept_Submission := True;
   Count := IPC.Submissions;
   B.Submit (Object, B.Read_Value, Types, Request, 0, Token, Sent);
   Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
   Token := Token + 1;
   declare
      Loan : W.Frame with Import, Address => G.Mapping;
   begin
      for Code in W.Status loop
         if Code not in W.Conflict | W.Rejected | W.Capacity_Exceeded | W.Uncertain then
         B.Submit (Object, B.Read_Value, Types, Request, 0, Token, Sent);
         Check (Sent = C.Submitted);
         Count := IPC.Submissions;
         B.Submit (Object, B.Read_Value, Types, Request, 0, Token + 1, Sent);
         Check (Sent = C.Busy and IPC.Submissions = Count);
         B.Take_Outcome (Object, Types, Reply); Check (Reply.State = B.No_Outcome);
         C.Complete (Object, Answer (Token + 1, Code), Done);
         Check (Done = C.Ignored and C.Status (Object) = C.Waiting);
         Loan.Value := Value;
         C.Complete (Object, Answer (Token, Code, (if Code in W.Success | W.Stale then 7 else 0)), Done);
         Check (Done = C.Completed);
         B.Take_Outcome (Object, Types, Reply);
         Check (Reply.State = B.Service_Outcome and Reply.Code = Code and Reply.Action = B.Read_Value);
         Check (Reply.Has_Value = (Code in W.Success | W.Stale));
         Check (B.Can_Resume (Reply) = (Code = W.Success));
         Check (B.Can_Resume (Reply, B.Accept_Stale) = (Code in W.Success | W.Stale));
         if Reply.Has_Value then
            Check (Reply.Value = V.Integer_Constant (42) and Reply.Revision = 7);
         else Check (Reply.Revision = 0); end if;
         B.Take_Outcome (Object, Types, Reply); Check (Reply.State = B.No_Outcome);
         Token := Token + 1;
         end if;
      end loop;
      Request.Request_Argument := V.Integer_Constant (42);
      for Code in W.Status loop
         if Code not in W.Stale | W.Missing | W.Schema_Mismatch | W.Capacity_Exceeded | W.Uncertain then
            B.Submit (Object, B.Write_Value, Types, Request, 7, Token, Sent);
            Check (Sent = C.Submitted and Loan.Value = Value);
            Count := IPC.Submissions;
            B.Submit (Object, B.Write_Value, Types, Request, 7, Token + 1, Sent);
            Check (Sent = C.Busy and IPC.Submissions = Count);
            C.Complete (Object, Answer (Token, Code, (if Code = W.Success then 8 else 0)), Done);
            Check (Done = C.Completed);
            declare Empty_Types : Registry; begin
               B.Take_Outcome (Object, Empty_Types, Reply);
               Check (Reply.State = B.Type_Mismatch and C.Status (Object) = C.Result_Ready and not Reply.Has_Value);
            end;
            B.Take_Outcome (Object, Types, Reply);
            Check (Reply.State = B.Service_Outcome and Reply.Code = Code and Reply.Action = B.Write_Value);
            Check (Reply.Has_Value and B.Can_Resume (Reply));
            declare Expected : V.Value; Converted : Boolean; begin
               Config_Object_Outcomes.To_VM (Types, True, Code, (if Code = W.Success then 8 else 0), Expected, Converted);
               Check (Converted and Reply.Value = Expected and Reply.Revision = (if Code = W.Success then 8 else 0));
            end;
            Token := Token + 1;
         end if;
      end loop;
      -- An authenticated but invalid write receipt is uncertain, not rejection.
      B.Submit (Object, B.Write_Value, Types, Request, 7, Token, Sent); Check (Sent = C.Submitted);
      C.Complete (Object, Answer (Token, W.Success, 99), Done); Check (Done = C.Completed);
      B.Take_Outcome (Object, Types, Reply);
      Check (Reply.State = B.Uncertain and B.Can_Resume (Reply) and Reply.Has_Value);
      Check (Reply.Value.Alternative = Config_Object_Outcomes.Write_Alternative'Enum_Rep (Config_Object_Outcomes.Uncertain));
      Check (C.Status (Object) = C.Failed);
      Count := IPC.Submissions;
      B.Submit (Object, B.Write_Value, Types, Request, 7, Token + 1, Sent);
      Check (Sent = C.Unavailable and IPC.Submissions = Count);
   end;
   C.Retire (Object, Good); Check (Good);
   -- An unsupported VM shape must not consume the owned native object. The
   -- host can still take it through the general native client API.
   declare
      Text_Client : C.Client;
      Text_Contract : Binding;
      Text_Value : CCL.Objects.Image;
   begin
      Bind (Types, String_Type, [5, 6, 7, 8], Text_Contract, Good); Check (Good);
      Text_Value := Empty (Text_Contract);
      Append_Text (Text_Value, "native text", Built); Check (Built = Added);
      C.Initialize (Text_Client, 5, Good); Check (Good);
      C.Open (Text_Client, "org.cubit.text", Text_Contract, W.Read_Only, 0, 1, Sent);
      Check (Sent = C.Submitted);
      C.Complete (Text_Client, Answer (1, W.Success, 55), Done); Check (Done = C.Completed);
      C.Take_Result (Text_Client, Native, Taken); Check (Taken and Native.Valid);
      Request.Request_Argument := V.Integer_Constant (0);
      B.Submit (Text_Client, B.Read_Value, Types, Request, 0, 2, Sent); Check (Sent = C.Submitted);
      declare Loan : W.Frame with Import, Address => G.Mapping; begin Loan.Value := Text_Value; end;
      C.Complete (Text_Client, Answer (2, W.Success, 1), Done); Check (Done = C.Completed);
      B.Take_Outcome (Text_Client, Types, Reply);
      Check (Reply.State = B.Type_Mismatch and C.Status (Text_Client) = C.Result_Ready and not B.Can_Resume (Reply));
      C.Take_Result (Text_Client, Native, Taken);
      Check (Taken and Native.Valid and Native.Value = Text_Value);
      B.Submit (Text_Client, B.Read_Value, Types, Request, 0, 3, Sent); Check (Sent = C.Submitted);
      -- Conflict is not a legal Get response. Treat the invalid receipt as
      -- uncertain transport, rather than claiming a service-level conflict.
      C.Complete (Text_Client, Answer (3, W.Conflict), Done); Check (Done = C.Completed);
      B.Take_Outcome (Text_Client, Types, Reply);
      Check (Reply.State = B.Uncertain and C.Status (Text_Client) = C.Failed and not Reply.Has_Value);
      C.Retire (Text_Client, Good); Check (Good);
   end;
   declare
      Pending_Client : C.Client;
      Empty_Types : Registry;
   begin
      C.Initialize (Pending_Client, 5, Good); Check (Good);
      C.Open (Pending_Client, "org.cubit.test", Contract, W.Read_Write, 0, 1, Sent); Check (Sent = C.Submitted);
      C.Complete (Pending_Client, Answer (1, W.Success, 55), Done); Check (Done = C.Completed);
      C.Take_Result (Pending_Client, Native, Taken); Check (Taken and Native.Valid);
      Request.Request_Argument := V.Integer_Constant (42);
      B.Submit (Pending_Client, B.Write_Value, Types, Request, 0, 2, Sent); Check (Sent = C.Submitted);
      C.Complete (Pending_Client, Answer (2, W.Uncertain), Done); Check (Done = C.Completed);
      B.Take_Outcome (Pending_Client, Empty_Types, Reply);
      Check (Reply.State = B.Type_Mismatch and C.Status (Pending_Client) = C.Result_Ready);
      B.Take_Outcome (Pending_Client, Types, Reply);
      Check (Reply.State = B.Uncertain and Reply.Code = W.Uncertain and Reply.Has_Value and B.Can_Resume (Reply));
      Check (Reply.Value.Alternative = Config_Object_Outcomes.Write_Alternative'Enum_Rep (Config_Object_Outcomes.Uncertain));
      Check (C.Status (Pending_Client) = C.Failed);
      Count := IPC.Submissions;
      B.Submit (Pending_Client, B.Write_Value, Types, Request, 0, 3, Sent);
      Check (Sent = C.Unavailable and IPC.Submissions = Count);
      C.Complete (Pending_Client, Answer (2, W.Success, 1), Done);
      Check (Done = C.Ignored and C.Status (Pending_Client) = C.Failed);
      C.Retire (Pending_Client, Good); Check (Good);
   end;
   Check (IPC.Waits = 0);
   Ada.Text_IO.Put_Line ("Nonblocking Config VM calls: PASS" & Checks'Image & " checks");
end Call_Tests;

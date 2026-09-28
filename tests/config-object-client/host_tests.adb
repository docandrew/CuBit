with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Host_Values;
with CCL.Objects; use CCL.Objects;
with CCL.Types; use CCL.Types;
with Config_Object_Client.Host;
with Config_Object_Messages;
with CuBit.Messages;
with CuBit.Memory_Grants;

procedure Host_Tests is
   package C renames Config_Object_Client;
   package H renames Config_Object_Client.Host;
   package W renames Config_Object_Messages;
   package IPC renames CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   use type C.Phase;
   use type C.Submission;
   use type C.Completion_Result;
   use type H.Read_State;
   use type W.Status;
   use type CCL.Host_Values.Value_Kind;
   Types : Registry;
   Contract : Binding;
   Object : C.Client;
   Value : CCL.Objects.Image;
   Built : Build_Result;
   Item : H.Read_Result;
   Native : C.Response;
   Sent : C.Submission;
   Done : C.Completion_Result;
   Good, Taken : Boolean;
   Count : Natural;
   Token : Unsigned_64 := 3;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "host adapter check" & Checks'Image; end if;
   end Check;
   function Answer (Token : Unsigned_64; Code : W.Status; Word : Unsigned_64 := 0)
     return IPC.CompletionEntry is
     (requestId => 100, token => Token, msg => W.Reply (Code, Word),
      from => 42, status => IPC.COMPLETION_OK, valid => True);
begin
   G.Expected_Pages := W.Creation_Bytes / 4096;
   Bind (Types, String_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   Value := Empty (Contract);
   Append_Text (Value, String'(1 .. Maximum_Text_Bytes => 'x'), Built); Check (Built = Added);
   H.Take_Get_Result (Object, Item); Check (Item.State = H.No_Result);
   H.Set_Value (Object, CCL.Host_Values.Object_Constant (Value), 0, 1, Sent);
   Check (Sent = C.Unavailable);
   C.Initialize (Object, 5, Good); Check (Good);
   C.Create (Object, "org.cubit.test", Contract, W.Read_Write, 0, 1, Sent);
   Check (Sent = C.Submitted);
   H.Set_Value (Object, CCL.Host_Values.Object_Constant (Value), 0, 2, Sent);
   Check (Sent = C.Busy);
   C.Complete (Object, Answer (1, W.Success, 55), Done); Check (Done = C.Completed);
   H.Take_Get_Result (Object, Item);
   Check (Item.State = H.Other_Operation and C.Status (Object) = C.Result_Ready);
   C.Take_Result (Object, Native, Taken); Check (Taken and Native.Valid);
   declare
      Loan : W.Frame with Import, Address => G.Mapping;
      Bad : CCL.Objects.Image := Value;
   begin
      Count := IPC.Submissions;
      Bad.Schema := [9, 9, 9, 9];
      H.Set_Value (Object, CCL.Host_Values.Object_Constant (Bad), 0, 2, Sent);
      Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
      Bad := Value; Bad.Padding (1) := 1;
      H.Set_Value (Object, CCL.Host_Values.Object_Constant (Bad), 0, 2, Sent);
      Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
      H.Set_Value (Object, CCL.Host_Values.Integer_Constant (42), 0, 2, Sent);
      Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
      H.Set_Value (Object, CCL.Host_Values.Object_Constant (Value), 0, 2, Sent);
      Check (Sent = C.Submitted and Loan.Value = Value);
      C.Complete (Object, Answer (2, W.Success, 1), Done); Check (Done = C.Completed);
      H.Take_Get_Result (Object, Item); Check (Item.State = H.Other_Operation);
      C.Take_Result (Object, Native, Taken); Check (Taken and Native.Valid and Native.Revision = 1);
      for Scenario in 1 .. 4 loop
         declare
            Code : constant W.Status := (case Scenario is
              when 1 => W.Success, when 2 => W.Stale, when 3 => W.Denied, when others => W.Missing);
         begin
            C.Get (Object, Token, Sent); Check (Sent = C.Submitted);
            Loan.Value := Value;
            C.Complete (Object, Answer (Token, Code, (if Scenario <= 2 then 1 else 0)), Done);
            Check (Done = C.Completed);
            H.Take_Get_Result (Object, Item); Check (Item.Code = Code);
            if Scenario <= 2 then
               Check (Item.State = H.Value_Ready and Item.Revision = 1);
               Check (Item.Value.Kind = CCL.Host_Values.Object_Value and then Item.Value.Object = Value);
            else
               Check (Item.State = H.No_Value and Item.Value.Kind /= CCL.Host_Values.Object_Value);
            end if;
            Check (C.Status (Object) = C.Ready);
            Token := Token + 1;
         end;
      end loop;
      C.Get (Object, Token, Sent); Check (Sent = C.Submitted);
      Loan.Value := Value; Loan.Value.Padding (1) := 1;
      C.Complete (Object, Answer (Token, W.Success, 1), Done); Check (Done = C.Completed);
      H.Take_Get_Result (Object, Item);
      Check (Item.State = H.Invalid_Completion and C.Status (Object) = C.Failed);
      Count := IPC.Submissions;
      H.Set_Value (Object, CCL.Host_Values.Object_Constant (Value), 1, Token + 1, Sent);
      Check (Sent = C.Unavailable and IPC.Submissions = Count);
   end;
   C.Retire (Object, Good); Check (Good);
   Check (IPC.Waits = 0);
   Ada.Text_IO.Put_Line ("Nonblocking Config host-object adapter: PASS" & Checks'Image & " checks");
end Host_Tests;

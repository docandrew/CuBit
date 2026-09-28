with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Objects.Values;
with CCL.Types;
with CCL.VM;
with Config_Object_Client.VM;
with Config_Object_Messages;
with CuBit.Messages;
with CuBit.Memory_Grants;
with VM_Fixture;

procedure VM_Tests is
   package Client renames Config_Object_Client;
   package Adapter renames Config_Object_Client.VM;
   package W renames Config_Object_Messages;
   package IPC renames CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   use type Client.Phase;
   use type Client.Submission;
   use type Client.Completion_Result;
   use type Adapter.Read_State;
   use type W.Status;
   use type CCL.Objects.Image;
   use type CCL.VM.Value;
   use type CCL.Types.Type_Reference;
   use type CCL.Objects.Build_Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "Config VM adapter check" & Checks'Image; end if;
   end Check;
   function Answer (Token : Unsigned_64; Code : W.Status; Word : Unsigned_64 := 0)
     return IPC.CompletionEntry is
     (requestId => 100, token => Token, msg => W.Reply (Code, Word),
      from => 42, status => IPC.COMPLETION_OK, valid => True);

   procedure Run (Source, Shifted_Source : String) is
      Object : Client.Client;
      Types, Shifted : CCL.Types.Registry;
      Value, Shifted_Value, Wrong : CCL.VM.Value;
      Contract : CCL.Objects.Binding;
      Image : CCL.Objects.Image;
      Native : Client.Response;
      Item : Adapter.Read_Result;
      Sent : Client.Submission;
      Done : Client.Completion_Result;
      Good, Taken : Boolean;
      Count : Natural;
      Token : Unsigned_64 := 2;
   begin
      VM_Fixture.Run (Source, Types, Value, Good); Check (Good);
      VM_Fixture.Run (Shifted_Source, Shifted, Shifted_Value, Good); Check (Good);
      CCL.Objects.Bind
        (Types, (case Value.Kind is when CCL.VM.Integer_Value => CCL.Types.Integer_Type,
                 when CCL.VM.Boolean_Value => CCL.Types.Boolean_Type,
                 when CCL.VM.Variant_Value | CCL.VM.Object_Value => Value.Data_Type,
                 when CCL.VM.Resource_Value => CCL.Types.Invalid_Type),
         [1, 2, 3, 4], Contract, Good); Check (Good);
      CCL.Objects.Values.From_VM (Contract, Types, Value, Image, Good); Check (Good);
      Adapter.Take_Get_Result (Object, Types, Item); Check (Item.State = Adapter.No_Result);
      Adapter.Set_Value (Object, Types, Value, 0, 1, Sent); Check (Sent = Client.Unavailable);
      Client.Initialize (Object, 5, Good); Check (Good);
      Client.Create (Object, "org.cubit.test", Contract, W.Read_Write, 0, 1, Sent);
      Check (Sent = Client.Submitted);
      Adapter.Set_Value (Object, Types, Value, 0, 2, Sent); Check (Sent = Client.Busy);
      Client.Complete (Object, Answer (1, W.Success, 55), Done); Check (Done = Client.Completed);
      Adapter.Take_Get_Result (Object, Types, Item);
      Check (Item.State = Adapter.Other_Operation and Client.Status (Object) = Client.Result_Ready);
      Client.Take_Result (Object, Native, Taken); Check (Taken and Native.Valid);
      declare
         Loan : W.Frame with Import, Address => G.Mapping;
      begin
         Count := IPC.Submissions;
         for Bad in 1 .. 4 loop
            Wrong := Value;
            case Bad is
               when 1 => Wrong.Copyable := False;
               when 2 => Wrong.Type_Tag := 1;
               when 3 => Wrong.Data_Type :=
                 (if Value.Data_Type = CCL.Types.Invalid_Type then CCL.Types.Integer_Type
                  else CCL.Types.Invalid_Type);
               when others => Wrong := CCL.VM.Integer_Constant (0);
                  Wrong.Data_Type := CCL.Types.Boolean_Type;
            end case;
            Adapter.Set_Value (Object, Types, Wrong, 0, 2, Sent);
            Check (Sent = Client.Invalid_Request and IPC.Submissions = Count);
            Check (Client.Status (Object) = Client.Ready);
         end loop;
         Adapter.Set_Value (Object, Shifted, Shifted_Value, 0, 2, Sent);
         Check (Sent = Client.Submitted and Loan.Value = Image);
         Client.Complete (Object, Answer (2, W.Success, 1), Done); Check (Done = Client.Completed);
         Adapter.Take_Get_Result (Object, Types, Item); Check (Item.State = Adapter.Other_Operation);
         Client.Take_Result (Object, Native, Taken); Check (Taken and Native.Valid and Native.Revision = 1);
         for Scenario in 1 .. 4 loop
            declare
               Code : constant W.Status :=
                 (case Scenario is when 1 => W.Success, when 2 => W.Stale,
                  when 3 => W.Missing, when others => W.Denied);
               Has_Value : constant Boolean := Scenario <= 2;
               Empty_Types : CCL.Types.Registry;
            begin
               Token := Token + 1;
               Client.Get (Object, Token, Sent); Check (Sent = Client.Submitted);
               Adapter.Take_Get_Result (Object, Types, Item); Check (Item.State = Adapter.No_Result);
               Loan.Value := Image;
               Client.Complete (Object, Answer (Token, Code, (if Has_Value then 1 else 0)), Done);
               Check (Done = Client.Completed);
               Loan.Value := (others => <>); -- conversion must use the owned snapshot
               if Has_Value and Value.Data_Type > CCL.Types.String_Type then
                  Adapter.Take_Get_Result (Object, Empty_Types, Item);
                  Check (Item.State = Adapter.Type_Mismatch and Client.Status (Object) = Client.Result_Ready);
                  Check (Item.Value = CCL.VM.Integer_Constant (0));
                  Adapter.Set_Value (Object, Types, Value, 1, Token + 1, Sent);
                  Check (Sent = Client.Busy);
               end if;
               Adapter.Take_Get_Result (Object, Shifted, Item);
               Check (Item.Code = Code and Item.Revision = (if Has_Value then 1 else 0));
               Check (Item.State = (if Has_Value then Adapter.Value_Ready else Adapter.No_Value));
               Check (Item.Value = (if Has_Value then Shifted_Value else CCL.VM.Integer_Constant (0)));
               Check (Client.Status (Object) = Client.Ready);
               Adapter.Take_Get_Result (Object, Types, Item); Check (Item.State = Adapter.No_Result);
            end;
         end loop;
         Token := Token + 1;
         Client.Get (Object, Token, Sent); Check (Sent = Client.Submitted);
         Loan.Value := Image; Loan.Value.Reserved := 1;
         Client.Complete (Object, Answer (Token, W.Success, 1), Done); Check (Done = Client.Completed);
         Adapter.Take_Get_Result (Object, Shifted, Item);
         Check (Item.State = Adapter.Invalid_Completion and Client.Status (Object) = Client.Failed);
         Check (Item.Code = W.Unavailable and Item.Revision = 0 and Item.Value = CCL.VM.Integer_Constant (0));
         Adapter.Set_Value (Object, Types, Value, 1, Token + 1, Sent); Check (Sent = Client.Unavailable);
      end;
      Client.Retire (Object, Good); Check (Good);
   end Run;
begin
   G.Expected_Pages := W.Creation_Bytes / 4096;
   Run ("(+ 20 22)", "(+ 21 21)");
   Run ("true", "true");
   for Alternative in 1 .. 3 loop
      declare
         Prefix : constant String := "(type Reading (variant (Value Integer) (Unavailable) (Flag Boolean))) ";
         Expression : constant String :=
           (case Alternative is when 1 => "(Reading.Value 42)",
            when 2 => "Reading.Unavailable", when others => "(Reading.Flag true)");
      begin
         Run (Prefix & Expression, "(type Unrelated (enum Other)) " & Prefix & Expression);
      end;
   end loop;
   declare
      Object : Client.Client;
      Types : CCL.Types.Registry;
      Contract : CCL.Objects.Binding;
      Image : CCL.Objects.Image;
      Item : Adapter.Read_Result;
      Native : Client.Response;
      Built : CCL.Objects.Build_Result;
      Sent : Client.Submission;
      Done : Client.Completion_Result;
      Good, Taken : Boolean;
   begin
      CCL.Objects.Bind (Types, CCL.Types.String_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
      Image := CCL.Objects.Empty (Contract);
      CCL.Objects.Append_Text (Image, "supported native object, unsupported VM value", Built);
      Check (Built = CCL.Objects.Added);
      Client.Initialize (Object, 5, Good); Check (Good);
      Client.Open (Object, "org.cubit.test", Contract, W.Read_Only, 0, 1, Sent); Check (Sent = Client.Submitted);
      Client.Complete (Object, Answer (1, W.Success, 55), Done); Check (Done = Client.Completed);
      Client.Take_Result (Object, Native, Taken); Check (Taken and Native.Valid);
      Client.Get (Object, 2, Sent); Check (Sent = Client.Submitted);
      declare
         Loan : W.Frame with Import, Address => G.Mapping;
      begin
         Loan.Value := Image;
      end;
      Client.Complete (Object, Answer (2, W.Success, 1), Done); Check (Done = Client.Completed);
      Adapter.Take_Get_Result (Object, Types, Item);
      Check (Item.State = Adapter.Type_Mismatch and Client.Status (Object) = Client.Result_Ready);
      Client.Take_Result (Object, Native, Taken);
      Check (Taken and Native.Valid and Native.Value = Image and Client.Status (Object) = Client.Ready);
      Client.Retire (Object, Good); Check (Good);
   end;
   declare
      Object : Client.Client;
      Types : CCL.Types.Registry;
      Contract : CCL.Objects.Binding;
      Item : Adapter.Read_Result;
      Native : Client.Response;
      Sent : Client.Submission;
      Done : Client.Completion_Result;
      Completion : IPC.CompletionEntry;
      Good, Taken : Boolean;
      Count : Natural;
   begin
      CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
      Client.Initialize (Object, 5, Good); Check (Good);
      Client.Open (Object, "org.cubit.test", Contract, W.Read_Write, 0, 1, Sent); Check (Sent = Client.Submitted);
      Client.Complete (Object, Answer (1, W.Success, 55), Done); Check (Done = Client.Completed);
      Client.Take_Result (Object, Native, Taken); Check (Taken and Native.Valid);
      IPC.Accept_Submission := False;
      Adapter.Set_Value (Object, Types, CCL.VM.Integer_Constant (42), 0, 2, Sent);
      Check (Sent = Client.Not_Submitted and Client.Status (Object) = Client.Ready);
      IPC.Accept_Submission := True;
      Count := IPC.Submissions;
      Adapter.Set_Value (Object, Types, CCL.VM.Integer_Constant (42), 0, 2, Sent);
      Check (Sent = Client.Invalid_Request and IPC.Submissions = Count);
      Adapter.Set_Value (Object, Types, CCL.VM.Integer_Constant (42), 0, 3, Sent);
      Check (Sent = Client.Submitted);
      Client.Complete (Object, Answer (2, W.Success, 1), Done); Check (Done = Client.Ignored);
      Completion := Answer (3, W.Success, 1); Completion.status := IPC.COMPLETION_TARGET_DIED;
      Client.Complete (Object, Completion, Done); Check (Done = Client.Completed);
      Adapter.Take_Get_Result (Object, Types, Item); Check (Item.State = Adapter.Other_Operation);
      Client.Take_Result (Object, Native, Taken);
      Check (Taken and not Native.Valid and Client.Status (Object) = Client.Failed);
      Count := IPC.Submissions;
      Adapter.Set_Value (Object, Types, CCL.VM.Integer_Constant (42), 0, 4, Sent);
      Check (Sent = Client.Unavailable and IPC.Submissions = Count);
      Client.Retire (Object, Good); Check (Good);
   end;
   Check (IPC.Waits = 0);
   Ada.Text_IO.Put_Line ("Nonblocking Config VM adapter: PASS" & Checks'Image & " checks");
end VM_Tests;

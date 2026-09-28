with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces; use Interfaces;
with System;
with CCL.Types;
with CCL.Objects;
with CCL.Resources;
with Config_Object_Client.Resources;
with Config_Object_Messages;
with CuBit.Messages;
with CuBit.Memory_Grants;

procedure Resource_Client_Tests is
   package T renames CCL.Types;
   package O renames CCL.Objects;
   package R renames CCL.Resources;
   package C renames Config_Object_Client;
   package H renames C.Resources;
   package W renames Config_Object_Messages;
   package IPC renames CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   use type T.Definition_Result;
   use type O.Build_Result;
   use type O.Image;
   use type R.Outcome;
   use type R.Reference;
   use type C.Submission;
   use type C.Completion_Result;
   use type H.Lifetime;
   use type H.Cleanup_Result;
   Types : T.Registry;
   Kind, Wrong_Kind : T.Type_Reference;
   Defined : T.Definition_Result;
   Contract : O.Binding;
   Value : O.Image;
   Built : O.Build_Result;
   Good : Boolean;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Site & " resource client check" & Checks'Image; end if;
   end Check;
   function Reply (Token : Unsigned_64; Code : W.Status; Word : Unsigned_64 := 0)
     return IPC.CompletionEntry is
     (requestId => 1, token => Token, msg => W.Reply (Code, Word), from => 42,
      status => IPC.COMPLETION_OK, valid => True);
   type Scenario is
     (Normal, Stop_Acquire, Retire_Acquire, Stop_Read, Stop_Write, Denied_Acquire,
      Refused_Submission, Failed_Grant, Uncertain_Acquire, Uncertain_Write, Uncertain_Close);
begin
   T.Define (Types, (Identifier => T.Named ("IntegerCollection"), Form => T.Resource,
     Count => 1, Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Kind, Defined);
   Check (Defined = T.Defined);
   T.Define (Types, (Identifier => T.Named ("BooleanCollection"), Form => T.Resource,
     Count => 1, Parts => [1 => (T.Named ("Value"), T.Boolean_Type), others => <>]), Wrong_Kind, Defined);
   Check (Defined = T.Defined);
   O.Bind (Types, T.Integer_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   Value := O.Empty (Contract); O.Append (Value, O.Integer_Cell (42), Built); Check (Built = O.Added);
   G.Expected_Pages := W.Creation_Bytes / 4096;
   for Test in Scenario loop
      declare
         Owner : R.Registry (R.Context_ID (Scenario'Pos (Test) + 901));
         Session : R.Run;
         Outcome : R.Outcome;
         Object : H.Collection;
         Ref : R.Reference;
         Sent : C.Submission;
         Done : C.Completion_Result;
         Answer : C.Response;
         Taken : Boolean;
         Cleanup : H.Cleanup_Result;
         Count : Natural;
         Last_Operation : Unsigned_64 := 1;
      begin
         R.Start (Owner, Types, Session, Outcome); Check (Outcome = R.Succeeded);
         Check (H.State (Object) = H.Vacant);
         Count := IPC.Submissions;
         H.Create (Object, Owner, Session, Wrong_Kind, 5, "org.cubit.test", Contract, W.Read_Write, 0, 1, Sent);
         Check (Sent = C.Invalid_Request and IPC.Submissions = Count and R.Empty (Owner));
         IPC.Accept_Submission := Test /= Refused_Submission;
         G.Allow_Grant := Test /= Failed_Grant;
         H.Create (Object, Owner, Session, Kind, 5, "org.cubit.test", Contract, W.Read_Write, 0, 1, Sent);
         IPC.Accept_Submission := True; G.Allow_Grant := True;
         if Test in Refused_Submission | Failed_Grant then
            Check (Sent = (if Test = Failed_Grant then C.Unavailable else C.Not_Submitted));
            Check (H.State (Object) = H.Draining and H.Reference_Of (Object, Owner) = R.No_Reference);
         else
            Check (Sent = C.Submitted and H.State (Object) = H.Acquiring);
            H.Complete (Object, Owner, Reply (2, W.Success, 55), Done); Check (Done = C.Ignored);
            H.Take_Result (Object, Owner, Answer, Taken); Check (not Taken);
            if Test = Stop_Acquire then
               R.Stop (Owner, Session, Outcome); Check (Outcome = R.Succeeded);
            elsif Test = Retire_Acquire then H.Retire (Object, Owner);
            end if;
            if Test in Stop_Acquire | Retire_Acquire then
               H.Cleanup (Object, Owner, 2, Cleanup);
               Check (Cleanup = H.Completion_Pending);
            end if;
            H.Complete (Object, Owner, Reply (1,
              (if Test = Denied_Acquire then W.Denied elsif Test = Uncertain_Acquire then W.Uncertain else W.Success),
              (if Test in Denied_Acquire | Uncertain_Acquire then 0 else 55)), Done);
            Check (Done = C.Completed);
            H.Take_Result (Object, Owner, Answer, Taken); Check (Taken and Answer.Valid);
            Ref := H.Reference_Of (Object, Owner);
            if Test in Stop_Acquire | Retire_Acquire | Denied_Acquire | Uncertain_Acquire then
               Check (Ref = R.No_Reference and H.State (Object) = H.Draining);
            else
               Check (Ref /= R.No_Reference and H.State (Object) = H.Available);
               H.Get (Object, Owner, R.No_Reference, 2, Sent); Check (Sent = C.Invalid_Request);
               if Test in Stop_Write | Uncertain_Write then
                  H.Set (Object, Owner, Ref, Value, 0, 2, Sent);
               else H.Get (Object, Owner, Ref, 2, Sent); end if;
               Check (Sent = C.Submitted and H.State (Object) = H.Calling);
               if Test in Stop_Read | Stop_Write then
                  R.Stop (Owner, Session, Outcome); Check (Outcome = R.Succeeded);
               end if;
               declare Loan : W.Frame with Import, Address => G.Mapping; begin Loan.Value := Value; end;
               H.Complete (Object, Owner, Reply (2,
                 (if Test = Uncertain_Write then W.Uncertain else W.Success),
                 (if Test = Uncertain_Write then 0 else 1)), Done); Check (Done = C.Completed);
               H.Take_Result (Object, Owner, Answer, Taken); Check (Taken and Answer.Valid);
               if Test in Stop_Read | Stop_Write | Uncertain_Write then
                  Check (H.Reference_Of (Object, Owner) = R.No_Reference and H.State (Object) = H.Draining);
               else
                  Check (Answer.Value = Value);
                  H.Close (Object, Owner, Ref, 3, Sent); Check (Sent = C.Submitted);
                  declare Completion : IPC.CompletionEntry := Reply (3, W.Success); begin
                     if Test = Uncertain_Close then Completion.status := IPC.COMPLETION_TARGET_DIED; end if;
                     H.Complete (Object, Owner, Completion, Done); Check (Done = C.Completed);
                  end;
                  H.Take_Result (Object, Owner, Answer, Taken); Check (Taken);
                  Check (H.Reference_Of (Object, Owner) = R.No_Reference);
                  Last_Operation := 3;
               end if;
            end if;
         end if;
         H.Cleanup (Object, Owner, 4, Cleanup);
         if Test in Uncertain_Acquire | Uncertain_Close then
            Check (Cleanup = H.Quarantine_Required and H.State (Object) = H.Quarantined and not R.Empty (Owner));
            Count := IPC.Submissions;
            H.Create (Object, Owner, Session, Kind, 5, "org.cubit.test", Contract, W.Read_Write, 0, 5, Sent);
            Check (Sent = C.Busy and IPC.Submissions = Count);
         else
            if Cleanup = H.Close_Submitted then
               Check (IPC.Last_Request.words (0) = 55);
               H.Complete (Object, Owner, Reply (4, W.Success), Done); Check (Done = C.Completed);
               H.Take_Result (Object, Owner, Answer, Taken); Check (Taken and Answer.Valid);
               Last_Operation := 4;
               G.Is_Retired := False;
               H.Cleanup (Object, Owner, 5, Cleanup); Check (Cleanup = H.Grant_Pending and not R.Empty (Owner));
               G.Is_Retired := True;
               H.Cleanup (Object, Owner, 5, Cleanup);
            end if;
            Check (Cleanup = H.Released and H.State (Object) = H.Vacant and R.Empty (Owner));
            if Test = Normal then
               H.Create (Object, Owner, Session, Kind, 5, "org.cubit.test", Contract, W.Read_Write, 0, Last_Operation, Sent);
               Check (Sent = C.Invalid_Request and R.Empty (Owner));
               H.Create (Object, Owner, Session, Kind, 5, "org.cubit.test", Contract, W.Read_Write, 0, 10, Sent);
               Check (Sent = C.Submitted);
               H.Complete (Object, Owner, Reply (1, W.Success, 55), Done); Check (Done = C.Ignored);
               H.Complete (Object, Owner, Reply (10, W.Success, 56), Done); Check (Done = C.Completed);
               H.Take_Result (Object, Owner, Answer, Taken); Check (Taken);
               Check (H.Reference_Of (Object, Owner) /= Ref);
               H.Get (Object, Owner, Ref, 11, Sent); Check (Sent = C.Invalid_Request);
               H.Cleanup (Object, Owner, 11, Cleanup); Check (Cleanup = H.Close_Submitted);
               H.Complete (Object, Owner, Reply (11, W.Success), Done); Check (Done = C.Completed);
               H.Take_Result (Object, Owner, Answer, Taken); Check (Taken);
               H.Cleanup (Object, Owner, 12, Cleanup); Check (Cleanup = H.Released);
            end if;
         end if;
      end;
   end loop;
   declare
      Owner : R.Registry (999);
      Foreign : R.Registry (1000);
      Session, Foreign_Run : R.Run;
      Outcome : R.Outcome;
      A, B : H.Collection;
      A_Ref, B_Ref, Old_Ref : R.Reference;
      A_Mapping, B_Mapping : System.Address;
      Sent : C.Submission;
      Done : C.Completion_Result;
      Answer : C.Response;
      Taken : Boolean;
      Cleanup : H.Cleanup_Result;
      Count : Natural;
      Token : Unsigned_64 := 100;
   begin
      R.Start (Owner, Types, Session, Outcome); Check (Outcome = R.Succeeded);
      R.Start (Foreign, Types, Foreign_Run, Outcome); Check (Outcome = R.Succeeded);
      H.Create (A, Owner, Session, Kind, 5, "org.cubit.a", Contract, W.Read_Write, 0, 1, Sent);
      Check (Sent = C.Submitted); A_Mapping := G.Mapping;
      H.Create (B, Owner, Session, Kind, 6, "org.cubit.b", Contract, W.Read_Write, 0, 2, Sent);
      Check (Sent = C.Submitted); B_Mapping := G.Mapping;
      H.Complete (A, Owner, Reply (2, W.Success, 66), Done); Check (Done = C.Ignored);
      H.Complete (A, Foreign, Reply (1, W.Success, 55), Done); Check (Done = C.Ignored);
      H.Complete (A, Owner, Reply (1, W.Success, 55), Done); Check (Done = C.Completed);
      H.Take_Result (A, Foreign, Answer, Taken); Check (not Taken);
      H.Take_Result (A, Owner, Answer, Taken); Check (Taken);
      H.Complete (B, Owner, Reply (2, W.Success, 66), Done); Check (Done = C.Completed);
      H.Take_Result (B, Owner, Answer, Taken); Check (Taken);
      A_Ref := H.Reference_Of (A, Owner); B_Ref := H.Reference_Of (B, Owner);
      Check (A_Ref /= R.No_Reference and B_Ref /= R.No_Reference and A_Ref /= B_Ref);
      Count := IPC.Submissions;
      H.Get (A, Owner, B_Ref, 3, Sent); Check (Sent = C.Invalid_Request);
      H.Set (A, Owner, B_Ref, Value, 0, 3, Sent); Check (Sent = C.Invalid_Request);
      H.Close (A, Owner, B_Ref, 3, Sent); Check (Sent = C.Invalid_Request);
      H.Get (A, Foreign, A_Ref, 3, Sent); Check (Sent = C.Invalid_Request and IPC.Submissions = Count);
      H.Get (A, Owner, A_Ref, 3, Sent); Check (Sent = C.Submitted and IPC.Last_Request.words (0) = 55);
      declare Loan : W.Frame with Import, Address => A_Mapping; begin Loan.Value := Value; end;
      H.Get (B, Owner, B_Ref, 4, Sent); Check (Sent = C.Submitted and IPC.Last_Request.words (0) = 66);
      declare Loan : W.Frame with Import, Address => B_Mapping; begin Loan.Value := Value; end;
      H.Complete (A, Owner, Reply (4, W.Success, 1), Done); Check (Done = C.Ignored);
      H.Complete (A, Owner, Reply (3, W.Success, 1), Done); Check (Done = C.Completed);
      H.Take_Result (A, Owner, Answer, Taken); Check (Taken and Answer.Value = Value);
      H.Complete (B, Owner, Reply (4, W.Success, 1), Done); Check (Done = C.Completed);
      H.Take_Result (B, Owner, Answer, Taken); Check (Taken and Answer.Value = Value);
      H.Cleanup (A, Foreign, 5, Cleanup); Check (Cleanup = H.Invalid_Owner);
      H.Cleanup (A, Owner, 5, Cleanup); Check (Cleanup = H.Close_Submitted);
      H.Complete (A, Owner, Reply (5, W.Success), Done); Check (Done = C.Completed);
      H.Take_Result (A, Owner, Answer, Taken); Check (Taken);
      H.Cleanup (A, Owner, 6, Cleanup); Check (Cleanup = H.Released);
      Check (H.Reference_Of (B, Owner) = B_Ref);
      H.Cleanup (B, Owner, 6, Cleanup); Check (Cleanup = H.Close_Submitted);
      H.Complete (B, Owner, Reply (6, W.Success), Done); Check (Done = C.Completed);
      H.Take_Result (B, Owner, Answer, Taken); Check (Taken);
      H.Cleanup (B, Owner, 7, Cleanup); Check (Cleanup = H.Released and R.Empty (Owner));
      -- More acquisitions than the bounded registry contains slots. Reuse the
      -- same stable loan storage; old refs and tokens never become valid again.
      Old_Ref := A_Ref;
      for Cycle in 1 .. 3 * R.Maximum_Resources loop
         H.Create (A, Owner, Session, Kind, 5, "org.cubit.a", Contract, W.Read_Write, 0, Token, Sent);
         Check (Sent = C.Submitted);
         H.Complete (A, Owner, Reply (Token, W.Success, 55), Done); Check (Done = C.Completed);
         H.Take_Result (A, Owner, Answer, Taken); Check (Taken);
         A_Ref := H.Reference_Of (A, Owner); Check (A_Ref /= Old_Ref);
         H.Get (A, Owner, Old_Ref, Token + 1, Sent); Check (Sent = C.Invalid_Request);
         H.Cleanup (A, Owner, Token + 1, Cleanup); Check (Cleanup = H.Close_Submitted);
         H.Complete (A, Owner, Reply (Token + 1, W.Success), Done); Check (Done = C.Completed);
         H.Take_Result (A, Owner, Answer, Taken); Check (Taken);
         G.Is_Retired := False;
         H.Cleanup (A, Owner, Token + 2, Cleanup); Check (Cleanup = H.Grant_Pending and not R.Empty (Owner));
         G.Is_Retired := True;
         H.Cleanup (A, Owner, Token + 2, Cleanup); Check (Cleanup = H.Released and R.Empty (Owner));
         Old_Ref := A_Ref; Token := Token + 3;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Resource-bound Config client: PASS" & Checks'Image & " checks");
end Resource_Client_Tests;

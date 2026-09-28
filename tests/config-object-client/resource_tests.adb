with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects;
with CCL.Resources;
with Config_Object_Client;
with Config_Object_Messages;
with CuBit.Messages;
with CuBit.Memory_Grants;

-- Linux-hosted integration of the production resource registry and native
-- async Config client. IPC/grants are modeled; no public source factory yet.
procedure Resource_Tests is
   package R renames CCL.Resources;
   package C renames Config_Object_Client;
   package W renames Config_Object_Messages;
   package IPC renames CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   use type R.Outcome;
   use type R.Reference;
   use type R.Run;
   use type C.Submission;
   use type C.Completion_Result;
   use type W.Status;
   use type CCL.Types.Definition_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Objects.Image;
   type Scenario is (Ordinary_Close, Stop_During_Create, Stop_During_Get, Stop_During_Set, Denied_Create);
   Types : CCL.Types.Registry;
   Kind : CCL.Types.Type_Reference;
   Defined : CCL.Types.Definition_Result;
   Contract : CCL.Objects.Binding;
   Value : CCL.Objects.Image;
   Built : CCL.Objects.Build_Result;
   Good : Boolean;
   Checks : Natural := 0;
   function Reply (Token : Unsigned_64; Status : W.Status; Value : Unsigned_64 := 0)
     return IPC.CompletionEntry is
     (requestId => 1, token => Token, msg => W.Reply (Status, Value),
      from => 42, status => IPC.COMPLETION_OK, valid => True);
   procedure Check (OK : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Site & " Config resource check" & Checks'Image; end if;
   end Check;
begin
   T:
   declare
      package T renames CCL.Types;
   begin
      T.Define (Types, (Identifier => T.Named ("IntegerCollection"), Form => T.Resource,
        Count => 1, Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Kind, Defined);
      Check (Defined = T.Defined);
   end T;
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Value, CCL.Objects.Integer_Cell (42), Built); Check (Built = CCL.Objects.Added);
   G.Expected_Pages := W.Creation_Bytes / 4096;
   for Test in Scenario loop
      declare
         Owner : R.Registry (R.Context_ID (Scenario'Pos (Test) + 1));
         Session, Next_Run : R.Run;
         Factory, Call : R.Ticket;
         Item : R.Reference;
         Object : C.Client;
         Answer : C.Response;
         Result : R.Outcome;
         Sent : C.Submission;
         Completed : C.Completion_Result;
         Taken, Retired : Boolean;
         Stopped : Boolean := False;
      begin
         R.Start (Owner, Types, Session, Result); Check (Result = R.Succeeded);
         R.Reserve (Owner, Session, Kind, Factory, Result); Check (Result = R.Succeeded);
         C.Initialize (Object, 5, Good); Check (Good);
         C.Create (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent);
         Check (Sent = C.Submitted and W.Valid_Request (IPC.Last_Request, W.Create_Collection));
         if Test = Stop_During_Create then
            R.Stop (Owner, Session, Result); Check (Result = R.Succeeded); Stopped := True;
            R.Reclaim (Owner, Factory, Result); Check (Result = R.Not_Ready);
         end if;
         C.Complete (Object, Reply (2, W.Success, 55), Completed); Check (Completed = C.Ignored);
         Check (R.Valid_Ticket (Owner, Factory));
         C.Complete (Object, Reply (1, (if Test = Denied_Create then W.Denied else W.Success),
           (if Test = Denied_Create then 0 else 55)), Completed); Check (Completed = C.Completed);
         C.Take_Result (Object, Answer, Taken); Check (Taken and Answer.Valid);
         R.Publish (Owner, Factory, Answer.Code = W.Success, Item, Result);
         Check (Result = (if Test in Stop_During_Create | Denied_Create then R.Not_Ready else R.Succeeded));
         Check ((Item /= R.No_Reference) = (Test not in Stop_During_Create | Denied_Create));
         if Test in Stop_During_Get | Stop_During_Set then
            R.Begin_Use (Owner, Item, Kind, Call, Result); Check (Result = R.Succeeded);
            if Test = Stop_During_Get then
               C.Get (Object, 2, Sent);
               declare
                  Loan : W.Frame with Import, Address => G.Mapping;
               begin
                  Loan.Value := Value;
               end;
            else C.Set (Object, Value, 0, 2, Sent);
            end if;
            Check (Sent = C.Submitted);
            R.Stop (Owner, Session, Result); Check (Result = R.Succeeded); Stopped := True;
            Check (not R.Current (Owner, Item));
            R.Start (Owner, Types, Next_Run, Result); Check (Result = R.Busy);
            R.Reclaim (Owner, Factory, Result); Check (Result = R.Not_Ready);
            C.Complete (Object, Reply (2, W.Success, 1), Completed); Check (Completed = C.Completed);
            C.Take_Result (Object, Answer, Taken); Check (Taken and Answer.Valid and Answer.Revision = 1);
            if Test = Stop_During_Get then Check (Answer.Value = Value); end if;
            R.Finish_Use (Owner, Call, True, Result); Check (Result = R.Succeeded and not R.Current (Owner, Item));
         end if;
         if Test /= Denied_Create then
            if not Stopped then
               R.Retire (Owner, Item, Result); Check (Result = R.Succeeded);
            end if;
            R.Begin_Cleanup (Owner, Factory, Call, Result); Check (Result = R.Succeeded);
            C.Close (Object, 3, Sent); Check (Sent = C.Submitted);
            Check (W.Valid_Request (IPC.Last_Request, W.Close_Collection) and IPC.Last_Request.words (0) = 55);
            R.Reclaim (Owner, Factory, Result); Check (Result = R.Not_Ready);
            C.Complete (Object, Reply (3, W.Success), Completed); Check (Completed = C.Completed);
            C.Take_Result (Object, Answer, Taken); Check (Taken and Answer.Valid and Answer.Code = W.Success);
            R.Finish_Use (Owner, Call, False, Result); Check (Result = R.Succeeded);
         end if;
         -- Draining IPC is not the same as reclaiming a grant. The adapter
         -- must wait for confirmed retirement before releasing this slot.
         G.Is_Retired := False;
         C.Retire (Object, Retired); Check (not Retired and not R.Empty (Owner));
         R.Start (Owner, Types, Next_Run, Result); Check (Result = R.Busy);
         G.Is_Retired := True;
         C.Retire (Object, Retired); Check (Retired);
         R.Reclaim (Owner, Factory, Result); Check (Result = R.Succeeded and R.Empty (Owner));
         if not Stopped then R.Stop (Owner, Session, Result); Check (Result = R.Succeeded); end if;
         R.Start (Owner, Types, Next_Run, Result); Check (Result = R.Succeeded and Next_Run /= Session);
         Check (not R.Current (Owner, Item) and not R.Valid_Ticket (Owner, Factory));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Async Config resource stop/drain/close: PASS" & Checks'Image & " checks");
end Resource_Tests;

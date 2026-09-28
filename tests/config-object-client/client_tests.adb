with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects;
with CCL.Objects.Schemas;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with Config_Object_Messages;
with Config_Object_Client; use Config_Object_Client;

procedure Client_Tests is
   package W renames Config_Object_Messages;
   package G renames CuBit.Memory_Grants;
   use type W.Status;
   use type W.Operation;
   use type CCL.Objects.Image;
   use type CCL.Objects.Binding;
   use type CCL.Objects.Build_Result;
   Types : CCL.Types.Registry;
   Contract, Unbound : CCL.Objects.Binding;
   Value : CCL.Objects.Image;
   Built : CCL.Objects.Build_Result;
   Good : Boolean;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "native Config client check" & Checks'Image; end if;
   end Check;
   function Answer (Token : Number; Code : W.Status; Word : Number := 0) return CompletionEntry is
     (requestId => 100, token => Token, msg => W.Reply (Code, Word),
      from => 42, status => COMPLETION_OK, valid => True);
   type Fault is
     (None, Stale_Value, Missing_Value, Denied_Value, Wrong_Schema, Bad_Object,
      Zero_Revision, Large_Revision, Bad_Length, Bad_Flags, Bad_Reserved,
      Bad_Tail, Unknown_Status, Target_Died, Cancelled, Overflow, Zero_Request);
   procedure Run (Scenario : Fault; Create_First : Boolean := False) is
      Object : Client;
      Sent : Submission;
      Done : Completion_Result;
      Item : Response;
      Completion : CompletionEntry;
      OK, Taken : Boolean;
      Valid : constant Boolean := Scenario in None | Stale_Value | Missing_Value | Denied_Value;
      Has_Value : constant Boolean := Scenario in None | Stale_Value;
   begin
      Initialize (Object, 5, OK); Check (OK and Status (Object) = Ready);
      Get (Object, 1, Sent); Check (Sent = Invalid_Request);
      Open (Object, "org.cubit.settings", Unbound, W.Read_Write, 0, 1, Sent); Check (Sent = Invalid_Request);
      if Create_First then
         Create (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent);
         Check (W.Valid_Request (Last_Request, W.Create_Collection));
         declare
            Creation : W.Creation_Frame with Import, Address => G.Mapping;
            Decoded : CCL.Objects.Binding;
         begin
            CCL.Objects.Schemas.Read (Creation.Metadata, Decoded, OK);
            Check (OK and Decoded = Contract);
         end;
      else
         Open (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent);
         Check (W.Valid_Request (Last_Request, W.Open_Collection));
      end if;
      Check (Sent = Submitted);
      declare
         Loan : W.Frame with Import, Address => G.Mapping;
      begin
         Check (W.Valid_Descriptor (Loan.Control));
         Get (Object, 2, Sent); Check (Sent = Busy);
         Complete (Object, Answer (2, W.Success, 999), Done); Check (Done = Ignored);
         Complete (Object, Answer (1, W.Success, 55), Done); Check (Done = Completed);
         Take_Result (Object, Item, Taken); Check (Taken and Item.Valid and Item.Code = W.Success);
         Get (Object, 2, Sent); Check (Sent = Submitted);
         Check (W.Valid_Request (Last_Request, W.Get_Object) and Last_Request.words (0) = 55);
         Loan.Value := Value;
         Completion := Answer (2, W.Success, 1);
         case Scenario is
            when None => null;
            when Stale_Value => Completion := Answer (2, W.Stale, 1);
            when Missing_Value => Completion := Answer (2, W.Missing);
            when Denied_Value => Completion := Answer (2, W.Denied);
            when Wrong_Schema => Loan.Value.Schema (0) := 99;
            when Bad_Object => Loan.Value.Reserved := 1;
            when Zero_Revision => Completion.msg.words (0) := 0;
            when Large_Revision => Completion.msg.words (0) := Number'Last;
            when Bad_Length => Completion.msg.tag.length := 4;
            when Bad_Flags => Completion.msg.tag.flags := 1;
            when Bad_Reserved => Completion.msg.tag.reserved := 1;
            when Bad_Tail => Completion.msg.words (3) := 1;
            when Unknown_Status => Completion.msg.tag.label := 0;
            when Target_Died => Completion.status := COMPLETION_TARGET_DIED;
            when Cancelled => Completion.status := COMPLETION_CANCELLED;
            when Overflow => Completion.status := COMPLETION_QUEUE_OVERFLOW;
            when Zero_Request => Completion.requestId := 0;
         end case;
         Complete (Object, Completion, Done); Check (Done = Completed);
         Loan.Value := (others => <>); -- cannot change the owned validated result
         Complete (Object, Completion, Done); Check (Done = Ignored);
         Get (Object, 3, Sent); Check (Sent = Busy);
         Take_Result (Object, Item, Taken); Check (Taken and (Item.Valid = Valid));
         Check (Item.Value = (if Has_Value then Value else (others => <>)));
         Check (Item.Revision = (if Has_Value then 1 else 0));
         Check (Status (Object) = (if Valid then Ready else Failed));
         if Valid then
            Set (Object, Value, 1, 3, Sent); Check (Sent = Submitted);
            Check (W.Valid_Request (Last_Request, W.Set_Object) and Loan.Value = Value);
            Complete (Object, Answer (3, W.Success, 2), Done); Check (Done = Completed);
            Take_Result (Object, Item, Taken); Check (Taken and Item.Valid and Item.Revision = 2);
            Check (Item.Value = CCL.Objects.Image'(others => <>));
            Close (Object, 4, Sent); Check (Sent = Submitted);
            Check (W.Valid_Request (Last_Request, W.Close_Collection));
            Complete (Object, Answer (4, W.Success), Done); Check (Done = Completed);
            Take_Result (Object, Item, Taken); Check (Taken and Item.Valid);
            Get (Object, 5, Sent); Check (Sent = Invalid_Request);
         else
            Get (Object, 3, Sent); Check (Sent = Unavailable);
         end if;
      end;
      Retire (Object, OK); Check (OK and Status (Object) = Retired);
   end Run;
begin
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Value, CCL.Objects.Integer_Cell (42), Built); Check (Built = CCL.Objects.Added);
   G.Expected_Pages := W.Creation_Bytes / 4096;
   for Scenario in Fault loop Run (Scenario); end loop;
   for Scenario in Fault loop Run (Scenario, Create_First => True); end loop;
   declare
      Descriptor : W.Open_Descriptor;
      Message : CuBit.Messages.Message;
   begin
      for Index in 0 .. 10 loop
         W.Describe ("org.cubit.settings", W.Read_Write, 0, [1, 2, 3, 4], Descriptor, Good); Check (Good);
         case Index is
            when 0 => null;
            when 1 => Descriptor.Format := 0;
            when 2 => Descriptor.Access_Rights := 0;
            when 3 => Descriptor.Access_Rights := 4;
            when 4 => Descriptor.Reserved := 1;
            when 5 => Descriptor.Name_Length := 0;
            when 6 => Descriptor.Name_Length := Unsigned_32'Last;
            when 7 => Descriptor.Name (128) := 'x';
            when 8 => Descriptor.Schema := [others => 0];
            when 9 => Descriptor.Padding (1) := 1;
            when 10 => Descriptor.Name (1) := '.';
            when others => null;
         end case;
         Check (W.Valid_Descriptor (Descriptor) = (Index = 0));
      end loop;
      for Op in W.Operation loop
         Message := W.Request (Op, (7, 9), 55, 1); Check (W.Valid_Request (Message, Op));
         Message.words (3) := 1; Check (not W.Valid_Request (Message, Op));
         Check (not W.Valid_Reply (W.Reply (W.Stale, 1), Op) or Op = W.Get_Object);
         Check (W.Valid_Reply (W.Reply (W.Uncertain), Op) =
           (Op in W.Set_Object | W.Create_Collection));
      end loop;
      Check (not W.Valid_Reply (W.Reply (W.Success, 1), W.Set_Object, 1));
      Check (not W.Valid_Reply (W.Reply (W.Success, 1), W.Set_Object, Number'Last));
   end;
   -- A service-authenticated uncertain write is a valid receipt, but is not a
   -- reusable client. A late successful receipt cannot silently revive it.
   declare
      Object : Client;
      Sent : Submission;
      Done : Completion_Result;
      Item : Response;
      OK, Taken : Boolean;
      Count : Natural;
   begin
      Initialize (Object, 5, OK); Check (OK);
      Open (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent); Check (Sent = Submitted);
      Complete (Object, Answer (1, W.Success, 55), Done); Check (Done = Completed);
      Take_Result (Object, Item, Taken); Check (Taken and Item.Valid);
      Set (Object, Value, 0, 2, Sent); Check (Sent = Submitted);
      Complete (Object, Answer (2, W.Uncertain), Done); Check (Done = Completed);
      Take_Result (Object, Item, Taken);
      Check (Taken and Item.Valid and Item.Code = W.Uncertain and Item.Revision = 0 and Status (Object) = Failed);
      Count := Submissions;
      Set (Object, Value, 0, 3, Sent); Check (Sent = Unavailable and Submissions = Count);
      Complete (Object, Answer (2, W.Success, 1), Done); Check (Done = Ignored and Status (Object) = Failed);
      Retire (Object, OK); Check (OK);
   end;
   -- Creation can persist its schema without delivering a usable handle.
   -- A fresh authorized Open is recovery, not a retry of the failed client.
   declare
      Object, Recovered : Client;
      Sent : Submission;
      Done : Completion_Result;
      Item : Response;
      OK, Taken : Boolean;
      Count : Natural;
   begin
      Initialize (Object, 5, OK); Check (OK);
      Create (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent);
      Check (Sent = Submitted);
      Complete (Object, Answer (1, W.Uncertain), Done); Check (Done = Completed);
      Take_Result (Object, Item, Taken);
      Check (Taken and Item.Valid and Item.Code = W.Uncertain and Status (Object) = Failed);
      Count := Submissions;
      Create (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 2, Sent);
      Check (Sent = Unavailable and Submissions = Count);
      Open (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 2, Sent);
      Check (Sent = Unavailable and Submissions = Count);
      Complete (Object, Answer (1, W.Success, 55), Done);
      Check (Done = Ignored and Status (Object) = Failed);
      Retire (Object, OK); Check (OK);
      Initialize (Recovered, 5, OK); Check (OK);
      Open (Recovered, "org.cubit.settings", Contract, W.Read_Write, 0, 3, Sent);
      Check (Sent = Submitted and W.Valid_Request (Last_Request, W.Open_Collection));
      Complete (Recovered, Answer (1, W.Success, 55), Done); Check (Done = Ignored);
      Complete (Recovered, Answer (3, W.Success, 66), Done); Check (Done = Completed);
      Take_Result (Recovered, Item, Taken); Check (Taken and Item.Valid and Item.Code = W.Success);
      Get (Recovered, 4, Sent); Check (Sent = Submitted and Last_Request.words (0) = 66);
      Complete (Recovered, Answer (4, W.Missing), Done); Check (Done = Completed);
      Take_Result (Recovered, Item, Taken);
      Check (Taken and Item.Valid and Item.Code = W.Missing and Status (Recovered) = Ready);
      Close (Recovered, 5, Sent); Check (Sent = Submitted);
      Complete (Recovered, Answer (5, W.Success), Done); Check (Done = Completed);
      Take_Result (Recovered, Item, Taken); Check (Taken and Item.Valid);
      Retire (Recovered, OK); Check (OK);
   end;
   declare
      Object : Client;
      Sent : Submission;
      Done : Completion_Result;
      OK : Boolean;
   begin
      Initialize (Object, 5, OK); Check (OK);
      Accept_Submission := False;
      Open (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent); Check (Sent = Not_Submitted);
      Accept_Submission := True;
      Open (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent); Check (Sent = Invalid_Request);
      Open (Object, "org.cubit.settings", Contract, W.Read_Write, 0, Number'Last, Sent); Check (Sent = Invalid_Request);
      Open (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 2, Sent); Check (Sent = Submitted);
      G.Allow_Revoke := False; G.Is_Retired := False;
      Retire (Object, OK); Check (not OK);
      Complete (Object, Answer (2, W.Success, 55), Done); Check (Done = Ignored);
      G.Allow_Revoke := True;
      Retire (Object, OK); Check (not OK);
      G.Is_Retired := True;
      Retire (Object, OK); Check (OK);
      Initialize (Object, 5, OK); Check (not OK);
   end;
   for Bad_Stage in W.Operation loop
      declare
         Object : Client;
         Sent : Submission;
         Done : Completion_Result;
         Item : Response;
         OK, Taken : Boolean;
         Completion : CompletionEntry;
      begin
         Initialize (Object, 5, OK); Check (OK);
         if Bad_Stage = W.Create_Collection then
            Create (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent);
         else
            Open (Object, "org.cubit.settings", Contract, W.Read_Write, 0, 1, Sent);
         end if;
         Check (Sent = Submitted);
         if Bad_Stage in W.Open_Collection | W.Create_Collection then
            Completion := Answer (1, W.Success, 0); -- no zero handle
         else
            Complete (Object, Answer (1, W.Success, 55), Done); Check (Done = Completed);
            Take_Result (Object, Item, Taken); Check (Taken and Item.Valid);
            case Bad_Stage is
               when W.Get_Object =>
                  Get (Object, 2, Sent); Completion := Answer (2, W.Conflict);
               when W.Set_Object =>
                  Set (Object, Value, 1, 2, Sent); Completion := Answer (2, W.Success, 1);
               when W.Close_Collection =>
                  Close (Object, 2, Sent); Completion := Answer (2, W.Success, 55);
               when W.Open_Collection | W.Create_Collection => null;
            end case;
            Check (Sent = Submitted);
         end if;
         Complete (Object, Completion, Done); Check (Done = Completed);
         Take_Result (Object, Item, Taken); Check (Taken and not Item.Valid and Status (Object) = Failed);
         Retire (Object, OK); Check (OK);
      end;
   end loop;
   declare
      Object : Client;
      OK : Boolean;
   begin
      G.Allow_Grant := False;
      Initialize (Object, 5, OK); Check (not OK and Status (Object) = Failed);
      G.Allow_Grant := True;
      Initialize (Object, 5, OK); Check (not OK);
      Retire (Object, OK); Check (OK);
   end;
   Ada.Text_IO.Put_Line ("Native Config object client/protocol: PASS" & Checks'Image & " checks");
end Client_Tests;

with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects;
with CCL.Objects.Schemas;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with Config_Worker_Channel; use Config_Worker_Channel;
with Config_Worker_Protocol;
with Config_Worker_Messages;

procedure Config_Channel_Tests is
   package P renames Config_Worker_Protocol;
   package Grants renames CuBit.Memory_Grants;
   use type P.Frame;
   use type P.Reply_Kind;
   use type CCL.Objects.Image;
   use type CCL.Objects.Build_Result;
   use type CCL.Objects.Binding;
   Types : CCL.Types.Registry;
   Contract : CCL.Objects.Binding;
   Value : CCL.Objects.Image;
   Good : Boolean;
   Built : CCL.Objects.Build_Result;
   Cases : Natural := 0;
   function Answer (Token : Unsigned_64 := 1) return CompletionEntry is
     (requestId => 100, token => Token, msg => Config_Worker_Messages.Acknowledgment,
      from => 42, status => COMPLETION_OK, valid => True);
   function Request (Token : Unsigned_64 := 1; Op : P.Operation := P.Commit) return P.Frame is
      Frame : P.Frame;
      Valid : Boolean;
   begin
      P.Make_Request (Op, 10, Token, 0, "org.cubit.settings", "test",
                      Contract, Value, Frame, Valid);
      pragma Assert (Valid);
      return Frame;
   end Request;

   procedure Round_Trip (Fault : Natural) is
      Object : Channel;
      Input : P.Frame := Request;
      Original : constant P.Frame := Input;
      Expected, Output : P.Frame;
      Completion : CompletionEntry := Answer;
      Ok, Valid, Taken : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Calls : constant Natural := Submissions;
   begin
      Initialize (Object, 12, Ok);
      pragma Assert (Ok and Status (Object) = Ready);
      declare
         Loan : P.Frame with Import, Address => Grants.Mapping;
      begin
         Submit (Object, Input, Contract, Sent);
         pragma Assert (Sent = Submitted and Status (Object) = Waiting);
         pragma Assert (Submissions = Calls + 1 and Last_Token = 1 and Last_Endpoint = 12);
         pragma Assert (Last_Request.tag.label = Config_Worker_Messages.Operation'Enum_Rep
                          (Config_Worker_Messages.Exchange_Frame));
         pragma Assert (Last_Request.words = [7, 9, P.Frame_Bytes, Unsigned_64 (P.Version)]);
         pragma Assert (Loan = Original);
         Input.Session := 999; Input.Value := (others => <>);
         pragma Assert (Loan = Original); -- caller cannot mutate retained request
         Submit (Object, Request (2), Contract, Sent);
         pragma Assert (Sent = Busy);
         Complete (Object, Answer (2), Done);
         pragma Assert (Done = Ignored and Pending_Token (Object) = 1);
         Complete (Object, NULL_COMPLETION, Done);
         pragma Assert (Done = Ignored);
         Take_Result (Object, Output, Valid, Taken);
         pragma Assert (not Taken and not Valid);

         P.Make_Reply (Original, P.Committed, 1, Contract, Value, Loan, Ok);
         pragma Assert (Ok);
         case Fault is
            when 1 => Completion.status := COMPLETION_TARGET_DIED;
            when 2 => Completion.status := COMPLETION_CANCELLED;
            when 3 => Completion.status := COMPLETION_QUEUE_OVERFLOW;
            when 4 => Completion.requestId := 0;
            when 5 => Completion.msg.tag.length := 4;
            when 6 => Completion.msg.tag.flags := 1;
            when 7 => Completion.msg.tag.reserved := 1;
            when 8 => Completion.msg.words (0) := P.Frame_Bytes - 1;
            when 9 => Loan.Revision := 0;
            when 10 => Loan.Token := 2;
            when 11 => Loan.Session := 11;
            when 12 => Loan.Name (1) := 'x';
            when 13 => Loan.Context (1) := 'x';
            when 14 => Loan.Reserved := 1;
            when 15 => Loan.Padding (1) := 1;
            when 16 => Loan.Value := Value;
            when 17 => Loan.Reply := Unsigned_32'Last;
            when 18 => Loan.Format := 0;
            when 19 => Loan.Action := P.Operation'Enum_Rep (P.Load);
            when 20 => Loan.Name_Length := Unsigned_32'Last;
            when 21 => Loan.Context_Length := Unsigned_32'Last;
            when 22 => Completion.msg.tag.label := 16#F007#;
            when 23 => Completion.msg.words (3) := 1;
            when 24 => P.Make_Reply (Original, P.Uncertain, 0, Contract, Value, Loan, Ok);
            when 25 => P.Make_Reply (Original, P.Rejected, 0, Contract, Value, Loan, Ok);
            when 26 => P.Make_Reply (Original, P.Conflict, 2, Contract, Value, Loan, Ok);
            when others => null;
         end case;
         Expected := Loan;
         Complete (Object, Completion, Done);
         pragma Assert (Done = Completed and Status (Object) = Result_Ready);
         -- Even an authorized worker retaining its mapping cannot alter the
         -- owned response after completion validation.
         Loan := (others => <>);
         Complete (Object, Completion, Done);
         pragma Assert (Done = Ignored);
         Submit (Object, Request (2), Contract, Sent);
         pragma Assert (Sent = Busy);
         Take_Result (Object, Output, Valid, Taken);
         pragma Assert (Taken and (Valid = (Fault = 0 or Fault >= 24)));
         pragma Assert (Output = (if Valid then Expected else (others => <>)));
         pragma Assert (Pending_Token (Object) = 0);
         pragma Assert (Status (Object) = (if Fault in 1 .. 24 then Failed else Ready));
         Submit (Object, Request (2), Contract, Sent);
         pragma Assert (Sent = (if Fault in 1 .. 24 then Unavailable else Submitted));
         Retire (Object, Ok);
         pragma Assert (Ok and Status (Object) = Retired);
      end;
      Cases := Cases + 1;
   end Round_Trip;

   procedure Loads (Kind : P.Reply_Kind) is
      Object : Channel;
      Input : constant P.Frame := Request (1, P.Load);
      Output, Expected : P.Frame;
      Ok, Valid, Taken : Boolean;
      Sent : Submission;
      Done : Completion_Result;
   begin
      Initialize (Object, 5, Ok); pragma Assert (Ok);
      Submit (Object, Input, Contract, Sent); pragma Assert (Sent = Submitted);
      declare
         Loan : P.Frame with Import, Address => Grants.Mapping;
      begin
         P.Make_Reply (Input, Kind, (if Kind = P.Loaded then 1 else 0),
                       Contract, Value, Loan, Ok);
         pragma Assert (Ok);
         Expected := Loan;
         Complete (Object, Answer, Done); pragma Assert (Done = Completed);
         Loan := (others => <>);
         Take_Result (Object, Output, Valid, Taken);
         pragma Assert (Taken and Valid and Output = Expected);
         pragma Assert (Status (Object) = (if Kind = P.Load_Failed then Failed else Ready));
         Retire (Object, Ok); pragma Assert (Ok);
      end;
      Cases := Cases + 1;
   end Loads;

   procedure Provisioning (Fault : Natural) is
      Object : Channel;
      Restored, Empty : CCL.Objects.Binding;
      Ok, Valid, Taken : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Completion : CompletionEntry := Answer;
      Output : P.Frame;
   begin
      Initialize (Object, 12, Ok); pragma Assert (Ok);
      Provision (Object, Empty, 1, Sent); pragma Assert (Sent = Invalid_Request);
      Provision (Object, Contract, 0, Sent); pragma Assert (Sent = Invalid_Request);
      Provision (Object, Contract, NO_COMPLETION_TOKEN, Sent); pragma Assert (Sent = Invalid_Request);
      Provision (Object, Contract, 1, Sent); pragma Assert (Sent = Submitted);
      pragma Assert (Config_Worker_Messages.Valid_Schema_Request (Last_Request));
      declare
         Loan : CCL.Objects.Schemas.Image with Import, Address => Grants.Mapping;
      begin
         CCL.Objects.Schemas.Read (Loan, Restored, Ok);
         pragma Assert (Ok and Restored = Contract);
      end;
      Submit (Object, Request (2), Contract, Sent); pragma Assert (Sent = Busy);
      Provision (Object, Contract, 2, Sent); pragma Assert (Sent = Busy);
      Completion.msg := Config_Worker_Messages.Schema_Acknowledgment;
      case Fault is
         when 1 => Completion.msg := Config_Worker_Messages.Acknowledgment;
         when 2 => Completion.msg.tag.length := 0;
         when 3 => Completion.msg.words (1) := 1;
         when 4 => Completion.msg := Config_Worker_Messages.Error (Config_Worker_Messages.Denied);
         when 5 => Completion.status := COMPLETION_TARGET_DIED;
         when 6 => Completion.requestId := 0;
         when 7 => Completion.msg.tag.flags := 1;
         when 8 => Completion.msg.tag.reserved := 1;
         when 9 => Completion.msg.words (0) := P.Frame_Bytes;
         when others => null;
      end case;
      Complete (Object, Completion, Done); pragma Assert (Done = Completed);
      Take_Result (Object, Output, Valid, Taken);
      pragma Assert (not Taken and not Valid and Status (Object) = Result_Ready);
      Take_Provision_Result (Object, Valid, Taken);
      pragma Assert (Taken and (Valid = (Fault = 0)));
      pragma Assert (Status (Object) = (if Fault = 0 then Ready else Failed));
      Take_Provision_Result (Object, Valid, Taken); pragma Assert (not Taken and not Valid);
      if Fault = 0 then
         Provision (Object, Contract, 1, Sent); pragma Assert (Sent = Invalid_Request);
         Submit (Object, Request (2), Contract, Sent); pragma Assert (Sent = Submitted);
         --  Provisioning success cannot stand in for a durable frame receipt.
         Completion.token := 2; Complete (Object, Completion, Done); pragma Assert (Done = Completed);
         Take_Provision_Result (Object, Valid, Taken); pragma Assert (not Taken);
         Take_Result (Object, Output, Valid, Taken);
         pragma Assert (Taken and not Valid and Status (Object) = Failed);
      end if;
      Retire (Object, Ok); pragma Assert (Ok);
      Cases := Cases + 1;
   end Provisioning;
begin
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, Good);
   pragma Assert (Good);
   Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Value, CCL.Objects.Integer_Cell (42), Built);
   pragma Assert (Built = CCL.Objects.Added);
   Grants.Expected_Pages := Loan_Bytes_Count / 4096;
   for Fault in 0 .. 26 loop Round_Trip (Fault); end loop;
   Loads (P.Loaded); Loads (P.Absent); Loads (P.Load_Failed);
   for Fault in 0 .. 9 loop Provisioning (Fault); end loop;
   declare
      Object : Channel;
      Ok, Valid, Taken : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Output : P.Frame;
   begin
      Initialize (Object, 4, Ok); pragma Assert (Ok);
      Submit (Object, (others => <>), Contract, Sent);
      pragma Assert (Sent = Invalid_Request and Status (Object) = Ready);
      Accept_Submission := False;
      Submit (Object, Request, Contract, Sent);
      pragma Assert (Sent = Not_Submitted and Status (Object) = Ready);
      Accept_Submission := True;
      Submit (Object, Request, Contract, Sent); pragma Assert (Sent = Invalid_Request);
      Submit (Object, Request (2), Contract, Sent); pragma Assert (Sent = Submitted);
      Grants.Allow_Revoke := False; Grants.Is_Retired := False;
      Retire (Object, Ok); pragma Assert (not Ok and Status (Object) = Retired);
      Complete (Object, Answer (2), Done); pragma Assert (Done = Ignored);
      Take_Result (Object, Output, Valid, Taken); pragma Assert (not Taken and not Valid);
      Grants.Allow_Revoke := True;
      Retire (Object, Ok); pragma Assert (not Ok); -- revoke accepted, still loaned
      Grants.Is_Retired := True;
      Retire (Object, Ok); pragma Assert (Ok);
      Initialize (Object, 4, Ok); pragma Assert (not Ok);
      Submit (Object, Request (3), Contract, Sent); pragma Assert (Sent = Unavailable);
      Cases := Cases + 1;
   end;
   declare
      Object : Channel;
      Ok : Boolean;
   begin
      Grants.Allow_Grant := False;
      Initialize (Object, 4, Ok); pragma Assert (not Ok and Status (Object) = Failed);
      Grants.Allow_Grant := True;
      Initialize (Object, 4, Ok); pragma Assert (not Ok);
      Retire (Object, Ok); pragma Assert (Ok);
      Cases := Cases + 1;
   end;
   Ada.Text_IO.Put_Line ("Config asynchronous worker channel: PASS" & Cases'Image & " scenarios");
end Config_Channel_Tests;

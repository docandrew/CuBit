with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with Config_Worker_Channel; use Config_Worker_Channel;
with Config_Schema_Protocol;
with Config_Worker_Protocol;
with Config_Worker_Messages;

procedure Type_Channel_Tests is
   package P renames Config_Schema_Protocol;
   package Wire renames Config_Worker_Messages;
   package Grants renames CuBit.Memory_Grants;
   use type P.Frame;
   use type P.Operation;
   Types : CCL.Types.Registry;
   Contract : CCL.Objects.Binding;
   Good : Boolean;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with "type channel check" & Checks'Image; end if;
   end Check;
   function Completion (Token : Unsigned_64 := 1) return CompletionEntry is
     (requestId => 10, token => Token, msg => Wire.Type_Acknowledgment,
      from => 42, status => COMPLETION_OK, valid => True);
begin
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1,2,3,4], Contract, Good);
   Check (Good);
   Grants.Expected_Pages := Loan_Bytes_Count / 4096;
   for Action in P.Operation loop
      for Fault in 0 .. 15 loop
         declare
            Object : Channel;
            Input, Output, Expected : P.Frame;
            Other : Config_Worker_Protocol.Frame;
            Answer : CompletionEntry := Completion;
            Ok, Valid, Taken : Boolean;
            Sent : Submission;
            Done : Completion_Result;
            Poisoned : constant Boolean := Fault /= 0;
         begin
            Initialize (Object, 12, Ok); Check (Ok);
            P.Make_Request (Action, 100, 1, "org.cubit.settings", "machine", Contract, Input, Ok);
            Check (Ok);
            Submit_Type (Object, Input, Sent);
            Check (Sent = Submitted and Status (Object) = Waiting);
            Check (Wire.Valid_Type_Request (Last_Request));
            Check (not Wire.Valid_Request (Last_Request) and not Wire.Valid_Schema_Request (Last_Request));
            declare
               Loan : P.Frame with Import, Address => Grants.Mapping;
            begin
               Check (Loan = Input);
               Submit_Type (Object, Input, Sent); Check (Sent = Busy);
               Provision (Object, Contract, 2, Sent); Check (Sent = Busy);
               Complete (Object, Completion (2), Done); Check (Done = Ignored);
               Complete (Object, NULL_COMPLETION, Done); Check (Done = Ignored);
               Take_Type_Result (Object, Output, Valid, Taken); Check (not Taken and not Valid);
               P.Make_Reply (Input, (if Action = P.Create then P.Created else P.Loaded),
                 Contract, Loan, Ok); Check (Ok);
               case Fault is
                  when 1 => Answer.status := COMPLETION_TARGET_DIED;
                  when 2 => Answer.requestId := 0;
                  when 3 => Answer.msg := Wire.Acknowledgment;
                  when 4 => Answer.msg := Wire.Schema_Acknowledgment;
                  when 5 => Answer.msg.tag.length := 0;
                  when 6 => Answer.msg.words (1) := 1;
                  when 7 => Loan.Token := 2;
                  when 8 => Loan.Session := 101;
                  when 9 => Loan.Name (1) := 'x';
                  when 10 => Loan.Context (1) := 'x';
                  when 11 => Loan.Metadata.Reserved := 1;
                  when 12 => Loan.Padding (1) := 1;
                  when 13 => Loan.Action := 0;
                  when 14 => Loan.Reply := Unsigned_32'Last;
                  when 15 =>
                     P.Make_Reply (Input, (if Action = P.Create then P.Uncertain else P.Load_Failed),
                       Contract, Loan, Ok); Check (Ok);
                  when others => null;
               end case;
               Expected := Loan;
               -- Caller/worker cannot rewrite retained request or completed result.
               Input.Session := 999;
               Complete (Object, Answer, Done); Check (Done = Completed);
               Loan := (others => <>);
               Take_Result (Object, Other, Valid, Taken); Check (not Taken and not Valid);
               Take_Provision_Result (Object, Valid, Taken); Check (not Taken and not Valid);
               Take_Type_Result (Object, Output, Valid, Taken);
               Check (Taken and (Valid = (Fault in 0 | 15)));
               if Valid then Check (Output = Expected); else Check (Output = P.Frame'(others => <>)); end if;
               Check (Status (Object) = (if Poisoned then Failed else Ready));
               Take_Type_Result (Object, Output, Valid, Taken); Check (not Taken and not Valid);
               P.Make_Request (Action, 100, 1, "org.cubit.settings", "machine", Contract, Input, Ok);
               Submit_Type (Object, Input, Sent);
               Check (Sent = (if Poisoned then Unavailable else Invalid_Request));
            end;
            Grants.Is_Retired := True;
            Retire (Object, Ok); Check (Ok);
         end;
      end loop;
   end loop;
   -- Queue rejection burns the identity without publishing a completion.
   declare
      Object : Channel;
      Input : P.Frame;
      Sent : Submission;
      Ok : Boolean;
   begin
      Initialize (Object, 12, Ok); Check (Ok);
      P.Make_Request (P.Create, 1, 1, "org.cubit", "machine", Contract, Input, Ok);
      Check (Ok);
      Accept_Submission := False;
      Submit_Type (Object, Input, Sent); Check (Sent = Not_Submitted and Status (Object) = Ready);
      Accept_Submission := True;
      Submit_Type (Object, Input, Sent); Check (Sent = Invalid_Request);
      Retire (Object, Ok); Check (Ok);
   end;
   Ada.Text_IO.Put_Line ("Native type channel: PASS" & Checks'Image & " checks");
end Type_Channel_Tests;

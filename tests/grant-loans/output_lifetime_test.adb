with Ada.Text_IO; use Ada.Text_IO;
with Memory_Grants; use Memory_Grants;
with Loan_Proof;

--  Hosted composition tests, not hardware or native multi-output integration.
--  A/B have independent presentation state and terminal mappings of one parent
--  arena. Closing output A must not close the entire arena or release B's pins.
procedure Output_Lifetime_Test is
   package L renames Loan_Proof.Production;
   package P renames Loan_Proof.Presentation;
   use type L.Reservation_Result, L.Loan_Phase, L.Loan_Reference;
   use type P.State, P.Submission_ID, P.Admission, P.Event;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "output lifetime check" & Checks'Image;
      end if;
   end Check;

   type Stalled_Phase is (Waiting_To_Read, Backend_Reading, Scanout_Held);
   procedure Scenario (Stalled_At : Stalled_Phase; Shared_Range : Boolean) is
      Parent : Lifecycle := Available_Lifecycle;
      Scope : L.State;
      A, B, Replacement : L.Loan_Reference;
      A_Frame, B_Frame : P.State;
      A_ID, B_ID, Ignored_ID : P.Submission_ID;
      Reserved : L.Reservation_Result;
      Admission : P.Admission;
      Applied, Return_Buffer : Boolean;
      User_Return_Result : Return_Result;
      Revoked : Revocation_Result;
      Released : Hold_Release_Result;
      procedure Frame_Event
        (Frame : in out P.State; ID : P.Submission_ID; Action : P.Event) is
      begin
         P.Apply (Frame, ID, Action, Applied, Return_Buffer);
         Check (Applied);
         Check (Return_Buffer = (Action = P.Release_Buffer));
      end Frame_Event;
      procedure Present_B is
      begin
         L.Acquire (Scope, B, Applied);
         Check (Applied);
         P.Submit (B_Frame, Admission, B_ID);
         Check (Admission = P.Accepted);
         Frame_Event (B_Frame, B_ID, P.Begin_Read);
         Frame_Event (B_Frame, B_ID, P.Was_Presented);
         Frame_Event (B_Frame, B_ID, P.Release_Buffer);
         L.Return_Reader (Scope, B, Applied);
         Check (Applied and L.Readers (Scope, B) = 0);
         Frame_Event (B_Frame, B_ID, P.Retire);
      end Present_B;
   begin
      Retain_Forwarding_Hold (Parent, Applied);
      Check (Applied);
      L.Configure (Scope, (42, 7), 2, Borrowed_Read_Write, L.Forward_Once,
                   Applied);
      Check (Applied);
      L.Reserve (Scope, (0, 1, Borrowed_Read_Only), A, Reserved);
      Check (Reserved = L.Reserved);
      L.Publish (Scope, A, Applied);
      Check (Applied);
      L.Reserve (Scope, ((if Shared_Range then 0 else 1), 1,
                        Borrowed_Read_Only), B, Reserved);
      Check (Reserved = L.Reserved);
      L.Publish (Scope, B, Applied);
      Check (Applied);
      L.Acquire (Scope, A, Applied);
      Check (Applied);
      P.Submit (A_Frame, Admission, A_ID);
      Check (Admission = P.Accepted);
      if Stalled_At /= Waiting_To_Read then
         Frame_Event (A_Frame, A_ID, P.Begin_Read);
      end if;
      if Stalled_At = Scanout_Held then
         Frame_Event (A_Frame, A_ID, P.Was_Presented);
      end if;

      --  Output loss closes A admission, not B or the parent allocation.
      P.Close (A_Frame);
      L.Revoke (Scope, A, Applied);
      Check (Applied and L.Phase_Of (Scope, A) = L.Draining);
      declare
         Stalled : constant P.State := A_Frame;
      begin
         for Cycle in 1 .. 128 loop
            Present_B;
            if Cycle = 1 then
               --  IDs are local to a session! The native adapter must select
               --  the authenticated output/session before applying an event.
               Check (A_ID = B_ID);
            end if;
            Check (A_Frame = Stalled and P.Buffer_Held (A_Frame));
            Check (L.Readers (Scope, A) = 1);
            Check (Has_Forwarding_Hold (Parent));
            P.Submit (A_Frame, Admission, Ignored_ID);
            Check (Admission = P.Closed);
            L.Acquire (Scope, A, Applied);
            Check (not Applied);
            L.Finish_Retirement (Scope, A, Applied);
            Check (not Applied); -- unplugging is not a final-reader release
            L.Release_Parent (Scope, Applied);
            Check (not Applied);
            Record_Return (Parent, User_Return_Result);
            Check (User_Return_Result = Return_Rejected);
         end loop;
      end;

      --  Simulate TRUSTED backend quiescence. These model events do not prove
      --  real DMA has stopped; no native adapter may infer this from unplug.
      if Stalled_At = Backend_Reading then
         Frame_Event (A_Frame, A_ID, P.Discard);
      end if;
      Frame_Event (A_Frame, A_ID, P.Release_Buffer);
      L.Return_Reader (Scope, A, Applied);
      Check (Applied and L.Phase_Of (Scope, A) = L.Unmapping);
      L.Finish_Retirement (Scope, A, Applied);
      Check (Applied); -- adapter must have unmapped/acknowledged shootdown
      Frame_Event (A_Frame, A_ID, P.Retire);
      Check (P.Is_Closed (A_Frame));
      P.Apply (A_Frame, A_ID, P.Release_Buffer, Applied, Return_Buffer);
      Check (not Applied and not Return_Buffer);
      Present_B;

      --  Reuse the retired loan slot. A delayed old reference cannot retire
      --  the replacement, and B's ongoing session is unchanged.
      L.Reserve (Scope, (0, 1, Borrowed_Read_Only), Replacement, Reserved);
      Check (Reserved = L.Reserved and Replacement /= A);
      L.Acquire (Scope, A, Applied);
      Check (not Applied);
      L.Revoke (Scope, A, Applied);
      Check (not Applied);
      Check (L.Phase_Of (Scope, Replacement) = L.Mapping);
      Present_B;

      --  Entire adapter/parent retirement is deliberately a broader operation.
      Request_Revocation (Parent, Revoked);
      Check (Revoked = Revocation_Pending);
      L.Close (Scope);
      P.Close (B_Frame);
      L.Acquire (Scope, B, Applied);
      Check (not Applied);
      L.Publish (Scope, Replacement, Applied);
      Check (not Applied);
      L.Release_Parent (Scope, Applied);
      Check (not Applied);
      L.Finish_Retirement (Scope, Replacement, Applied);
      Check (Applied);
      L.Finish_Retirement (Scope, B, Applied);
      Check (Applied);
      L.Release_Parent (Scope, Applied);
      Check (Applied);
      Release_Forwarding_Hold (Parent, Released);
      Check (Released = Revocation_Completed_On_Hold_Release);
      Check (not Is_Active (Parent));
   end Scenario;
begin
   for Stalled_At in Stalled_Phase loop
      for Shared_Range in Boolean loop
         Scenario (Stalled_At, Shared_Range);
      end loop;
   end loop;
   Put_Line ("PASS output lifetimes:" & Checks'Image & " checks");
end Output_Lifetime_Test;

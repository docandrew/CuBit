with Ada.Text_IO; use Ada.Text_IO;
with Memory_Grants; use Memory_Grants;
with Loan_Proof;

procedure Parent_Lifetime_Test is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "parent lifetime check" & Checks'Image;
      end if;
   end Check;

   --  Independent boolean/count model; exhaustive small event histories plus
   --  every count boundary below. Never edits the production private record.
   type Model is record
      Alive, Open : Boolean := True;
      Held, Used : Boolean := False;
      Users : Natural := 0;
   end record;
   type Action is (Acquire, User_Return, Retain, Release, Revoke, Receiver_Exit);

   procedure Compare (Value : Lifecycle; Expected : Model) is
   begin
      Check (Is_Valid (Value));
      Check (Is_Active (Value) = Expected.Alive);
      Check (Is_Available (Value) = Expected.Open);
      Check (Is_Revocation_Pending (Value) =
             (Expected.Alive and not Expected.Open));
      Check (Acquisition_Total (Value) = Expected.Users);
      Check (Has_Forwarding_Hold (Value) = Expected.Held);
      Check (Can_Acquire (Value) =
             (Expected.Open and Expected.Users < Maximum_Acquisition_Count));
      Check (Can_Retain_Forwarding_Hold (Value) =
             (Expected.Open and not Expected.Used));
   end Compare;

   procedure Walk (Value : Lifecycle; Expected : Model; Remaining : Natural) is
   begin
      Compare (Value, Expected);
      if Remaining = 0 then
         return;
      end if;
      for Event in Action loop
         declare
            Next : Lifecycle := Value;
            M : Model := Expected;
            Applied, Had : Boolean;
            RR : Return_Result;
            HR : Hold_Release_Result;
            VR : Revocation_Result;
         begin
            case Event is
               when Acquire =>
                  if M.Open and M.Users < Maximum_Acquisition_Count then
                     Record_Acquire (Next);
                     M.Users := M.Users + 1;
                  end if;
               when User_Return =>
                  Record_Return (Next, RR);
                  if M.Users = 0 then
                     Check (RR = Return_Rejected and Next = Value);
                  else
                     M.Users := M.Users - 1;
                     if not M.Open and M.Users = 0 and not M.Held then
                        M.Alive := False;
                        Check (RR = Revocation_Completed_On_Return);
                     else
                        Check (RR = Acquisition_Returned);
                     end if;
                  end if;
               when Retain =>
                  Retain_Forwarding_Hold (Next, Applied);
                  Check (Applied = (M.Open and not M.Used));
                  if Applied then
                     M.Held := True;
                     M.Used := True;
                  else
                     Check (Next = Value);
                  end if;
               when Release =>
                  Release_Forwarding_Hold (Next, HR);
                  if not M.Held then
                     Check (HR = Hold_Release_Rejected and Next = Value);
                  else
                     M.Held := False;
                     if not M.Open and M.Users = 0 then
                        M.Alive := False;
                        Check (HR = Revocation_Completed_On_Hold_Release);
                     else
                        Check (HR = Forwarding_Hold_Released);
                     end if;
                  end if;
               when Revoke =>
                  Request_Revocation (Next, VR);
                  if not M.Alive then
                     Check (VR = Revocation_Rejected and Next = Value);
                  else
                     M.Open := False;
                     M.Alive := M.Held or M.Users /= 0;
                     Check (VR = (if M.Alive then Revocation_Pending
                                  else Revocation_Completed));
                  end if;
               when Receiver_Exit =>
                  Close_Receiver (Next, Had);
                  Check (Had = (M.Users /= 0));
                  M.Users := 0;
                  M.Open := False;
                  M.Alive := M.Held;
            end case;
            Walk (Next, M, Remaining - 1);
         end;
      end loop;
   end Walk;

   procedure Boundaries is
      Value : Lifecycle;
      Applied, Had : Boolean;
      RR : Return_Result;
      HR : Hold_Release_Result;
      VR : Revocation_Result;
   begin
      for Count in Acquisition_Count loop
         for Held in Boolean loop
            for Exit_Receiver in Boolean loop
               Value := Available_Lifecycle;
               for I in 1 .. Count loop
                  Record_Acquire (Value);
               end loop;
               Check (Can_Acquire (Value) =
                      (Count < Maximum_Acquisition_Count));
               if Held then
                  Retain_Forwarding_Hold (Value, Applied);
                  Check (Applied); -- independent of all 127 user slots
               end if;
               Request_Revocation (Value, VR);
               Check (VR = (if Held or Count /= 0 then Revocation_Pending
                            else Revocation_Completed));
               Retain_Forwarding_Hold (Value, Applied);
               Check (not Applied);
               if Exit_Receiver then
                  Close_Receiver (Value, Had);
                  Check (Had = (Count /= 0));
               else
                  for I in reverse 1 .. Count loop
                     Record_Return (Value, RR);
                     Check (Acquisition_Total (Value) = I - 1);
                     Check (Has_Forwarding_Hold (Value) = Held);
                  end loop;
               end if;
               for I in 1 .. 3 loop
                  Record_Return (Value, RR);
                  Check (RR = Return_Rejected);
                  Check (Is_Active (Value) = Held);
               end loop;
               Release_Forwarding_Hold (Value, HR);
               Check (HR = (if Held then Revocation_Completed_On_Hold_Release
                            else Hold_Release_Rejected));
               Check (not Is_Active (Value));
               Release_Forwarding_Hold (Value, HR);
               Check (HR = Hold_Release_Rejected);
            end loop;
         end loop;
      end loop;
   end Boundaries;

   procedure Scope_Integration is
      package L renames Loan_Proof.Production;
      use type L.Reservation_Result, L.Loan_Phase;
      Parent : Lifecycle := Available_Lifecycle;
      Scope : L.State;
      Child : L.Loan_Reference;
      Reserved : L.Reservation_Result;
      Applied, Had : Boolean;
      HR : Hold_Release_Result;
      RR : Return_Result;
   begin
      Retain_Forwarding_Hold (Parent, Applied);
      Check (Applied);
      L.Configure (Scope, (17, 3), 2, Borrowed_Read_Write, L.Forward_Once,
                   Applied);
      Check (Applied);
      L.Reserve (Scope, (0, 1, Borrowed_Read_Only), Child, Reserved);
      Check (Reserved = L.Reserved);
      L.Publish (Scope, Child, Applied);
      Check (Applied);
      L.Acquire (Scope, Child, Applied);
      Check (Applied);
      Record_Acquire (Parent);
      Close_Receiver (Parent, Had);
      Check (Had and Is_Revocation_Pending (Parent));
      L.Close (Scope);
      Record_Return (Parent, RR);
      Check (RR = Return_Rejected and Has_Forwarding_Hold (Parent));
      L.Release_Parent (Scope, Applied);
      Check (not Applied);
      L.Return_Reader (Scope, Child, Applied);
      Check (Applied and L.Phase_Of (Scope, Child) = L.Unmapping);
      L.Release_Parent (Scope, Applied);
      Check (not Applied); -- a reader return alone is not unmap evidence
      L.Finish_Retirement (Scope, Child, Applied);
      Check (Applied); -- real adapter must first unmap/acknowledge shootdown
      L.Release_Parent (Scope, Applied);
      Check (Applied);
      Release_Forwarding_Hold (Parent, HR);
      Check (HR = Revocation_Completed_On_Hold_Release);
      Check (not Is_Active (Parent));
   end Scope_Integration;
begin
   Walk (Available_Lifecycle, (others => <>), 7);
   Walk (Inactive_Lifecycle, (Alive | Open => False, others => <>), 7);
   Boundaries;
   Scope_Integration;
   Put_Line ("PASS parent lifetime:" & Checks'Image & " checks");
end Parent_Lifetime_Test;

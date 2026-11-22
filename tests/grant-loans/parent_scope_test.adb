with Ada.Text_IO;
with Interfaces; use type Interfaces.Unsigned_64;
with Memory_Grants; use Memory_Grants;
with Memory_Grants.Loans;
procedure Parent_Scope_Test is
   package L is new Memory_Grants.Loans;
   use L;
   Identity : constant Reference := (0, Initial_Generation);
   Stale : constant Reference := (0, Initial_Generation + 1);
   OK : Boolean;
   Released : Hold_Release_Result;
begin
   for Flags in Interfaces.Unsigned_64 range 0 .. 15 loop
      pragma Assert (Valid_Creation_Request (1, Flags) = (Flags <= 7));
      pragma Assert (Valid_Creation_Request (4096, Flags) = (Flags <= 7));
      pragma Assert (not Valid_Creation_Request (0, Flags));
      pragma Assert (not Valid_Creation_Request (4097, Flags));
   end loop;
   pragma Assert (not Valid_Creation_Request (Interfaces.Unsigned_64'Last, 0));
   pragma Assert (not Valid_Creation_Request (1, Interfaces.Unsigned_64'Last));
   -- Both orders of parent revocation and ordinary reader return; receiver
   -- death is a third path. All must retain parent until child shootdown.
   for Scenario in 0 .. 2 loop
      declare
         Parent : Lifecycle := Available_Lifecycle;
         Scope : State;
         Child : Loan_Reference;
         Reservation : Reservation_Result;
         Revoked : Revocation_Result;
         Returned : Return_Result;
         Had : Boolean;
      begin
         L.Open_Forwarding (Scope, Parent, Identity, 4, Borrowed_Read_Write,
                      Forward_Once, OK);
         pragma Assert (not OK and Phase (Scope) = Unconfigured);
         Record_Acquire (Parent);
         L.Open_Forwarding (Scope, Parent, Identity, 4, Borrowed_Read_Write,
                      No_Forwarding, OK);
         pragma Assert (not OK and not Has_Forwarding_Hold (Parent));
         L.Open_Forwarding (Scope, Parent, Identity, 4, Borrowed_Read_Write,
                      Forward_Once, OK);
         pragma Assert (OK and Has_Forwarding_Hold (Parent));
         Reserve (Scope, (1, 2, Borrowed_Read_Only), Child, Reservation);
         pragma Assert (Reservation = Reserved);
         Publish (Scope, Child, OK); pragma Assert (OK);
         Acquire (Scope, Child, OK); pragma Assert (OK);
         L.Close_Forwarding (Scope, Stale, OK);
         pragma Assert (not OK and Phase (Scope) = Accepting);
         L.Release_Forwarding (Scope, Parent, Identity, OK, Released);
         pragma Assert (not OK and Has_Forwarding_Hold (Parent));
         case Scenario is
            when 0 =>
               Request_Revocation (Parent, Revoked);
               pragma Assert (Revoked = Revocation_Pending);
               Record_Return (Parent, Returned);
               pragma Assert (Returned = Acquisition_Returned);
            when 1 =>
               Record_Return (Parent, Returned);
               pragma Assert (Returned = Acquisition_Returned);
               Request_Revocation (Parent, Revoked);
               pragma Assert (Revoked = Revocation_Pending);
            when 2 =>
               Close_Receiver (Parent, Had);
               pragma Assert (Had);
         end case;
         pragma Assert (Is_Active (Parent) and Has_Forwarding_Hold (Parent));
         L.Close_Forwarding (Scope, Identity, OK);
         pragma Assert (OK and Phase_Of (Scope, Child) = Draining);
         L.Release_Forwarding (Scope, Parent, Identity, OK, Released);
         pragma Assert (not OK);
         Return_Reader (Scope, Child, OK);
         pragma Assert (OK and Phase_Of (Scope, Child) = Unmapping);
         L.Release_Forwarding (Scope, Parent, Identity, OK, Released);
         pragma Assert (not OK); -- no acknowledged unmap/shootdown yet
         Finish_Retirement (Scope, Child, OK); pragma Assert (OK);
         L.Release_Forwarding (Scope, Parent, Stale, OK, Released);
         pragma Assert (not OK and Has_Forwarding_Hold (Parent));
         L.Release_Forwarding (Scope, Parent, Identity, OK, Released);
         pragma Assert (OK and Released = Revocation_Completed_On_Hold_Release
                        and not Is_Active (Parent) and Phase (Scope) = Retired);
         L.Release_Forwarding (Scope, Parent, Identity, OK, Released);
         pragma Assert (not OK);
         L.Open_Forwarding (Scope, Parent, Identity, 4, Borrowed_Read_Write,
                      Forward_Once, OK);
         pragma Assert (not OK);
      end;
   end loop;
   -- Native receiver-exit adapter revokes the terminal child, then drains only
   -- that dead receiver's readers. Cover zero through the acquisition ceiling,
   -- both before and after upstream closure, without consuming the root reader.
   for Closed_Upstream in Boolean loop
      for Count in Acquisition_Count loop
         declare
            Parent : Lifecycle := Available_Lifecycle;
            Scope : State;
            Child : Loan_Reference;
            Reservation : Reservation_Result;
         begin
            Record_Acquire (Parent);
            L.Open_Forwarding (Scope, Parent, Identity, 1, Borrowed_Read_Write,
                              Forward_Once, OK);
            pragma Assert (OK);
            Reserve (Scope, (0, 1, Borrowed_Read_Only), Child, Reservation);
            pragma Assert (Reservation = Reserved);
            Publish (Scope, Child, OK); pragma Assert (OK);
            for Reader in 1 .. Count loop
               Acquire (Scope, Child, OK); pragma Assert (OK);
            end loop;
            if Closed_Upstream then
               L.Close_Forwarding (Scope, Identity, OK); pragma Assert (OK);
            else
               Revoke (Scope, Child, OK); pragma Assert (OK);
            end if;
            for Reader in 1 .. Readers (Scope, Child) loop
               Return_Reader (Scope, Child, OK); pragma Assert (OK);
            end loop;
            pragma Assert (Phase_Of (Scope, Child) = Unmapping);
            Finish_Retirement (Scope, Child, OK); pragma Assert (OK);
            if not Closed_Upstream then
               L.Close_Forwarding (Scope, Identity, OK); pragma Assert (OK);
            end if;
            L.Release_Forwarding (Scope, Parent, Identity, OK, Released);
            pragma Assert (OK and Released = Forwarding_Hold_Released
                           and Acquisition_Total (Parent) = 1);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line
     ("Parent/scope composition PASS: revoke/return/death, stale identity, child drain and shootdown gates (policy only)");
end Parent_Scope_Test;

with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Closed_Tables;
procedure Table_Ticket_Tests is
   Live : Boolean := True;
   function Ready return Boolean is (Live);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = 42 then Stamp else 0);
   package B is new Intel_GPU_Buffer_Requests (Session_Of, Ready);
   package Closed is new B.Closed_Tables;
   Pool : B.Service;
   ID, Previous, Pinned, App : B.Ticket := 0;
   OK, Consumed : Boolean;
   Response : B.Words;
begin
   B.Reserve_Private (Pool, 42, ID, Kind => B.Incremental_Tables, Pages => 1);
   pragma Assert (ID = 0);
   B.Reserve_Private (Pool, 0, ID, True, B.Incremental_Tables, Pages => 1);
   pragma Assert (ID = 0);
   B.Reserve_Private (Pool, 42, Pinned, Pages => 1);
   pragma Assert (Pinned /= 0);
   pragma Assert (not B.Is_Table_Allocation (Pool, 42, Pinned, B.Replacement_Tables));
   B.Finish_Private (Pool, Pinned, Consumed); pragma Assert (Consumed);
   B.Handle (Pool, 42, 42, B.Label, 4, 0, 0, [1, B.Create, 4096, 0], Response, App);
   pragma Assert (App /= 0);
   pragma Assert (not B.Is_Table_Allocation (Pool, 42, App, B.Incremental_Tables));
   pragma Assert (not B.Is_Table_Allocation (Pool, 42, App, B.Replacement_Tables));
   B.Complete (Pool, App, (Ready => False), Response, Consumed);
   pragma Assert (Consumed);
   pragma Assert (B.Ticket_Bytes (Pool, App) = 4096); -- uncertain backing retained
   B.Retire_Session (Pool, 42);
   pragma Assert (B.Ticket_Bytes (Pool, App) = 4096);
   pragma Assert (B.Ticket_Bytes (Pool, Pinned) = 4096);
   for Cycle in 1 .. 256 loop
      declare
         Session : constant Unsigned_64 := Unsigned_64 (Cycle) + 100;
         Kind : constant B.Private_Table_Kind :=
           (if Cycle mod 2 = 0 then B.Replacement_Tables else B.Incremental_Tables);
         Other : constant B.Private_Table_Kind :=
           (if Cycle mod 2 = 0 then B.Incremental_Tables else B.Replacement_Tables);
      begin
         B.Reserve_Private (Pool, Session, ID, True, Kind, Pages => Cycle);
         pragma Assert (ID /= 0 and ID /= Previous);
         pragma Assert (B.Ticket_Bytes (Pool, ID) = Unsigned_64 (Cycle) * 4096);
         pragma Assert (B.Ticket_Bytes (Pool, Previous) = 0);
         pragma Assert (B.Is_Table_Allocation (Pool, Session, ID, Kind));
         pragma Assert (not B.Is_Table_Allocation (Pool, Session, ID, Other));
         pragma Assert (not B.Is_Table_Allocation (Pool, Session + 1, ID, Kind));
         pragma Assert (not B.Is_Table_Allocation (Pool, Session, Previous, Kind));
         Live := False;
         pragma Assert (not B.Is_Table_Allocation (Pool, Session, ID, Kind));
         Live := True;
         B.Complete (Pool, ID, (Ready => False), Response, Consumed);
         pragma Assert (not Consumed); -- cannot create an application handle
         B.Acknowledge_Private_Retirement (Pool, Session, ID, True, OK);
         pragma Assert (not OK); -- allocator is still pending
         B.Finish_Private (Pool, ID, Consumed); pragma Assert (Consumed);
         pragma Assert (B.Ticket_Bytes (Pool, ID) = Unsigned_64 (Cycle) * 4096);
         if Cycle mod 3 = 0 then
            B.Retire_Session (Pool, Session);
            pragma Assert (B.Is_Table_Allocation (Pool, Session, ID, Kind));
            Closed.Acknowledge (Pool, Session, ID, False, OK); pragma Assert (not OK);
            Closed.Acknowledge (Pool, Session + 1, ID, True, OK); pragma Assert (not OK);
            pragma Assert (B.Ticket_Bytes (Pool, ID) = Unsigned_64 (Cycle) * 4096);
            Closed.Acknowledge (Pool, Session, ID, True, OK); pragma Assert (OK);
         else
            B.Acknowledge_Private_Retirement (Pool, Session, ID, False, OK);
            pragma Assert (not OK);
            B.Acknowledge_Private_Retirement (Pool, Session + 1, ID, True, OK);
            pragma Assert (not OK);
            B.Acknowledge_Private_Retirement (Pool, Session, ID, True, OK);
            pragma Assert (OK);
         end if;
         pragma Assert (not B.Is_Table_Allocation (Pool, Session, ID, Kind));
         pragma Assert (B.Ticket_Bytes (Pool, ID) = 0);
         if Previous /= 0 then
            pragma Assert (B.Ticket_Slot (ID) = B.Ticket_Slot (Previous));
            pragma Assert (B.Ticket_Generation (ID) = B.Ticket_Generation (Previous) + 1);
         end if;
         Previous := ID;
      end;
   end loop;
   B.Reserve_Private (Pool, 999, ID, True, B.Incremental_Tables, Pages => 1);
   B.Quarantine (Pool);
   pragma Assert (B.Ticket_Bytes (Pool, ID) = 4096);
   pragma Assert (not B.Is_Table_Allocation (Pool, 999, ID, B.Incremental_Tables));
   Ada.Text_IO.Put_Line ("private table tickets: PASS 256 cross-session/role generations; no app completion; closed and live retirement");
end Table_Ticket_Tests;

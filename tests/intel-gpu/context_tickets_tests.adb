with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Requests.Contexts;
with Intel_GPU_Buffer_Requests.Closed_Tables;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Buffer_Reply;
procedure Context_Tickets_Tests is
   Live : Boolean := True;
   function Owner return Boolean is (Live);
   function No_App (Sender, Stamp : Unsigned_64) return Unsigned_64 is (0);
   package P is new Intel_GPU_Buffer_Requests (No_App, Owner);
   package C is new P.Contexts;
   package T is new P.Closed_Tables;
   Pool : P.Service;
   ID, Old, Other, Pinned, Tables : P.Ticket := 0;
   Old_Session : Unsigned_64 := 0;
   Accepted, Consumed : Boolean;
begin
   C.Reserve (Pool, 0, ID); pragma Assert (ID = 0);
   P.Reserve_Private (Pool, 100, Pinned);
   P.Finish_Private (Pool, Pinned, Consumed); pragma Assert (Consumed);
   P.Reserve_Private (Pool, 100, Tables, Reclaimable => True);
   P.Finish_Private (Pool, Tables, Consumed); pragma Assert (Consumed);
   P.Retire_Session (Pool, 100);
   C.Acknowledge (Pool, 100, Pinned, True, Accepted); pragma Assert (not Accepted);
   C.Acknowledge (Pool, 100, Tables, True, Accepted); pragma Assert (not Accepted);
   for Session in Unsigned_64 range 1001 .. 1128 loop
      C.Reserve (Pool, Session, ID);
      pragma Assert (ID = 3 + (Session - 1001) * P.Ticket_Stride);
      pragma Assert (P.Ticket_Session (Pool, ID) = Session);
      if Old /= 0 then
         P.Retire_Session (Pool, Old_Session);
         P.Finish_Private (Pool, Old, Consumed); pragma Assert (not Consumed);
         pragma Assert (P.Ticket_Session (Pool, Old) = 0);
         C.Acknowledge (Pool, Old_Session, Old, True, Accepted); pragma Assert (not Accepted);
      end if;
      C.Acknowledge (Pool, Session, ID, True, Accepted); pragma Assert (not Accepted);
      P.Finish_Private (Pool, ID, Consumed); pragma Assert (Consumed);
      pragma Assert (not C.Can_Retire (Pool, Session, ID)); -- still live
      P.Acknowledge_Private_Retirement (Pool, Session, ID, True, Accepted);
      pragma Assert (not Accepted); -- never a replacement table
      P.Retire_Session (Pool, Session);
      pragma Assert (C.Can_Retire (Pool, Session, ID));
      C.Acknowledge (Pool, Session + 1, ID, True, Accepted); pragma Assert (not Accepted);
      C.Acknowledge (Pool, Session, ID + P.Ticket_Stride, True, Accepted); pragma Assert (not Accepted);
      C.Acknowledge (Pool, Session, ID, False, Accepted); pragma Assert (not Accepted);
      Live := False;
      C.Acknowledge (Pool, Session, ID, True, Accepted); pragma Assert (not Accepted);
      C.Reserve (Pool, Session + 1, Other); pragma Assert (Other = 0);
      Live := True;
      C.Acknowledge (Pool, Session, ID, True, Accepted); pragma Assert (Accepted);
      C.Acknowledge (Pool, Session, ID, True, Accepted); pragma Assert (not Accepted);
      pragma Assert (not C.Can_Retire (Pool, Session, ID));
      if Session = 1001 then
         P.Reserve_Private (Pool, 2000, Other, Reclaimable => True);
         pragma Assert (Other = 4); -- cannot take acknowledged context slot3
         P.Finish_Private (Pool, Other, Consumed); pragma Assert (Consumed);
         P.Acknowledge_Private_Retirement (Pool, 2000, Other, True, Accepted);
         pragma Assert (Accepted); -- context reservations must not take slot4
      end if;
      Old := ID; Old_Session := Session;
   end loop;
   for Mask in Unsigned_32 range 0 .. 15 loop
      declare
         State : P.Service;
         Parent, Pending : P.Ticket;
      begin
         Live := True;
         C.Reserve (State, 77, Parent);
         pragma Assert (Parent = 1);
         -- Revocation during allocation does not discard the pending receipt.
         P.Retire_Session (State, 77);
         C.Acknowledge (State, 77, Parent, True, Accepted); pragma Assert (not Accepted);
         P.Finish_Private (State, Parent, Consumed); pragma Assert (Consumed);
         if (Mask and 1) /= 0 then P.Reserve_Private (State, 88, Pending); end if;
         if (Mask and 2) /= 0 then P.Quarantine (State); end if;
         Live := (Mask and 4) = 0;
         C.Acknowledge (State, 77, Parent, (Mask and 8) = 0, Accepted);
         pragma Assert (Accepted = (Mask = 0));
      end;
   end loop;
   Live := True;
   declare
      package E renames Intel_GPU_Physical_Extents;
      package V renames Intel_GPU_Buffer_Reply;
      Physical_Calls : Natural := 0;
      function Allocate (CPU : Unsigned_64) return Unsigned_64 is
      begin
         pragma Assert (CPU = 16#7000_0000_0000# + Unsigned_64 (Physical_Calls) * E.Block_Bytes);
         Physical_Calls := Physical_Calls + 1;
         return 16#1000_0000# + Unsigned_64 (Physical_Calls - 1) * E.Block_Bytes;
      end Allocate;
      package Supervisor is new Intel_GPU_Extent_Allocator (Owner, Allocate);
      Arena : Supervisor.Pool;
      Driver : P.Service;
      View, Neighbor, Probe : V.Extent_View;
      Parent, Previous : P.Ticket := 0;
      Released : Boolean;
      CPU, DMA, Neighbor_CPU, Neighbor_DMA : Unsigned_64 := 0;
      Before : Supervisor.Budget;
   begin
      -- This is the supervisor's real slice allocator and the driver's actual
      -- ticket bookkeeping, but no IPC, physical allocation, GPU or RAM writes.
      -- Neighbor slot 2 stays assigned throughout; only parent slot 1 recycles.
      Supervisor.Acquire_Buffer (Arena, 7, 2, 64, 1, Neighbor, Accepted);
      pragma Assert (Accepted and Physical_Calls = 1);
      Neighbor_CPU := V.CPU_Address (Neighbor);
      Neighbor_DMA := V.Page_Address (Neighbor, 0);
      for Generation in Unsigned_32 range 1 .. 256 loop
         C.Reserve (Driver, 1000 + Unsigned_64 (Generation), Parent);
         pragma Assert (P.Ticket_Slot (Parent) = 1);
         pragma Assert (P.Ticket_Generation (Parent) = Generation);
         Supervisor.Acquire_Buffer (Arena, 7, P.Ticket_Slot (Parent), 72,
           P.Ticket_Generation (Parent), View, Accepted);
         pragma Assert (Accepted);
         if Generation = 1 then
            CPU := V.CPU_Address (View); DMA := V.Page_Address (View, 0);
         end if;
         pragma Assert (V.CPU_Address (View) = CPU and V.Page_Address (View, 0) = DMA);
         P.Finish_Private (Driver, Parent, Consumed); pragma Assert (Consumed);
         P.Retire_Session (Driver, 1000 + Unsigned_64 (Generation));
         pragma Assert (C.Can_Retire (Driver, 1000 + Unsigned_64 (Generation), Parent));
         Before := Supervisor.Memory_Budget (Arena);
         if Previous /= 0 then
            Supervisor.Retire_Buffer (Arena, 7, P.Ticket_Slot (Previous),
              P.Ticket_Generation (Previous), True, Released);
            pragma Assert (not Released);
            C.Acknowledge (Driver, 999 + Unsigned_64 (Generation), Previous,
              True, Accepted);
            pragma Assert (not Accepted);
         end if;
         Supervisor.Retire_Buffer (Arena, 8, 1, Generation, True, Released);
         pragma Assert (not Released);
         Supervisor.Retire_Buffer (Arena, 7, 1, Generation, False, Released);
         pragma Assert (not Released);
         pragma Assert (Supervisor.Memory_Budget (Arena).Retained = Before.Retained);
         -- Model exact acknowledged reference retirement. The native coordinator
         -- still has to establish this fact and authenticate the reply.
         Supervisor.Retire_Buffer (Arena, 7, 1, Generation, True, Released);
         pragma Assert (Released);
         C.Acknowledge (Driver, 1000 + Unsigned_64 (Generation), Parent,
           Released, Accepted);
         pragma Assert (Accepted);
         pragma Assert (Supervisor.Memory_Budget (Arena).Retained = Before.Retained - 72 * 4096);
         pragma Assert (Supervisor.Memory_Budget (Arena).Committed = Before.Committed);
         Supervisor.Retire_Buffer (Arena, 7, 1, Generation, True, Released);
         pragma Assert (not Released);
         C.Acknowledge (Driver, 1000 + Unsigned_64 (Generation), Parent, True, Accepted);
         pragma Assert (not Accepted);
         Supervisor.Acquire_Buffer (Arena, 7, 1, 72, Generation, Probe, Accepted);
         pragma Assert (not Accepted and not V.Valid (Probe));
         Supervisor.Acquire_Buffer (Arena, 7, 2, 64, 1, Probe, Accepted);
         pragma Assert (Accepted and V.CPU_Address (Probe) = Neighbor_CPU and
           V.Page_Address (Probe, 0) = Neighbor_DMA and Physical_Calls = 1);
         Previous := Parent;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Context/supervisor composition PASS256 generations: slice reuse, stale acknowledgments rejected, neighbor retained, physical arena unchanged (host model)");
   declare
      State : P.Service;
      Parent, Pinned_Table, Replacement, Previous : P.Ticket := 0;
   begin
      C.Reserve (State, 77, Parent);
      P.Finish_Private (State, Parent, Consumed); pragma Assert (Consumed);
      P.Reserve_Private (State, 77, Pinned_Table);
      P.Finish_Private (State, Pinned_Table, Consumed); pragma Assert (Consumed);
      P.Retire_Session (State, 77);
      T.Acknowledge (State, 77, Parent, True, Accepted); pragma Assert (not Accepted);
      T.Acknowledge (State, 77, Pinned_Table, True, Accepted); pragma Assert (not Accepted);
      for Generation in Unsigned_64 range 1 .. 128 loop
         P.Reserve_Private (State, 100 + Generation, Replacement, Reclaimable => True);
         pragma Assert (Replacement = 3 + (Generation - 1) * P.Ticket_Stride);
         P.Retire_Session (State, 100 + Generation);
         pragma Assert (not T.Can_Retire (State, 100 + Generation, Replacement));
         P.Finish_Private (State, Replacement, Consumed); pragma Assert (Consumed);
         -- Repeated close must preserve the exact closed kind.
         P.Retire_Session (State, 100 + Generation);
         pragma Assert (T.Can_Retire (State, 100 + Generation, Replacement));
         P.Acknowledge_Private_Retirement (State, 100 + Generation, Replacement, True, Accepted);
         pragma Assert (not Accepted); -- live-table path must stay unavailable
         C.Acknowledge (State, 100 + Generation, Replacement, True, Accepted);
         pragma Assert (not Accepted); -- never a context parent
         T.Acknowledge (State, 101 + Generation, Replacement, True, Accepted);
         pragma Assert (not Accepted);
         T.Acknowledge (State, 100 + Generation, Replacement, False, Accepted);
         pragma Assert (not Accepted);
         if Previous /= 0 then
            P.Retire_Session (State, 99 + Generation);
            T.Acknowledge (State, 99 + Generation, Previous, True, Accepted);
            pragma Assert (not Accepted);
         end if;
         T.Acknowledge (State, 100 + Generation, Replacement, True, Accepted);
         pragma Assert (Accepted);
         T.Acknowledge (State, 100 + Generation, Replacement, True, Accepted);
         pragma Assert (not Accepted);
         Previous := Replacement;
      end loop;
      -- Re-reservation clears the closed latch and restores ordinary private
      -- allocation behavior; only a new close can enter the cleanup path.
      P.Reserve_Private (State, 300, Replacement, Reclaimable => True);
      P.Finish_Private (State, Replacement, Consumed); pragma Assert (Consumed);
      pragma Assert (not T.Can_Retire (State, 300, Replacement));
      P.Acknowledge_Private_Retirement (State, 300, Replacement, True, Accepted);
      pragma Assert (Accepted);
   end;
   for Mask in Unsigned_32 range 0 .. 15 loop
      declare State : P.Service; Replacement, Pending : P.Ticket; begin
         Live := True;
         P.Reserve_Private (State, 77, Replacement, Reclaimable => True);
         P.Finish_Private (State, Replacement, Consumed); pragma Assert (Consumed);
         P.Retire_Session (State, 77);
         if (Mask and 1) /= 0 then P.Reserve_Private (State, 88, Pending); end if;
         if (Mask and 2) /= 0 then P.Quarantine (State); end if;
         Live := (Mask and 4) = 0;
         T.Acknowledge (State, 77, Replacement, (Mask and 8) = 0, Accepted);
         pragma Assert (Accepted = (Mask = 0));
      end;
   end loop;
   Live := True;
   Ada.Text_IO.Put_Line ("Closed replacement tickets PASS128 generations +16 faults; revoked cleanup, kind isolation, stale rejection and live-path separation");
   declare State : P.Service; First, Second : P.Ticket; begin
      C.Reserve (State, 77, First);
      P.Finish_Private (State, First, Consumed); pragma Assert (Consumed);
      P.Retire_Session (State, 77);
      C.Reserve (State, 88, Second);
      pragma Assert (First = 1 and Second = 2); -- close alone never releases
   end;
   Ada.Text_IO.Put_Line ("Context tickets PASS128 owner generations +16 failure combinations; revoked cleanup, kind isolation, stale/duplicate rejection, pending receipt retention (metadata only)");
end Context_Tickets_Tests;

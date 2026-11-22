with Ada.Text_IO;
with Interfaces;
with Compositor_Source_Loans;
procedure Source_Loans_Tests is
   package L renames Compositor_Source_Loans;
   use L;
   -- Desktop's first table: eight surfaces, two publication slots plus one
   -- replacement-acquisition allowance per surface. Metadata only.
   Surfaces : constant := 8;
   subtype Table is State (Surfaces * 3);
   subtype Slots is Slot range 1 .. Surfaces * 3;
   S, Saved : Table;
   Tickets : array (Slots) of Ticket;
   Fresh, None, Old : Ticket;
begin
   for I in Slots loop
      Reserve (S, Tickets (I)); pragma Assert (Tickets (I) /= No_Ticket);
      Activate (S, Tickets (I));
   end loop;
   Saved := S;
   Reserve (S, None); pragma Assert (None = No_Ticket and S = Saved);
   -- Closing/replacing every visible surface does not free a grant while its
   -- renderer reader remains busy. Repeated polls preserve exact state.
   for I in Slots loop Retire (S, Tickets (I)); end loop;
   Saved := S;
   for Poll in 1 .. 1000 loop
      for I in Slots loop Observe_Renderer (S, Tickets (I), Busy); end loop;
      pragma Assert (S = Saved);
      Reserve (S, None); pragma Assert (None = No_Ticket and S = Saved);
   end loop;
   for I in Slots loop
      Observe_Renderer (S, Tickets (I), Retired);
      pragma Assert (Current (S, Tickets (I)) = Grant_Pending);
      Reserve (S, None); pragma Assert (None = No_Ticket);
      Observe_Grant (S, Tickets (I), True);
      Old := Tickets (I);
      Reserve (S, Fresh);
      pragma Assert (Fresh /= No_Ticket and Fresh /= Old and Index (Fresh) = I);
      pragma Assert (Current (S, Old) = Released and Current (S, Fresh) = Reserved);
      Cancel (S, Fresh); pragma Assert (Current (S, Fresh) = Released);
      Reserve (S, Tickets (I)); Activate (S, Tickets (I));
   end loop;
   -- Both kinds of uncertainty keep their occupied slots permanently held.
   Retire (S, Tickets (1)); Observe_Renderer (S, Tickets (1), Uncertain);
   Retire (S, Tickets (2)); Observe_Renderer (S, Tickets (2), Retired);
   Observe_Grant (S, Tickets (2), False);
   pragma Assert (Current (S, Tickets (1)) = Quarantined and Current (S, Tickets (2)) = Quarantined);
   Saved := S; Reserve (S, None); pragma Assert (None = No_Ticket and S = Saved);
   -- The table grows (Desktop doubles it with its surface table): every
   -- loan keeps its slot and phase, quarantine included, and the new
   -- slots serve reservations after the old ones.
   declare
      Larger : State (2 * Surfaces * 3);
   begin
      Grow (S, Larger);
      for I in Slots loop
         pragma Assert (At_Slot (Larger, I) = At_Slot (S, I));
         pragma Assert (Current (Larger, Tickets (I)) = Current (S, Tickets (I)));
      end loop;
      pragma Assert (Interfaces."=" (Last_Serial (Larger), Last_Serial (S)));
      Reserve (Larger, Fresh);
      pragma Assert (Fresh /= No_Ticket and Index (Fresh) = Slots'Last + 1);
      pragma Assert (Current (Larger, Fresh) = Reserved and Current (S, Fresh) = Unused);
      Activate (Larger, Fresh);
      Retire (Larger, Tickets (3));
      pragma Assert (Current (Larger, Tickets (3)) = Renderer_Pending);
   end;
   Ada.Text_IO.Put_Line ("SOURCE-LOANS: PASS 24000 busy observations, exhaustion, ordered release, stale tickets, cancellation, uncertainty and growth");
end Source_Loans_Tests;

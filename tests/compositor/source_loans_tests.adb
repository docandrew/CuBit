with Ada.Text_IO;
with Source_Loans_Check;
procedure Source_Loans_Tests is
   package L renames Source_Loans_Check.L;
   use L;
   S, Saved : State;
   Tickets : array (Slot) of Ticket;
   Fresh, None, Old : Ticket;
begin
   for I in Slot loop
      Reserve (S, Tickets (I)); pragma Assert (Tickets (I) /= No_Ticket);
      Activate (S, Tickets (I));
   end loop;
   Saved := S;
   Reserve (S, None); pragma Assert (None = No_Ticket and S = Saved);
   -- Closing/replacing every visible surface does not free a grant while its
   -- renderer reader remains busy. Repeated polls preserve exact state.
   for I in Slot loop Retire (S, Tickets (I)); end loop;
   Saved := S;
   for Poll in 1 .. 1000 loop
      for I in Slot loop Observe_Renderer (S, Tickets (I), Busy); end loop;
      pragma Assert (S = Saved);
      Reserve (S, None); pragma Assert (None = No_Ticket and S = Saved);
   end loop;
   for I in Slot loop
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
   Ada.Text_IO.Put_Line ("SOURCE-LOANS: PASS 24000 busy observations, exhaustion, ordered release, stale tickets, cancellation and uncertainty");
end Source_Loans_Tests;

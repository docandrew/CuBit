with Ada.Text_IO; use Ada.Text_IO;
with Compositor_Storage;
with Storage_Model;
procedure Storage_Tests is
   package R renames Storage_Model;
   use type R.Ticket, R.Phase, R.Slot;
   package Small is new Compositor_Storage (Last_Identity => 7);
   use type Small.Ticket;
   S : R.State := R.Open (1_024);
   T, Old, Neighbor : R.Ticket;
   Exhaust : Small.State := Small.Open (1);
   E : Small.Ticket;
   Saved : array (1 .. 7) of Small.Ticket;
   Held : array (R.Slot) of R.Ticket;
begin
   R.Reserve (S, 300, Neighbor); R.Allocated (S, Neighbor, True);
   for Cycle in 1 .. 4_096 loop
      R.Reserve (S, 724, T);
      pragma Assert (T /= R.No_Ticket and R.Charged (S) = 1_024);
      if Cycle > 1 then pragma Assert (not R.Current (S, Old) and T /= Old); end if;
      R.Allocated (S, T, True);
      R.Begin_Release (S, T, False);
      pragma Assert (R.Status (S, T) = R.Live and R.Charged (S) = 1_024);
      R.Begin_Release (S, T, True);
      pragma Assert (R.Status (S, T) = R.Releasing and R.Charged (S) = 1_024);
      Old := T;
      R.Reserve (S, 1, T);
      pragma Assert (T = R.No_Ticket);
      R.Released (S, Old, True);
      pragma Assert (not R.Current (S, Old) and R.Charged (S) = 300 and
                     R.Current (S, Neighbor) and R.Bytes (S, Neighbor) = 300);
   end loop;
   -- Unknown allocation/release outcomes consume both bytes and bounded slots.
   S := R.Open (Natural'Last);
   for I in R.Slot loop
      R.Reserve (S, Positive (I), Held (I));
      R.Allocated (S, Held (I), I mod 2 = 0);
      if I mod 2 = 0 then
         R.Begin_Release (S, Held (I), True);
         R.Released (S, Held (I), False);
      end if;
      pragma Assert (R.Status (S, Held (I)) = R.Quarantined);
   end loop;
   R.Reserve (S, 1, T);
   pragma Assert (T = R.No_Ticket and R.Charged (S) = 36 and R.Valid (S));
   -- Capacity and arithmetic boundary: exact maximum refund and fresh identity.
   S := R.Open (Natural'Last);
   R.Reserve (S, Positive'Last, T); R.Allocated (S, T, True);
   R.Begin_Release (S, T, True); R.Released (S, T, True);
   pragma Assert (R.Charged (S) = 0 and not R.Current (S, T));
   Old := T; R.Reserve (S, Positive'Last, T);
   pragma Assert (T /= Old and not R.Current (S, Old));
   -- Reduced identity space exercises the production exhaustion branch.
   for I in Saved'Range loop
      Small.Reserve (Exhaust, 1, E); Saved (I) := E;
      pragma Assert (E /= Small.No_Ticket);
      for J in 1 .. I - 1 loop pragma Assert (not Small.Current (Exhaust, Saved (J))); end loop;
      Small.Allocated (Exhaust, E, True);
      Small.Begin_Release (Exhaust, E, True); Small.Released (Exhaust, E, True);
   end loop;
   Small.Reserve (Exhaust, 1, E);
   pragma Assert (E = Small.No_Ticket and Small.Charged (Exhaust) = 0);
   Put_Line ("COMPOSITOR-STORAGE: PASS 4096 reuse cycles, retained failures, neighbor isolation, overflow and identity exhaustion");
end Storage_Tests;

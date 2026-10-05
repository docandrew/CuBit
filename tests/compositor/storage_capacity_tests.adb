with Ada.Text_IO; with Storage_GPU_Model;
procedure Storage_Capacity_Tests is
   package A renames Storage_GPU_Model;
   use type A.Ticket, A.Phase, A.Slot;
   Total : constant := 140 * 141 / 2;
   S : A.State := A.Open (Total);
   Keys : array (A.Slot) of A.Ticket;
   Old, Extra : A.Ticket;
   Charged : Natural := 0;
begin
   pragma Assert (A.Slot'Last = 140 and A.Valid (S));
   for I in A.Slot loop
      A.Reserve (S, Natural (I), Keys (I)); pragma Assert (Keys (I) /= A.No_Ticket);
      A.Allocated (S, Keys (I), True); Charged := Charged + Natural (I);
      pragma Assert (A.Valid (S) and A.Charged (S) = Charged);
   end loop;
   pragma Assert (Charged = Total);
   A.Reserve (S, 1, Extra); pragma Assert (Extra = A.No_Ticket and A.Charged (S) = Total);
   for Cycle in 1 .. 8 loop
      for I in A.Slot loop
         Old := Keys (I);
         A.Begin_Release (S, Old, True); A.Released (S, Old, True);
         pragma Assert (not A.Current (S, Old) and A.Charged (S) = Total - Natural (I));
         A.Reserve (S, Natural (I), Keys (I)); pragma Assert (Keys (I) /= A.No_Ticket and Keys (I) /= Old);
         A.Allocated (S, Keys (I), True);
         pragma Assert (A.Valid (S) and A.Charged (S) = Total and not A.Current (S, Old));
         for J in A.Slot loop pragma Assert (A.Current (S, Keys (J)) and A.Status (S, Keys (J)) = A.Live); end loop;
      end loop;
   end loop;
   for I in A.Slot loop
      A.Begin_Release (S, Keys (I), True);
      A.Released (S, Keys (I), Natural (I) mod 17 /= 0);
      if Natural (I) mod 17 /= 0 then Charged := Charged - Natural (I);
      else pragma Assert (A.Current (S, Keys (I)) and A.Status (S, Keys (I)) = A.Quarantined);
      end if;
      pragma Assert (A.Valid (S) and A.Charged (S) = Charged);
   end loop;
   pragma Assert (Charged = 612);
   A.Reserve (S, Total, Extra); pragma Assert (Extra = A.No_Ticket and A.Charged (S) = 612);
   Ada.Text_IO.Put_Line ("PASS configurable ledger: 140 simultaneous allocations, 1120 replacements, stale tickets rejected, exact shared budget, eight quarantines remain charged");
end Storage_Capacity_Tests;

with Ada.Text_IO;
with Compositor_Lease_Request;
procedure Lease_Request_Tests is
   package L renames Compositor_Lease_Request;
   use type L.Phase, L.ID, L.State;
   S, Before : L.State;
   Accepted : Boolean;
begin
   for I in L.ID range 1 .. 10_000 loop
      L.Prepare (S, I, Accepted);
      pragma Assert (Accepted and L.Status (S) = L.Submitting);
      L.Submitted (S, False);
      pragma Assert (L.Status (S) = L.Ready and L.Token (S) = I);
   end loop;
   Before := S;
   L.Prepare (S, 10_000, Accepted);
   pragma Assert (not Accepted and S = Before);
   L.Prepare (S, L.ID'Last, Accepted);
   pragma Assert (not Accepted and S = Before);
   L.Prepare (S, 10_001, Accepted); pragma Assert (Accepted);
   L.Submitted (S, True);
   Before := S;
   for I in 1 .. 10_000 loop
      L.Prepare (S, 10_002, Accepted);
      pragma Assert (not Accepted and S = Before and L.Status (S) = L.Pending);
   end loop;
   L.Complete (S, 10_001, True);
   pragma Assert (L.Status (S) = L.Released);
   L.Complete (S, 10_001, True);
   pragma Assert (L.Status (S) = L.Quarantined);
   for Match in Boolean loop
      for Confirmed in Boolean loop
         declare T : L.State; begin
            L.Prepare (T, 50, Accepted); pragma Assert (Accepted);
            L.Submitted (T, True);
            L.Complete (T, (if Match then 50 else 51), Confirmed);
            pragma Assert (L.Status (T) = (if Match and Confirmed then L.Released else L.Quarantined));
         end;
      end loop;
   end loop;
   declare T : L.State; begin
      L.Complete (T, 0, True); pragma Assert (L.Status (T) = L.Quarantined);
      Before := T; L.Prepare (T, 1, Accepted); pragma Assert (not Accepted and T = Before);
   end;
   Ada.Text_IO.Put_Line ("LEASE-REQUEST: PASS 10000 rejected submissions, 10000 pending admission attempts, exact completion and quarantine cases");
end Lease_Request_Tests;

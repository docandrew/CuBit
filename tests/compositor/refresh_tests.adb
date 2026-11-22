with Ada.Text_IO; use Ada.Text_IO;
with Refresh_Proof;
procedure Refresh_Tests is
   package R renames Refresh_Proof;
   use type R.Phase;
   S : R.State;
   Published : Boolean;
begin
   for Cycle in 1 .. 300 loop
      for N in R.Item_Count loop
         R.Request (S);
         R.Start (S);
         pragma Assert (R.Current (S) = R.Listing and not R.Requested (S));
         R.Publish (S, False, Published);
         pragma Assert (not Published);
         R.Listed (S, N);
         for I in 1 .. N loop
            pragma Assert (R.Index (S) = I and R.Count (S) = N);
            for Repeat in 1 .. 20 loop R.Request (S); end loop;
            R.Publish (S, False, Published);
            pragma Assert (not Published);
            R.Read_Item (S);
         end loop;
         pragma Assert (R.Current (S) = R.Ready);
         for Hold in 1 .. 10 loop
            R.Publish (S, True, Published);
            pragma Assert (not Published and R.Current (S) = R.Ready);
         end loop;
         R.Publish (S, False, Published);
         pragma Assert (Published and R.Current (S) = R.Idle);
         pragma Assert (R.Requested (S) = (N > 0));
         R.Publish (S, False, Published);
         pragma Assert (not Published);
      end loop;
   end loop;
   Put_Line ("refresh: PASS4500 complete snapshots, visible holds, coalesced requests");
end Refresh_Tests;

with Region_Release;
with Region_Registry;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure Release_Tests is
   Cases : Natural := 0;
   procedure Run (Pages : Positive; Failure : Natural; Raise_Error : Boolean) is
      package Registry is new Region_Registry (1, 16#100000#, 16#200000#);
      use type Registry.Phase;
      Entries : Registry.Registry;
      Key, Next_Key : Registry.Handle;
      Accepted : Boolean;
      Bytes : constant Unsigned_64 := Unsigned_64 (Pages) * 4096;
      Present : array (0 .. Pages - 1) of Boolean := [others => True];
      Calls, Frees : Natural := 0;
      Synced : Boolean := False;
      Injected : exception;
      procedure Step (OK : out Boolean) is
      begin
         pragma Assert (Registry.State (Entries, 1, Key) = Registry.Retiring);
         Calls := Calls + 1;
         if Calls = Failure and Raise_Error then raise Injected; end if;
         OK := Calls /= Failure;
      end Step;
      procedure Unmap (Index : Natural; OK : out Boolean) is
      begin
         pragma Assert (not Synced and Frees = 0);
         Step (OK);
         if OK then Present (Index) := False; end if;
      end Unmap;
      procedure Synchronize (OK : out Boolean) is
      begin
         pragma Assert ((for all P of Present => not P) and Frees = 0);
         Step (OK);
         Synced := OK;
      end Synchronize;
      procedure Free (OK : out Boolean) is
      begin
         pragma Assert (Synced and (for all P of Present => not P));
         Step (OK);
         if OK then Frees := Frees + 1; end if;
      end Free;
      package R is new Region_Release (Unmap, Synchronize, Free);
      use type R.Phase;
      Object : R.Attempt;
   begin
      Registry.Reserve (Entries, 1, 16#100000#, Bytes, Key, Accepted);
      pragma Assert (Accepted);
      Registry.Bind_Backing (Entries, 1, Key, 16#300000#, Accepted);
      pragma Assert (Accepted);
      Registry.Commit (Entries, 1, Key, Accepted);
      pragma Assert (Accepted);
      R.Apply (Object, Pages, False);
      pragma Assert (R.State (Object) = R.Fresh and Calls = 0);
      pragma Assert (Registry.State (Entries, 1, Key) = Registry.Live);
      Registry.Begin_Retirement (Entries, 1, Key, Accepted);
      pragma Assert (Accepted);
      begin
         R.Apply (Object, Pages, True);
      exception
         when Injected => pragma Assert (Raise_Error and Failure /= 0);
      end;
      if Failure = 0 then
         pragma Assert (R.State (Object) = R.Released and Frees = 1);
         Registry.Finish_Retirement (Entries, 1, Key, Accepted);
         pragma Assert (Accepted);
         pragma Assert (Registry.State (Entries, 1, Key) = Registry.Absent);
         Registry.Reserve (Entries, 1, 16#100000#, Bytes, Next_Key, Accepted);
         pragma Assert (Accepted and Next_Key.Generation /= Key.Generation);
      else
         pragma Assert (R.State (Object) = R.Quarantined and Frees = 0);
         pragma Assert (Registry.Overlaps (Entries, 1, 16#100000#, Bytes));
         pragma Assert (Registry.Physical_Overlap (Entries, 16#300000#, Bytes));
         Registry.Reserve (Entries, 1, 16#100000#, Bytes, Next_Key, Accepted);
         pragma Assert (not Accepted);
      end if;
      declare
         Before : constant Natural := Calls;
      begin
         R.Apply (Object, Pages, True);
         R.Apply (Object, Pages, False);
         pragma Assert (Calls = Before);
      end;
      Cases := Cases + 1;
   end Run;
begin
   for Pages in 1 .. 16 loop
      Run (Pages, 0, False);
      for Failure in 1 .. Pages + 2 loop
         Run (Pages, Failure, False);
         Run (Pages, Failure, True);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("region release PASS cases=" & Natural'Image (Cases));
end Release_Tests;

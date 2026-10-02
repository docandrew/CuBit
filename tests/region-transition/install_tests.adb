with Region_Install;
with Ada.Text_IO;
procedure Install_Tests is
   procedure Run (Count : Positive; Fail_Map : Natural; Failure : Natural) is
      Mappings : array (0 .. 7) of Boolean := [others => False];
      Calls, Unmapped : Natural := 0;
      Synced, Freed : Boolean := False;
      Raised : exception;
      procedure Map_Page (Index : Natural; OK : out Boolean) is
      begin
         Calls := Calls + 1;
         OK := Index + 1 /= Fail_Map;
         if OK then Mappings (Index) := True; end if;
      end Map_Page;
      procedure Unmap_Page (Index : Natural; OK : out Boolean) is
      begin
         Calls := Calls + 1;
         Unmapped := Unmapped + 1;
         if Failure = 4 then raise Raised; end if;
         OK := Failure /= 1;
         if OK then Mappings (Index) := False; end if;
      end Unmap_Page;
      procedure Synchronize (OK : out Boolean) is
      begin
         Calls := Calls + 1;
         pragma Assert ((for all Present of Mappings => not Present));
         OK := Failure /= 2;
         Synced := OK;
      end Synchronize;
      procedure Release_Extent (OK : out Boolean) is
      begin
         Calls := Calls + 1;
         pragma Assert (Synced);
         OK := Failure /= 3;
         Freed := OK;
      end Release_Extent;
      package R is new Region_Install (Map_Page, Unmap_Page, Synchronize, Release_Extent);
      use type R.Phase;
      Object : R.Attempt;
   begin
      begin
         R.Apply (Object, Count);
      exception
         when Raised => pragma Assert (Failure = 4 and Fail_Map > 1);
      end;
      if Fail_Map = 0 then
         pragma Assert (R.State (Object) = R.Mapped and not Freed and not Synced);
      elsif Failure = 0 or (Fail_Map = 1 and (Failure = 1 or Failure = 4)) then
         pragma Assert (R.State (Object) = R.Released and Freed);
      else
         pragma Assert (R.State (Object) = R.Quarantined and not Freed);
      end if;
      if Failure /= 4 and Fail_Map > 0 then
         pragma Assert (Unmapped = Fail_Map - 1);
      end if;
      declare
         Before : constant Natural := Calls;
      begin
         R.Apply (Object, Count);
         pragma Assert (Calls = Before);
      end;
   end Run;
begin
   for Count in 1 .. 8 loop
      Run (Count, 0, 0);
      for Failure_Page in 1 .. Count loop
         for Failure in 0 .. 4 loop
            Run (Count, Failure_Page, Failure);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS: 188 extent installation/rollback/quarantine cases");
end Install_Tests;

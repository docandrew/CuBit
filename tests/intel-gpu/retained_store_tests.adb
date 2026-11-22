with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Initialize;
with Intel_GPU_Retained_Store;
procedure Retained_Store_Tests is
   type RAM is array (Natural range <>) of Unsigned_64;
   Index_RAM : RAM (0 .. 16383) with Alignment => 4096;
   Item_RAM : RAM (0 .. 2047) with Alignment => 4096;
   Index_Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Index_RAM'Address));
   Item_Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Item_RAM'Address));
begin
   for Mode in 0 .. 11 loop
      declare
         Owner : Boolean := True;
         Reserves, Commits, Clears : Natural := 0;
         Index_Bytes, Item_Bytes : Unsigned_64 := 0;
         type Budget is limited record
            Used : Unsigned_64 := 0;
            Limit : Unsigned_64 := 131072 + 16384;
            Charges : Natural := 0;
         end record;
         Shared : aliased Budget;
         Alternate : aliased Budget;
         Budget_Used : Unsigned_64 renames Shared.Used;
         Budget_Limit : Unsigned_64 renames Shared.Limit;
         Charges : Natural renames Shared.Charges;
         function Ready return Boolean is (Owner);
         function Charge (Account : in out Budget; Bytes : Unsigned_64) return Boolean is
         begin
            Account.Charges := Account.Charges + 1;
            if Mode = 8 or else Bytes > Account.Limit - Account.Used then return False; end if;
            Account.Used := Account.Used + Bytes;
            if Mode = 9 then Owner := False; end if;
            return True;
         end Charge;
         function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
         begin
            Reserves := Reserves + 1;
            if Mode = 1 then Owner := False; end if;
            if Mode = 5 then return 0; end if;
            if Bytes = 131072 then return Index_Base; end if;
            if Bytes = 4096 then return 16#80000000#; end if;
            pragma Assert (Bytes = 16384);
            return (if Mode = 4 then Index_Base else Item_Base);
         end Reserve;
         function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
         begin
            Commits := Commits + 1;
            if Mode = 6 then Owner := False; end if;
            pragma Assert (Bytes in 4096 .. 65536 and Bytes mod 4096 = 0);
            if Mode = 2 then return False; end if;
            if Base = Index_Base then
               pragma Assert (Offset = Index_Bytes and Offset + Bytes <= 131072);
               Index_Bytes := Offset + Bytes;
            else
               pragma Assert (Base = Item_Base and Offset = Item_Bytes and Offset + Bytes <= 16384);
               Item_Bytes := Offset + Bytes;
            end if;
            return True;
         end Commit;
         function Clear (Base, Bytes : Unsigned_64) return Boolean is
         begin
            Clears := Clears + 1;
            if Mode = 7 then Owner := False; end if;
            if Mode = 3 then return False; end if;
            return Intel_GPU_Metadata_Initialize.Clear (Base, Bytes);
         end Clear;
         package M is new Intel_GPU_Metadata_Arena (Reserve, Commit, Clear);
         type Item is limited record
            Value : Unsigned_64 := 42;
            Arena : M.Arena;
         end record;
         use type M.State;
         package R is new Intel_GPU_Retained_Store (Item, M, Ready, Budget, Charge);
         use type R.Phase, R.Element_Access;
         Object : R.Store;
         OK : Boolean;
         Saved : R.Element_Access;
         Before : Natural;
         procedure Finish is
            Step_Before : Natural;
         begin
            for Turn in 1 .. 200 loop
               Step_Before := Reserves + Commits + Charges;
               if Mode = 10 or else (Mode = 11 and R.State (Object) = R.Committing_Index) then
                  R.Step (Object, Alternate'Access);
               else
                  R.Step (Object, Shared'Access);
               end if;
               pragma Assert (Reserves + Commits + Charges <= Step_Before + 1);
               exit when R.State (Object) in R.Idle | R.Failed;
            end loop;
            pragma Assert (R.State (Object) in R.Idle | R.Failed);
         end Finish;
      begin
         pragma Assert (R.Element_Bytes = 4096);
         R.Request (Object, Shared'Access, 20000, 131072, 16384, OK);
         pragma Assert (not OK and Reserves = 0 and R.State (Object) = R.Idle);
         R.Request (Object, Shared'Access, 17, 131072, 0, OK);
         pragma Assert (not OK and Reserves = 0 and R.State (Object) = R.Idle);
         R.Request (Object, Shared'Access, 17, 131072, 16384, OK); pragma Assert (OK);
         R.Request (Object, Shared'Access, 18, 131072, 16384, OK); pragma Assert (not OK);
         Finish;
         pragma Assert (R.Charged_Bytes (Object) = Budget_Used);
         pragma Assert (Alternate.Used = 0 and Alternate.Charges = 0);
         if Mode /= 0 then
            pragma Assert (R.State (Object) = R.Failed and R.Lookup (Object, 17) = null);
            if Mode in 2 | 3 | 6 | 7 | 9 then pragma Assert (Budget_Used = 4096); end if;
            if Mode in 8 | 9 then pragma Assert (Commits = 0 and Clears = 0); end if;
            if Mode = 10 then pragma Assert (Reserves = 0 and Charges = 0); end if;
            if Mode = 11 then pragma Assert (Budget_Used = 4096 and Commits = 0); end if;
            Before := Reserves + Commits + Clears + Charges;
            Owner := True;
            for Turn in 1 .. 10 loop R.Step (Object, Shared'Access); end loop;
            R.Request (Object, Shared'Access, 17, 131072, 16384, OK);
            pragma Assert (not OK and Reserves + Commits + Clears + Charges = Before);
         else
            pragma Assert (R.State (Object) = R.Idle and R.Lookup (Object, 17).Value = 42);
            Saved := R.Lookup (Object, 17); Saved.Value := 99;
            R.Request (Object, Alternate'Access, 18, 131072, 16384, OK);
            pragma Assert (not OK and R.State (Object) = R.Idle);
            R.Request (Object, Shared'Access, 18, 262144, 16384, OK);
            pragma Assert (not OK and R.State (Object) = R.Idle);
            pragma Assert (M.Snapshot (Saved.Arena).Phase = M.Empty);
            M.Open (Saved.Arena, 4096, OK); pragma Assert (OK);
            R.Request (Object, Shared'Access, 9000, 131072, 16384, OK); pragma Assert (OK); Finish;
            pragma Assert (R.State (Object) = R.Idle and R.Lookup (Object, 9000).Value = 42);
            pragma Assert (R.Lookup (Object, 9000) /= Saved and R.Lookup (Object, 18) = null);
            R.Request (Object, Shared'Access, 18, 131072, 16384, OK); pragma Assert (OK); Finish;
            pragma Assert (R.Lookup (Object, 18).Value = 42 and R.Lookup (Object, 17) = Saved);
            pragma Assert (Saved.Value = 99 and Item_Bytes = 3 * 4096 and Reserves = 3);
            pragma Assert (M.Snapshot (Saved.Arena).Phase = M.Reserved and
                           M.Snapshot (Saved.Arena).Base = 16#80000000#);
            Before := Commits;
            R.Request (Object, Shared'Access, 17, 131072, 16384, OK); pragma Assert (OK); Finish;
            pragma Assert (Commits = Before and R.Lookup (Object, 17) = Saved);
            R.Request (Object, Shared'Access, 1, 131072, 16384, OK); pragma Assert (OK); Finish;
            pragma Assert (R.Lookup (Object, 1).Value = 42 and Item_Bytes = 16384);
            R.Request (Object, Shared'Access, 2, 131072, 16384, OK); pragma Assert (OK); Finish;
            pragma Assert (R.State (Object) = R.Failed and R.Lookup (Object, 2) = null);
            pragma Assert (Saved.Value = 99 and R.Lookup (Object, 17) = Saved);
            pragma Assert (R.Charged_Bytes (Object) = Index_Bytes + Item_Bytes);
            -- A different store uses the same backing budget, not a fresh
            -- allowance equal to its independent virtual reservation sizes.
            Budget_Limit := Budget_Used;
            declare
               Other : R.Store;
               Committed_Before : constant Natural := Commits;
            begin
               R.Request (Other, Shared'Access, 1, 131072, 16384, OK); pragma Assert (OK);
               for Turn in 1 .. 20 loop
                  R.Step (Other, Shared'Access);
                  exit when R.State (Other) = R.Failed;
               end loop;
               pragma Assert (R.State (Other) = R.Failed and Commits = Committed_Before);
               pragma Assert (R.Charged_Bytes (Other) = 0 and Saved.Value = 99);
            end;
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Retained store PASS: sparse growth, typed defaults, stable limited records, switching, bounded work, quota, overlap/owner/commit/clear failure and no replay");
end Retained_Store_Tests;

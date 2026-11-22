with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Metadata_Ranges;
with Intel_GPU_Metadata_Ranges.Testing;
procedure Metadata_Ranges_Tests is
   package R renames Intel_GPU_Metadata_Ranges;
   N : constant := 8192;
   function Base (I : Natural) return Unsigned_64 is (4096 + Unsigned_64 (I) * 8192);
begin
   for Order in 0 .. 2 loop
      declare
         Items : array (0 .. N) of aliased R.Node;
         Object, Other : R.Tree;
         OK, Hit, Expected : Boolean;
         Visits, Index : Natural;
         Address, Bytes : Unsigned_64;
      begin
         for I in 0 .. N - 1 loop
            Index := (case Order is when 0 => I, when 1 => N - 1 - I,
                      when others => (I * 4051) mod N);
            R.Insert (Object, Items (Index)'Unchecked_Access, Base (Index), 4096, OK, Visits);
            pragma Assert (OK and Visits <= 128 and R.Count (Object) = I + 1);
            if I < 128 or else I mod 127 = 0 or else I = N - 1 then
               pragma Assert (R.Testing.Valid (Object));
            end if;
         end loop;
         pragma Assert (R.Height (Object) <= 28);
         for Query in 0 .. 511 loop
            Address := 1 + Unsigned_64 ((Query * 131071) mod (N * 8192 + 16384));
            Bytes := 1 + Unsigned_64 ((Query * 73) mod 12288);
            Expected := False;
            for I in 0 .. N - 1 loop
               if Address < Base (I) + 4096 and then Base (I) < Address + Bytes then
                  Expected := True; exit;
               end if;
            end loop;
            R.Conflict (Object, Address, Bytes, Hit, Visits);
            pragma Assert (Hit = Expected and Visits <= 64);
         end loop;
         for I in 0 .. N - 1 loop
            R.Conflict (Object, Base (I), 4096, Hit, Visits); pragma Assert (Hit);
            R.Conflict (Object, Base (I) + 4096, 4096, Hit, Visits); pragma Assert (not Hit);
         end loop;
         R.Insert (Other, Items (0)'Unchecked_Access, Base (N), 1, OK, Visits);
         pragma Assert (not OK and R.Count (Other) = 0);
         R.Insert (Object, null, Base (N), 1, OK, Visits); pragma Assert (not OK);
         R.Insert (Object, Items (N)'Unchecked_Access, 0, 1, OK, Visits); pragma Assert (not OK);
         R.Insert (Object, Items (N)'Unchecked_Access, Base (N), 0, OK, Visits); pragma Assert (not OK);
         R.Insert (Object, Items (N)'Unchecked_Access, Unsigned_64'Last - 1, 2, OK, Visits);
         pragma Assert (not OK);
         R.Conflict (Object, Unsigned_64'Last - 1, 2, Hit, Visits); pragma Assert (Hit);
         R.Insert (Object, Items (N)'Unchecked_Access, Base (0) + 4095, 2, OK, Visits);
         pragma Assert (not OK and R.Count (Object) = N and R.Testing.Valid (Object));
         -- Rejections do not consume the caller's unlinked node.
         R.Insert (Object, Items (N)'Unchecked_Access, Base (N - 1) + 4096, 4096, OK, Visits);
         pragma Assert (OK and R.Count (Object) = N + 1 and R.Testing.Valid (Object));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Metadata ranges PASS:8192 ascending/descending/permuted inserts, AVL/parent/order invariants, linear overlap oracle, bounded visits, adjacency/rejection/node ownership");
end Metadata_Ranges_Tests;

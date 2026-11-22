with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Client_Budgets;
procedure Client_Budgets_Tests is
   package B renames Intel_GPU_Client_Budgets;
   Object, Huge : B.Ledger;
   type RAM is array (Natural range 0 .. 65535) of Unsigned_8;
   Metadata : RAM := [others => 16#A5#] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
   Metadata_Bytes : Unsigned_64 := 0;
   type Model is array (Positive range 1 .. 768) of Unsigned_64;
   Used : Model := [others => 0];
   function Key (Index : Positive) return Unsigned_64 is
     (if Index mod 2 = 0 then Unsigned_64 (Index) else Unsigned_64'Last - Unsigned_64 (Index));
   OK : Boolean;
begin
   B.Open (Object, 0, 65536, OK); pragma Assert (not OK);
   B.Open (Object, 1, 1, OK); pragma Assert (not OK);
   B.Open (Object, 1, 0, OK); pragma Assert (not OK);
   for Index in Used'Range loop
      if Index > B.Capacity (Object) then
         B.Open (Object, Key (Index), 65536, OK); pragma Assert (not OK);
         Metadata_Bytes := Metadata_Bytes + 4096;
         B.Extend (Object, Base, Metadata_Bytes, OK); pragma Assert (OK);
         for Prior in 1 .. Index - 1 loop
            pragma Assert (B.Snapshot (Object, Key (Prior)).Known);
         end loop;
      end if;
      B.Open (Object, Key (Index), 65536, OK); pragma Assert (OK);
      pragma Assert (B.Last_Probes (Object) <= B.Max_Probes);
      B.Open (Object, Key (Index), 4096, OK); pragma Assert (not OK);
   end loop;
   for Round in 1 .. 20 loop
      for Index in Used'Range loop
         declare
            Bytes : constant Unsigned_64 := Unsigned_64 ((Index + Round) mod 5 + 1) * 4096;
            Expected : constant Boolean := Bytes <= 65536 - Used (Index);
         begin
            B.Reserve (Object, Key (Index), Bytes, OK); pragma Assert (OK = Expected);
            if Expected then Used (Index) := Used (Index) + Bytes; end if;
            pragma Assert (B.Last_Probes (Object) <= B.Max_Probes);
            pragma Assert (B.Snapshot (Object, Key (Index)).Charged = Used (Index));
         end;
      end loop;
   end loop;
   for Index in Used'Range loop
      B.Close (Object, Key (Index));
      B.Reserve (Object, Key (Index), 4096, OK); pragma Assert (not OK);
      B.Open (Object, Key (Index), 65536, OK); pragma Assert (not OK);
      B.Release_Confirmed (Object, Key (Index), Used (Index), False, OK);
      pragma Assert (not OK and B.Snapshot (Object, Key (Index)).Charged = Used (Index));
      B.Release_Confirmed (Object, Key (Index), Used (Index) + 4096, True, OK);
      pragma Assert (not OK);
      B.Release_Confirmed (Object, Key (Index), Used (Index), True, OK);
      pragma Assert (OK and B.Snapshot (Object, Key (Index)).Charged = 0);
      B.Release_Confirmed (Object, Key (Index), Used (Index), True, OK);
      pragma Assert (not OK); -- whole-account duplicate, not ticket validation
   end loop;
   for I in Natural (Metadata_Bytes) .. Metadata'Last loop
      pragma Assert (Metadata (I) = 16#A5#);
   end loop;
   B.Open (Huge, 1, Unsigned_64'Last - 4095, OK); pragma Assert (OK);
   B.Reserve (Huge, 1, Unsigned_64'Last - 4095, OK); pragma Assert (OK);
   B.Reserve (Huge, 1, 4096, OK); pragma Assert (not OK);
   B.Quarantine (Huge);
   B.Release_Confirmed (Huge, 1, 4096, True, OK); pragma Assert (not OK);
   pragma Assert (B.Snapshot (Huge, 1).Charged = Unsigned_64'Last - 4095);
   B.Extend (Huge, Base, 4096, OK); pragma Assert (not OK);
   Ada.Text_IO.Put_Line ("Client budgets PASS:768 stable sessions,15360 admission checks, bounded trie, metadata growth, close/drain, quarantine and U64 boundary (trusted accounting only)");
end Client_Budgets_Tests;
